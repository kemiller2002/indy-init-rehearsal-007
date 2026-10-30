/// A stateful GitHub fake for the PAT tests (PERSIST-025): it enforces expectedHeadOid, keeps real
/// commit ancestry, accepts commits from simulated other writers, and can lose an answer before or
/// after applying a mutation. Immutable state; `handle` returns the next state with the answer.
module Causa.Fake.FakeGitHub

open System
open System.Text
open Causa.Domain.Json

type Commit =
    { Sha: string
      Parent: string option
      Tree: Map<string, string>
      Message: string }

type Loss =
    | LoseAfterApply
    | LoseBeforeApply

type CommitFault =
    | Unauthorized
    | Forbidden
    | RateLimited
    | Invalid

type Behaviour =
    { /// The next mutations lose their answers in this order.
      LoseNext: Loss list
      /// Every request fails as if the network were down.
      NetworkDown: bool
      /// Only reads fail as if the network were down.
      ReadsDown: bool
      /// Another writer commits an unrelated file before every mutation is applied.
      AdvanceBeforeEveryMutation: bool
      /// The next mutation is refused with this fault.
      NextCommitFault: CommitFault option
      /// The next applied mutation stores different bytes than it was sent.
      CorruptNextApply: bool
      /// The next compare answers are computed against the head before the latest commit (a lagging view).
      LaggingCompares: int }

type State =
    { Owner: string
      Name: string
      Branch: string
      Token: string
      ReadOnlyTokens: string list
      Commits: Map<string, Commit>
      Head: string
      Behaviour: Behaviour
      Counter: int
      Log: (string * string) list }

type Answer =
    | Respond of status: int * body: Json
    | NetworkFailure
    | AnswerLost

let calm = { LoseNext = []; NetworkDown = false; ReadsDown = false; AdvanceBeforeEveryMutation = false; NextCommitFault = None; CorruptNextApply = false; LaggingCompares = 0 }

let private sha (seed: string) =
    Security.Cryptography.SHA1.HashData(Encoding.UTF8.GetBytes seed) |> Convert.ToHexString |> fun s -> s.ToLowerInvariant()

let create (owner: string) (name: string) (token: string) (files: (string * string) list) : State =
    let root = { Sha = sha "root"; Parent = None; Tree = Map.ofList (("README.md", "Causa test data\n") :: files); Message = "Initial commit" }
    { Owner = owner; Name = name; Branch = "main"; Token = token; ReadOnlyTokens = []; Commits = Map.ofList [ root.Sha, root ]
      Head = root.Sha; Behaviour = calm; Counter = 0; Log = [] }

let headCommit (s: State) = s.Commits.[s.Head]

/// A commit by another writer, straight onto the branch.
let commitAs (s: State) (message: string) (changes: (string * string) list) : State =
    let parent = headCommit s
    let tree = changes |> List.fold (fun t (p, c) -> Map.add p c t) parent.Tree
    let c = { Sha = sha (sprintf "%s-%d-%s" parent.Sha (s.Counter + 1) message); Parent = Some parent.Sha; Tree = tree; Message = message }
    { s with Commits = s.Commits.Add(c.Sha, c); Head = c.Sha; Counter = s.Counter + 1 }

/// Replaces the branch with a new root history (a force-push by someone else).
let rewriteHistory (s: State) : State =
    let tree = (headCommit s).Tree
    let c = { Sha = sha (sprintf "rewrite-%d" (s.Counter + 1)); Parent = None; Tree = tree; Message = "Rewritten history" }
    { s with Commits = s.Commits.Add(c.Sha, c); Head = c.Sha; Counter = s.Counter + 1 }

let rec ancestors (s: State) (from: string) : string list =
    match s.Commits.TryFind from with
    | Some c -> c.Sha :: (c.Parent |> Option.map (ancestors s) |> Option.defaultValue [])
    | None -> []

let reachable (s: State) (commitSha: string) = ancestors s s.Head |> List.contains commitSha

let mutationCount (s: State) = s.Log |> List.filter (fst >> (=) "mutation") |> List.length

let readCount (kind: string) (s: State) = s.Log |> List.filter (fst >> (=) kind) |> List.length

let private notFound = Respond(404, JObject [ "message", JString "Not Found" ])

let private fileJson (content: string) =
    JObject [ "type", JString "file"; "encoding", JString "base64"; "content", JString(Convert.ToBase64String(utf8 content)) ]

let private compareJson (s: State) (baseSha: string) (headSha: string) =
    if not (s.Commits.ContainsKey baseSha) || not (s.Commits.ContainsKey headSha) then notFound
    else
        let fromHead = ancestors s headSha
        let commitsJson shas =
            JArray(shas |> List.map (fun c -> JObject [ "sha", JString c; "commit", JObject [ "message", JString s.Commits.[c].Message ] ]))
        if baseSha = headSha then Respond(200, JObject [ "status", JString "identical"; "commits", JArray [] ])
        elif List.contains baseSha fromHead then
            let after = fromHead |> List.takeWhile ((<>) baseSha) |> List.rev
            Respond(200, JObject [ "status", JString "ahead"; "commits", commitsJson after ])
        elif ancestors s baseSha |> List.contains headSha then Respond(200, JObject [ "status", JString "behind"; "commits", JArray [] ])
        else Respond(200, JObject [ "status", JString "diverged"; "commits", JArray [] ])

let private stale (expected: string) =
    Respond(
        200,
        JObject [
            "data", JObject [ "createCommitOnBranch", JNull ]
            "errors", JArray [ JObject [ "type", JString "STALE_DATA"; "message", JString(sprintf "Expected branch to point to \"%s\" but it did not. Pull and try again." expected) ] ]
        ])

let private graphError (kind: string) (message: string) =
    Respond(200, JObject [ "data", JObject [ "createCommitOnBranch", JNull ]; "errors", JArray [ JObject [ "type", JString kind; "message", JString message ] ] ])

let private applyCommit (s: State) (input: Json) : Result<State * string, Answer> =
    let text name j = match field name j with Some(JString v) -> v | _ -> ""
    let expected = text "expectedHeadOid" input
    let branch = field "branch" input |> Option.map (text "branchName") |> Option.defaultValue ""
    let message = field "message" input
    let headline = message |> Option.map (text "headline") |> Option.defaultValue ""
    let body = message |> Option.map (text "body") |> Option.defaultValue ""
    let additions =
        match field "fileChanges" input |> Option.bind (field "additions") with
        | Some(JArray adds) -> adds |> List.map (fun a -> text "path" a, Encoding.UTF8.GetString(Convert.FromBase64String(text "contents" a)))
        | _ -> []
    if branch <> s.Branch then Error(graphError "NOT_FOUND" "Branch not found")
    elif expected <> s.Head then Error(stale expected)
    else
        let stored = if s.Behaviour.CorruptNextApply then additions |> List.map (fun (p, c) -> p, c + " ") else additions
        let s' = commitAs { s with Behaviour = { s.Behaviour with CorruptNextApply = false } } (headline + "\n\n" + body) stored
        Ok(s', s'.Head)

let private mutation (s: State) (token: string) (body: string) : State * Answer =
    let s = { s with Log = s.Log @ [ "mutation", "" ] }
    let s = if s.Behaviour.AdvanceBeforeEveryMutation then commitAs s "Unrelated change by another writer" [ sprintf "notes/other-%d.md" (s.Counter + 1), "other\n" ] else s
    match parse body |> Result.toOption |> Option.bind (field "variables") |> Option.bind (field "input") with
    | None -> s, Respond(400, JObject [ "message", JString "Problems parsing JSON" ])
    | Some input ->
        match s.Behaviour.NextCommitFault, s.Behaviour.LoseNext with
        | Some fault, _ ->
            let s' = { s with Behaviour = { s.Behaviour with NextCommitFault = None } }
            match fault with
            | Unauthorized -> s', Respond(401, JObject [ "message", JString "Bad credentials" ])
            | Forbidden -> s', graphError "FORBIDDEN" "Resource not accessible by personal access token"
            | RateLimited -> s', Respond(403, JObject [ "message", JString "API rate limit exceeded for user." ])
            | Invalid -> s', graphError "UNPROCESSABLE" "A path is not valid"
        | None, LoseBeforeApply :: rest -> { s with Behaviour = { s.Behaviour with LoseNext = rest } }, AnswerLost
        | None, loss ->
            if List.contains token s.ReadOnlyTokens then s, graphError "FORBIDDEN" "Resource not accessible by personal access token"
            else
                match applyCommit s input, loss with
                | Ok(s', _), LoseAfterApply :: rest -> { s' with Behaviour = { s'.Behaviour with LoseNext = rest } }, AnswerLost
                | Ok(s', oid), _ ->
                    s', Respond(200, JObject [ "data", JObject [ "createCommitOnBranch", JObject [ "commit", JObject [ "oid", JString oid ] ] ] ])
                | Error answer, _ -> s, answer

/// Serves one request. `path` excludes the API base and includes the query string.
let handle (s: State) (httpMethod: string) (path: string) (authorization: string) (body: string) : State * Answer =
    let token = if authorization.StartsWith "Bearer " then authorization.Substring 7 else ""
    let route, query =
        match path.IndexOf '?' with
        | -1 -> path, ""
        | i -> path.Substring(0, i), path.Substring(i + 1)
    let param name =
        query.Split('&') |> Array.tryPick (fun kv -> match kv.Split('=', 2) with [| k; v |] when k = name -> Some(Uri.UnescapeDataString v) | _ -> None)
    let repoPrefix = sprintf "/repos/%s/%s" s.Owner s.Name
    let isRead = httpMethod = "GET"
    if s.Behaviour.NetworkDown || (isRead && s.Behaviour.ReadsDown) then { s with Log = s.Log @ [ "dropped", path ] }, NetworkFailure
    elif token <> s.Token && not (List.contains token s.ReadOnlyTokens) then s, Respond(401, JObject [ "message", JString "Bad credentials" ])
    elif httpMethod = "POST" && route = "/graphql" then mutation s token body
    elif not isRead then s, notFound
    else
        let log kind = { s with Log = s.Log @ [ kind, path ] }
        let rest = if route.StartsWith repoPrefix then Some(route.Substring repoPrefix.Length) else None
        match rest with
        | Some "" ->
            log "repository", Respond(200, JObject [ "default_branch", JString s.Branch; "permissions", JObject [ "push", JBool(token = s.Token); "pull", JBool true ] ])
        | Some r when r = "/git/ref/heads/" + s.Branch -> log "head", Respond(200, JObject [ "object", JObject [ "sha", JString s.Head ] ])
        | Some r when r.StartsWith "/git/ref/heads/" -> log "head", notFound
        | Some r when r.StartsWith "/git/trees/" ->
            match s.Commits.TryFind(r.Substring "/git/trees/".Length) with
            | Some c ->
                let entries = c.Tree |> Map.toList |> List.map (fun (p, _) -> JObject [ "path", JString p; "type", JString "blob" ])
                log "tree", Respond(200, JObject [ "tree", JArray entries; "truncated", JBool false ])
            | None -> log "tree", notFound
        | Some r when r.StartsWith "/contents/" ->
            let filePath = r.Substring "/contents/".Length |> fun p -> p.Split('/') |> Array.map Uri.UnescapeDataString |> String.concat "/"
            let at = param "ref" |> Option.defaultValue s.Head
            match s.Commits.TryFind at |> Option.bind (fun c -> c.Tree.TryFind filePath) with
            | Some content -> log "file", Respond(200, fileJson content)
            | None -> log "file", notFound
        | Some r when r.StartsWith "/compare/" ->
            let spec = r.Substring "/compare/".Length
            match spec.Split("...") with
            | [| b; h |] ->
                let current = if Uri.UnescapeDataString h = s.Branch then s.Head else h
                let lagging = s.Behaviour.LaggingCompares > 0 && Uri.UnescapeDataString h = s.Branch
                let headSha = if lagging then defaultArg (headCommit s).Parent s.Head else current
                let s' = if lagging then { s with Behaviour = { s.Behaviour with LaggingCompares = s.Behaviour.LaggingCompares - 1 } } else s
                let logged = { s' with Log = s'.Log @ [ "compare", path ] }
                logged, compareJson s b headSha
            | _ -> log "compare", notFound
        | _ -> s, notFound
