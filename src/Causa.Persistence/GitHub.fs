/// GitHub requests Causa sends and the typed classification of every answer.
/// Pure: requests are data executed by the Limen kernel (or a test interpreter).
module Causa.Persistence.GitHub

open System
open Causa.Domain.Json

type Connection =
    { ApiBase: string
      Owner: string
      Name: string
      Branch: string
      Workspace: string
      Token: string }

type Representation =
    | AsJson
    | AsText

type Request =
    { Method: string
      Url: string
      Headers: (string * string) list
      Body: string option
      Response: Representation
      TimeoutMs: int }

/// What came back, as the kernel reports it.
type Reply =
    | Answered of status: int * body: Json
    | Unsent
    | Lost
    | Undecodable of status: int option

let private segment (s: string) = Uri.EscapeDataString s

let private path (p: string) = p.Split('/') |> Array.map segment |> String.concat "/"

let private repoUrl (c: Connection) = sprintf "%s/repos/%s/%s" (c.ApiBase.TrimEnd '/') (segment c.Owner) (segment c.Name)

let private headers (c: Connection) accept =
    [ "authorization", "Bearer " + c.Token; "accept", accept; "x-github-api-version", "2022-11-28" ]

let private get c url representation =
    { Method = "GET"
      Url = url
      Headers = headers c (match representation with AsText -> "application/vnd.github.raw" | AsJson -> "application/vnd.github+json")
      Body = None
      Response = representation
      TimeoutMs = 15000 }

let repository (c: Connection) = get c (repoUrl c) AsJson

/// The branch head; the nonce defeats HTTP caching (PERSIST-005).
let head (c: Connection) (nonce: string) = get c (sprintf "%s/git/ref/heads/%s?n=%s" (repoUrl c) (path c.Branch) nonce) AsJson

let tree (c: Connection) (commit: string) = get c (sprintf "%s/git/trees/%s?recursive=1" (repoUrl c) commit) AsJson

/// A file read that names the commit it reads.
let file (c: Connection) (filePath: string) (commit: string) =
    get c (sprintf "%s/contents/%s?ref=%s" (repoUrl c) (path filePath) commit) AsJson

let compare (c: Connection) (baseCommit: string) (headRef: string) (nonce: string) =
    get c (sprintf "%s/compare/%s...%s?n=%s" (repoUrl c) baseCommit (path headRef) nonce) AsJson

let markerTrailer = "Causa-Write-Marker"

let commitMutation =
    "mutation CausaSave($input: CreateCommitOnBranchInput!) { createCommitOnBranch(input: $input) { commit { oid } } }"

/// One createCommitOnBranch mutation with an expected head (PERSIST-006, OD-INDY-9).
let createCommit (c: Connection) (expectedHead: string) (headline: string) (marker: string) (files: (string * string) list) =
    let addition (p: string, content: string) =
        JObject [ "path", JString p; "contents", JString(Convert.ToBase64String(utf8 content)) ]
    let input =
        JObject [
            "branch", JObject [ "repositoryNameWithOwner", JString(c.Owner + "/" + c.Name); "branchName", JString c.Branch ]
            "expectedHeadOid", JString expectedHead
            "message", JObject [ "headline", JString headline; "body", JString(markerTrailer + ": " + marker) ]
            "fileChanges", JObject [ "additions", JArray(files |> List.map addition) ]
        ]
    { Method = "POST"
      Url = c.ApiBase.TrimEnd('/') + "/graphql"
      Headers = [ "authorization", "Bearer " + c.Token; "content-type", "application/json" ]
      Body = Some(compact (JObject [ "query", JString commitMutation; "variables", JObject [ "input", input ] ]))
      Response = AsJson
      TimeoutMs = 20000 }

// ---- Classification ----

type CommitAnswer =
    | Committed of oid: string
    | StaleHead
    | CredentialRejected
    | PermissionRefused
    | RateLimit
    | ValidationRefused of string
    | NetworkNotSent
    | OutcomeUnknown

let private message body = match field "message" body with Some(JString m) -> m | _ -> ""

let private isRateLimitText (m: string) =
    let m = m.ToLowerInvariant()
    m.Contains "rate limit"

let private graphQLErrors body =
    match field "errors" body with
    | Some(JArray errors) ->
        errors |> List.map (fun e -> (match field "type" e with Some(JString t) -> t | _ -> ""), message e)
    | _ -> []

let private isOid (s: string) = s.Length = 40 && s |> Seq.forall Uri.IsHexDigit

/// Classifies a createCommitOnBranch answer from its typed content (PERSIST-008).
let classifyCommit (reply: Reply) : CommitAnswer =
    match reply with
    | Unsent -> NetworkNotSent
    | Lost -> OutcomeUnknown
    | Undecodable _ -> OutcomeUnknown
    | Answered(401, _) -> CredentialRejected
    | Answered((403 | 429), body) when isRateLimitText (message body) || graphQLErrors body |> List.exists (fst >> (=) "RATE_LIMITED") -> RateLimit
    | Answered(429, _) -> RateLimit
    | Answered(403, _) -> PermissionRefused
    | Answered(200, body) ->
        let oid =
            field "data" body
            |> Option.bind (field "createCommitOnBranch")
            |> Option.bind (field "commit")
            |> Option.bind (field "oid")
        match oid, graphQLErrors body with
        | Some(JString oid), [] when isOid oid -> Committed oid
        | _, errors when errors |> List.exists (fun (t, m) -> t = "STALE_DATA" || m.StartsWith "Expected branch to point to") -> StaleHead
        | _, errors when errors |> List.exists (fst >> (=) "RATE_LIMITED") -> RateLimit
        | _, errors when errors |> List.exists (fun (t, _) -> t = "FORBIDDEN" || t = "INSUFFICIENT_SCOPES") -> PermissionRefused
        | _, ((_, m) :: _ as errors) when errors |> List.forall (fun (t, _) -> t <> "") -> ValidationRefused m
        | _ -> OutcomeUnknown
    | Answered((400 | 404 | 422), body) -> ValidationRefused(message body)
    | Answered _ -> OutcomeUnknown

type ReadAnswer<'a> =
    | Read of 'a
    | Missing
    | ReadDenied of credential: bool
    | ReadRateLimited
    | ReadOffline
    | ReadFailed of string

let classifyRead (decode: Json -> Result<'a, string>) (reply: Reply) : ReadAnswer<'a> =
    match reply with
    | Answered(200, body) -> (match decode body with Ok v -> Read v | Error e -> ReadFailed("unexpected answer: " + e))
    | Answered(404, _) -> Missing
    | Answered(401, _) -> ReadDenied true
    | Answered((403 | 429), body) when isRateLimitText (message body) -> ReadRateLimited
    | Answered(403, _) -> ReadDenied false
    | Answered(status, _) -> ReadFailed(sprintf "GitHub answered %d" status)
    | Undecodable(Some 404) -> Missing
    | Undecodable(Some 401) -> ReadDenied true
    | Undecodable(Some 403) -> ReadDenied false
    | Undecodable status -> ReadFailed(sprintf "unreadable answer (%s)" (status |> Option.map string |> Option.defaultValue "no status"))
    | Unsent | Lost -> ReadOffline

let decodeHead (body: Json) =
    match field "object" body |> Option.bind (field "sha") with
    | Some(JString sha) when isOid sha -> Ok sha
    | _ -> Error "no branch head"

/// A contents-API answer: the exact file bytes, decoded from base64 as strict UTF-8.
let decodeText (body: Json) =
    match field "type" body, field "encoding" body, field "content" body with
    | Some(JString "file"), Some(JString "base64"), Some(JString content) ->
        try
            let bytes = Convert.FromBase64String(content.Replace("\n", ""))
            Ok(Text.UTF8Encoding(false, true).GetString bytes)
        with _ -> Error "the file is not valid UTF-8 base64 content"
    | Some(JString "file"), Some(JString "none"), _ -> Error "the file is too large to read through the contents API"
    | _ -> Error "expected a file"

type RepositoryInfo = { CanPush: bool; DefaultBranch: string }

let decodeRepository (body: Json) =
    let perm name = field "permissions" body |> Option.bind (field name) |> (function Some(JBool b) -> b | _ -> false)
    match field "default_branch" body with
    | Some(JString b) -> Ok { CanPush = perm "push"; DefaultBranch = b }
    | _ -> Error "not a repository"

/// Investigation ids under `<workspace>/investigations/<id>/manifest.json` in a tree listing.
let decodeTree (workspace: string) (body: Json) =
    let prefix = workspace.TrimEnd('/') + "/investigations/"
    match field "truncated" body, field "tree" body with
    | Some(JBool true), _ -> Error "the repository tree is too large to list completely"
    | _, Some(JArray entries) ->
        entries
        |> List.choose (fun e -> match field "path" e with Some(JString p) -> Some p | _ -> None)
        |> List.choose (fun p ->
            if p.StartsWith prefix && p.EndsWith "/manifest.json" then
                let id = p.Substring(prefix.Length, p.Length - prefix.Length - "/manifest.json".Length)
                if Causa.Domain.Prelude.Text.isUuid id then Some id else None
            else None)
        |> List.distinct
        |> List.sort
        |> Ok
    | _ -> Error "no tree listing"

type Comparison =
    { Status: string
      Commits: (string * string) list }

/// compare/{base}...{head}: whether base is an ancestor, and the commits after it (sha, message).
let decodeCompare (body: Json) =
    match field "status" body, field "commits" body with
    | Some(JString status), Some(JArray commits) ->
        let entries =
            commits
            |> List.choose (fun c ->
                match field "sha" c, field "commit" c |> Option.bind (field "message") with
                | Some(JString sha), Some(JString m) -> Some(sha, m)
                | _ -> None)
        Ok { Status = status; Commits = entries }
    | _ -> Error "no comparison"
