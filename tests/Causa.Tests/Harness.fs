/// Drives the real Causa engine (App.update) as browser tabs, running every effect it requests
/// against a provider: the stateful fake or the live GitHub test repository (PERSIST-025).
/// Faults (lost answers, other writers, network loss) are injected at this transport layer,
/// so the same scenario runs unchanged in both modes.
module Causa.Tests.Harness

open System
open System.Net.Http
open Causa.Domain
open Causa.Domain.Json
open Causa.Persistence
open Causa.Persistence.GitHub
open Causa.App
open Causa.Fake

type Provider =
    { Mode: string
      ApiBase: string
      Owner: string
      Name: string
      Branch: string
      Token: string
      Send: GitHub.Request -> Reply
      /// Another writer commits these files directly to the branch; returns the new commit.
      OtherWriter: (string * string) list -> string
      Head: unit -> string
      Reachable: string -> bool
      FileAt: string -> string -> string option }

// ---- Fake provider ----

let fakeProvider (owner: string) (name: string) (token: string) : Provider * (unit -> FakeGitHub.State) * ((FakeGitHub.State -> FakeGitHub.State) -> unit) =
    let state = ref (FakeGitHub.create owner name token [])
    let api = App.sameOriginApi
    let send (r: GitHub.Request) =
        let path = r.Url.Substring api.Length
        let auth = r.Headers |> List.tryFind (fst >> (=) "authorization") |> Option.map snd |> Option.defaultValue ""
        let next, answer = FakeGitHub.handle state.Value r.Method path auth (defaultArg r.Body "")
        state.Value <- next
        match answer with
        | FakeGitHub.Respond(status, body) -> Answered(status, body)
        | FakeGitHub.NetworkFailure -> if r.Method = "GET" then Unsent else Lost
        | FakeGitHub.AnswerLost -> Lost
    let provider =
        { Mode = "fake"; ApiBase = api; Owner = owner; Name = name; Branch = "main"; Token = token; Send = send
          OtherWriter = fun files -> state.Value <- FakeGitHub.commitAs state.Value "Another writer" files; state.Value.Head
          Head = fun () -> state.Value.Head
          Reachable = fun sha -> FakeGitHub.reachable state.Value sha
          FileAt = fun commit p -> state.Value.Commits.TryFind commit |> Option.bind (fun c -> c.Tree.TryFind p) }
    provider, (fun () -> state.Value), (fun f -> state.Value <- f state.Value)

// ---- Live provider (the real GitHub test repository) ----

let private http = new HttpClient(Timeout = TimeSpan.FromSeconds 30.0)

let private liveSend (r: GitHub.Request) : Reply =
    use message = new HttpRequestMessage(HttpMethod(r.Method), r.Url)
    message.Headers.TryAddWithoutValidation("user-agent", "causa-rehearsal-007-tests") |> ignore
    for (k, v) in r.Headers do
        if k <> "content-type" then message.Headers.TryAddWithoutValidation(k, v) |> ignore
    r.Body |> Option.iter (fun b -> message.Content <- new StringContent(b, Text.Encoding.UTF8, "application/json"))
    try
        use response = http.Send message
        let text = response.Content.ReadAsStringAsync().Result
        match parse text with
        | Ok body -> Answered(int response.StatusCode, body)
        | Error _ -> Undecodable(Some(int response.StatusCode))
    with _ -> if r.Method = "GET" then Unsent else Lost

let liveProvider (owner: string) (name: string) (branch: string) (token: string) : Provider =
    let conn = { ApiBase = "https://api.github.com"; Owner = owner; Name = name; Branch = branch; Workspace = ""; Token = token }
    let head () =
        match liveSend (GitHub.head conn (Guid.NewGuid().ToString "N")) with
        | Answered(200, body) -> (match decodeHead body with Ok h -> h | Error e -> failwith e)
        | other -> failwithf "could not read the live branch head: %A" other
    // The test's own other writer: retried with backoff on stale heads and rate limits; any other refusal
    // fails the test with GitHub's message (never the credential).
    let rec otherWriterAttempt (attempt: int) (files: (string * string) list) =
        let h = head ()
        let r = GitHub.createCommit conn h "Causa PAT: another writer" (Guid.NewGuid().ToString "N") files
        let reply = liveSend r
        let detail = match reply with Answered(status, body) -> sprintf "HTTP %d %s" status (compact body) | other -> sprintf "%A" other
        match classifyCommit reply with
        | Committed oid -> oid
        | StaleHead when attempt < 5 -> otherWriterAttempt (attempt + 1) files
        | RateLimit when attempt < 5 ->
            Threading.Thread.Sleep(TimeSpan.FromSeconds(float (5 * (attempt + 1))))
            otherWriterAttempt (attempt + 1) files
        | other -> failwithf "the other writer could not commit (%A): %s" other (detail.Replace(token, "***"))
    let otherWriter files = otherWriterAttempt 0 files
    { Mode = "live"; ApiBase = conn.ApiBase; Owner = owner; Name = name; Branch = branch; Token = token; Send = liveSend
      OtherWriter = otherWriter
      Head = head
      Reachable =
        fun sha ->
            match liveSend (GitHub.compare conn sha branch (Guid.NewGuid().ToString "N")) with
            | Answered(200, body) -> (match decodeCompare body with Ok c -> c.Status = "ahead" || c.Status = "identical" | _ -> false)
            | _ -> false
      FileAt =
        fun commit p ->
            match liveSend (GitHub.file conn p commit) with
            | Answered(200, body) -> decodeText body |> Result.toOption
            | _ -> None }

// ---- Fault injection at the transport ----

type Injection =
    | Forward
    | DropAnswer
    | DropRequest
    | AdvanceFirst

type Network =
    { mutable Online: bool
      /// Applied to each commit mutation in order; Forward once the list is exhausted.
      mutable Mutations: Injection list
      mutable EveryMutation: Injection option
      mutable FailReads: bool
      mutable Log: (string * GitHub.Request) list
      mutable Replies: (GitHub.Request * Reply) list
      mutable Clock: int }

let network () = { Online = true; Mutations = []; EveryMutation = None; FailReads = false; Log = []; Replies = []; Clock = 0 }

let private isMutation (r: GitHub.Request) = r.Method = "POST" && r.Url.EndsWith "/graphql"

let rec execute (p: Provider) (net: Network) (tab: string) (r: GitHub.Request) : Reply =
    net.Log <- net.Log @ [ tab, r ]
    let reply = transport p net r
    net.Replies <- net.Replies @ [ r, reply ]
    reply

and private transport (p: Provider) (net: Network) (r: GitHub.Request) : Reply =
    if not net.Online then Unsent
    elif isMutation r then
        let injection =
            match net.EveryMutation, net.Mutations with
            | Some i, _ -> i
            | None, i :: rest -> net.Mutations <- rest; i
            | None, [] -> Forward
        match injection with
        | Forward -> p.Send r
        | DropAnswer -> p.Send r |> ignore; Lost
        | DropRequest -> Lost
        | AdvanceFirst ->
            p.OtherWriter [ sprintf "notes/other-%s.md" (Guid.NewGuid().ToString "N"), "unrelated\n" ] |> ignore
            p.Send r
    elif net.FailReads then Unsent
    else p.Send r

// ---- Tabs ----

type Tab =
    { Name: string
      mutable Model: App.Model
      Env: App.Env
      mutable Faults: string list }

let private counter = ref 0

let private context : Engine.Context =
    { NewId = fun () -> Guid.NewGuid().ToString("D")
      Now = fun () -> Text.timestamp DateTime.UtcNow
      Actor = { ActorId = "causa-test"; DisplayName = None } }

let newTab (name: string) =
    counter.Value <- counter.Value + 1
    { Name = name; Model = App.init (sprintf "%s%04d%s" name counter.Value (Guid.NewGuid().ToString("N").Substring(0, 8)))
      Env = { Context = context; Nonce = fun () -> Guid.NewGuid().ToString("N").Substring(0, 12) }; Faults = [] }

type Driver = { Provider: Provider; Net: Network; mutable Queue: (Tab * Limen.Effect) list; mutable AutoTimers: bool }

let driver p = { Provider = p; Net = network (); Queue = []; AutoTimers = true }

let private apply (d: Driver) (tab: Tab) (msg: Limen.Inbound) =
    let next, effects = App.update tab.Env tab.Model msg
    // The rendered view must always be producible (it is what the page binds).
    View.render tab.Env next |> ignore
    tab.Model <- next
    d.Queue <- d.Queue @ (effects |> List.map (fun e -> tab, e))

/// Runs queued effects until none remain; timers fire on a virtual clock when AutoTimers is set.
let rec pump (d: Driver) =
    match d.Queue with
    | (tab, Limen.Http(id, r)) :: rest ->
        d.Queue <- rest
        let reply = execute d.Provider d.Net tab.Name r
        apply d tab (Limen.HttpResult(id, reply))
        pump d
    | (_, Limen.Navigate _) :: rest ->
        d.Queue <- rest
        pump d
    | [] -> ()

let rec settle (d: Driver) (tabs: Tab list) =
    pump d
    if d.AutoTimers then
        match tabs |> List.tryPick (fun t -> t.Model.Timer |> Option.map (fun (id, ms) -> t, id, ms)) with
        | Some(tab, id, ms) ->
            d.Net.Clock <- d.Net.Clock + ms
            apply d tab (Limen.Event("timer", None, Some id))
            settle d tabs
        | None -> ()

let send (d: Driver) (tab: Tab) (msg: Limen.Inbound) = apply d tab msg

let event (d: Driver) (tab: Tab) name key value =
    send d tab (Limen.Event(name, key, value))
    settle d [ tab ]

let fill (d: Driver) (tab: Tab) (field: string) (value: string) = send d tab (Limen.Event("draft:" + field, None, Some value))

let goTo (d: Driver) (tab: Tab) (id: string) = send d tab (Limen.LocationChanged { Path = "/"; Query = ""; Hash = "#/i/" + id })

let connect (d: Driver) (tab: Tab) (workspace: string) =
    let p = d.Provider
    [ "connect.owner", p.Owner; "connect.repo", p.Name; "connect.branch", p.Branch; "connect.workspace", workspace; "connect.token", p.Token
      "connect.api", (if p.ApiBase = App.defaultApi then "" else p.ApiBase) ]
    |> List.iter (fun (k, v) -> fill d tab k v)
    send d tab (Limen.Event("connect", None, None))
    settle d [ tab ]

let session (tab: Tab) id = tab.Model.Sessions.[id]

let saveState (tab: Tab) id = SaveState.name (session tab id).Save

/// Creates and saves a new investigation in `tab`; returns its id.
let createInvestigation (d: Driver) (tab: Tab) (title: string) =
    fill d tab "new.title" title
    fill d tab "new.problem" ("Why " + title.ToLowerInvariant() + "?")
    send d tab (Limen.Event("new.create", None, None))
    pump d
    match tab.Model.Route with
    | App.Open id -> id
    | _ -> failwith "the investigation was not created"

let observe (d: Driver) (tab: Tab) (id: string) (text: string) =
    goTo d tab id
    fill d tab "obs.text" text
    send d tab (Limen.Event("obs.add", None, None))

let save (d: Driver) (tab: Tab) (id: string) =
    send d tab (Limen.Event("save", Some id, None))
    settle d [ tab ]

let mutations (d: Driver) = d.Net.Log |> List.filter (fun (_, r) -> r.Method = "POST" && r.Url.EndsWith "/graphql")

let reads (kind: string) (d: Driver) = d.Net.Log |> List.filter (fun (_, r) -> r.Method = "GET" && r.Url.Contains kind)

let workspaceName () = sprintf "workspaces/r7-pat/%s" (Guid.NewGuid().ToString("N").Substring(0, 12))
