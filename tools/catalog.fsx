// Live catalog read (EXT-DATA-013, PF-014): reads causa-data's active catalog with CAUSA_GITHUB_TOKEN and
// loads every investigation in it through Causa's own contract verification and F# replay.
// Run after `dotnet build src/Causa.Persistence -c Release`. Never prints the credential.
#r "../src/Causa.Domain/bin/Release/net10.0/Causa.Domain.dll"
#r "../src/Causa.Persistence/bin/Release/net10.0/Causa.Persistence.dll"

open System
open System.Net.Http
open Causa.Domain.Json
open Causa.Persistence.GitHub

let token = Environment.GetEnvironmentVariable "CAUSA_GITHUB_TOKEN"
let repo = Environment.GetEnvironmentVariable "CAUSA_DATA_REPO" |> Option.ofObj |> Option.defaultValue "kemiller2002/causa-data"
let owner, name = match repo.Split('/') with [| o; n |] -> o, n | _ -> failwith "CAUSA_DATA_REPO must be owner/name"
let conn = { ApiBase = "https://api.github.com"; Owner = owner; Name = name; Branch = "main"; Workspace = ""; Token = token }
let http = new HttpClient()

let send (r: Request) =
    use m = new HttpRequestMessage(HttpMethod(r.Method), r.Url)
    m.Headers.TryAddWithoutValidation("user-agent", "causa-r7-catalog") |> ignore
    for (k, v) in r.Headers do m.Headers.TryAddWithoutValidation(k, v) |> ignore
    use resp = http.Send m
    int resp.StatusCode, (resp.Content.ReadAsStringAsync().Result |> parse |> Result.defaultValue JNull)

let read decode r =
    match send r with
    | 200, body -> decode body |> Result.defaultWith failwith
    | status, body -> failwithf "%s answered %d: %s" r.Url status (match field "message" body with Some(JString m) -> m | _ -> "")

let head = read decodeHead (Causa.Persistence.GitHub.head conn (Guid.NewGuid().ToString "N"))
printfn "causa-data main = %s" head
let fileText path = read decodeText (file conn path head)
let active = fileText "catalog/active.json"
printfn "catalog/active.json:\n%s" active

let treePaths =
    match send (tree conn head) with
    | 200, body ->
        match field "tree" body with
        | Some(JArray entries) -> entries |> List.choose (fun e -> match field "path" e with Some(JString p) -> Some p | _ -> None)
        | _ -> []
    | status, _ -> failwithf "tree answered %d" status

let workspaces =
    treePaths
    |> List.filter (fun p -> p.EndsWith "/manifest.json" && p.Contains "/investigations/")
    |> List.map (fun p -> p.Substring(0, p.IndexOf "/investigations/"))
    |> List.countBy id
printfn "investigation folders by workspace:"
for (ws, n) in workspaces do printfn "  %s: %d" ws n

// The active catalog names its workspace and manifest (catalog/active.json: workspace, manifest).
let activeJson = active |> parse |> Result.defaultWith failwith
let named key = match field key activeJson with Some(JString v) -> v | _ -> failwithf "catalog/active.json has no %s" key
let workspace = named "workspace"
let catalogManifest = fileText (named "manifest")
printfn "catalog manifest (%s):\n%s" (named "manifest") (if catalogManifest.Length > 6000 then catalogManifest.Substring(0, 6000) + "..." else catalogManifest)

let prefix = workspace + "/investigations/"
let ids = treePaths |> List.filter (fun p -> p.StartsWith prefix && p.EndsWith "/manifest.json") |> List.map (fun p -> p.Substring(prefix.Length).Split('/').[0]) |> List.distinct
let filesOf id : Causa.Domain.FileSet.Files =
    { Manifest = fileText (prefix + id + "/manifest.json"); Events = fileText (prefix + id + "/events.ndjson"); Snapshot = fileText (prefix + id + "/snapshot.json") }

let cases =
    match parse catalogManifest |> Result.toOption |> Option.bind (field "cases") with
    | Some(JArray cs) -> cs
    | _ -> failwith "the catalog manifest lists no cases"
let caseText k c = match field k c with Some(JString v) -> v | _ -> ""
let caseInt k c = match field k c with Some(JNumber v) -> int64 v | _ -> -1L

let check (c: Json) =
    let id = caseText "investigationId" c
    if not (List.contains id ids) then Error(sprintf "%s: listed in the catalog but absent from %s" id workspace)
    else
        let files = filesOf id
        let prefixLine = sprintf "%s (%s)" (caseText "caseKey" c) id
        match Causa.Domain.FileSet.parseEvents files.Events |> Result.bind Causa.Domain.Engine.replay with
        | Error e -> Error(sprintf "%s: refused by replay: %s" prefixLine e)
        | Ok inv ->
            let snapshotState = parse files.Snapshot |> Result.toOption |> Option.bind (field "state") |> Option.defaultValue JNull
            let replayed = Causa.Domain.FileSet.stateJson inv
            let diffs = Causa.Domain.FileSet.diffPaths "state" snapshotState replayed
            for d in diffs |> List.truncate 40 do printfn "  DIFF %s %s" (caseText "caseKey" c) d
            if diffs.Length > 40 then printfn "  DIFF %s ... %d more" (caseText "caseKey" c) (diffs.Length - 40)
            let expectations =
                [ "version", inv.Version = caseInt "expectedVersion" c
                  "lifecycle", inv.Lifecycle = caseText "expectedLifecycle" c
                  "state digest", canonicalSha256 replayed = caseText "expectedStateSha256" c
                  "events digest", sha256Hex (utf8 files.Events) = caseText "eventsSha256" c ]
            let failed = expectations |> List.filter (snd >> not) |> List.map fst
            match failed, Causa.Domain.FileSet.verify id files with
            | [], Ok v -> Ok(sprintf "%s: VERIFIED by full contract validation and replay: version %d, %s, state digest matches the catalog" prefixLine v.Investigation.Version v.Investigation.Lifecycle)
            | _, Error e -> Error(sprintf "%s: refused: %s" prefixLine e)
            | missing, _ -> Error(sprintf "%s: replay differs from the catalog on %s" prefixLine (String.Join(", ", missing)))

// awaitingSinceVersion rule finder: which event version the recorded value equals (booleans only, no values).
for c in cases do
    let id = caseText "investigationId" c
    if List.contains id ids then
        let files = filesOf id
        let events = Causa.Domain.FileSet.parseEvents files.Events |> Result.defaultValue []
        let recorded =
            match parse files.Snapshot |> Result.toOption |> Option.bind (field "state") |> Option.bind (field "correctiveActions") with
            | Some(JArray xs) -> xs
            | _ -> []
        for (i, ca) in List.indexed recorded do
            let caId = match field "id" ca with Some(JString v) -> v | _ -> ""
            let expected = match field "awaitingSinceVersion" ca with Some(JNumber v) -> Some(int64 v) | _ -> None
            let mine = events |> List.filter (fun e -> field "correctiveActionId" e.Payload = Some(JString caId))
            let transitions = mine |> List.filter (fun e -> e.EventType = "CorrectiveActionTransitioned")
            let versionOf (f: Causa.Domain.Engine.Envelope -> bool) = mine |> List.tryFind f |> Option.map (fun e -> e.AggregateVersion)
            let toState st = versionOf (fun e -> e.EventType = "CorrectiveActionTransitioned" && field "state" e.Payload = Some(JString st))
            let criterion = events |> List.tryFind (fun e -> e.EventType = "CriterionRecorded" && field "correctiveActionId" e.Payload = Some(JString caId)) |> Option.map (fun e -> e.AggregateVersion)
            let candidates =
                [ "creation", versionOf (fun e -> e.EventType = "CorrectiveActionRecorded")
                  "firstTransition", transitions |> List.tryHead |> Option.map (fun e -> e.AggregateVersion)
                  "lastTransition", transitions |> List.tryLast |> Option.map (fun e -> e.AggregateVersion)
                  "approved", toState "approved"; "inProgress", toState "inProgress"; "implemented", toState "implemented"
                  "awaiting", toState "awaitingVerificationOfEffect"; "criterion", criterion
                  "zero", Some 0L; "minusOne", Some -1L; "final", events |> List.tryLast |> Option.map (fun e -> e.AggregateVersion)
                  "lifecycleAwaitingVerification", events |> List.tryFind (fun e -> e.EventType = "LifecycleChanged" && field "to" e.Payload = Some(JString "awaitingVerification")) |> Option.map (fun e -> e.AggregateVersion)
                  "lastLifecycle", events |> List.filter (fun e -> e.EventType = "LifecycleChanged") |> List.tryLast |> Option.map (fun e -> e.AggregateVersion) ]
            let state = match field "state" ca with Some(JString v) -> v | _ -> "?"
            printfn "AWAIT %s[%d] state=%s %s" (caseText "caseKey" c) i state
                (String.Join(" ", candidates |> List.map (fun (n, v) -> sprintf "%s=%b" n (v.IsSome && v = expected))))

let results = cases |> List.map check
for r in results do (match r with Ok m -> printfn "%s" m | Error e -> printfn "REFUSED %s" e)
let verified = results |> List.filter Result.isOk |> List.length
let pass = verified = 4 && results.Length = 4
printfn "verdict: %s (%d of %d catalog cases verified)" (if pass then "PASS" else "FAIL") verified results.Length
exit (if pass then 0 else 1)
