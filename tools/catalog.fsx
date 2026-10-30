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

// Shape report: the field names the frozen contract uses, so the reader can be checked against real data.
let rec shape (json: Json) =
    match json with
    | JObject fs -> "{" + String.Join(",", fs |> List.map (fun (k, v) -> k + ":" + shape v)) + "}"
    | JArray [] -> "[]"
    | JArray (x :: _) -> "[" + shape x + "]"
    | JString _ -> "s" | JNumber _ -> "n" | JBool _ -> "b" | JNull -> "null"

let prefix = workspace + "/investigations/"
let ids = treePaths |> List.filter (fun p -> p.StartsWith prefix && p.EndsWith "/manifest.json") |> List.map (fun p -> p.Substring(prefix.Length).Split('/').[0]) |> List.distinct
let filesOf id : Causa.Domain.FileSet.Files =
    { Manifest = fileText (prefix + id + "/manifest.json"); Events = fileText (prefix + id + "/events.ndjson"); Snapshot = fileText (prefix + id + "/snapshot.json") }
let all = ids |> List.map (fun id -> id, filesOf id)
match all with
| (id, f) :: _ ->
    printfn "SHAPE manifest %s: %s" id (f.Manifest |> parse |> Result.map shape |> Result.defaultValue "unparseable")
    printfn "SHAPE snapshot %s: %s" id (f.Snapshot |> parse |> Result.map (fun j -> match j with JObject fs -> String.Join(",", fs |> List.map (fun (k, v) -> k + (match k, v with "state", JObject sfs -> "{" + String.Join(",", sfs |> List.map fst) + "}" | _ -> ""))) | _ -> shape j) |> Result.defaultValue "unparseable")
    printfn "FIRST EVENT %s: %s" id (f.Events.Split('\n').[0])
| [] -> printfn "no investigations under %s" prefix
let eventShapes =
    all
    |> List.collect (fun (_, f) -> f.Events.TrimEnd('\n').Split('\n') |> List.ofArray)
    |> List.choose (fun line -> parse line |> Result.toOption)
    |> List.map (fun e ->
        let t = match field "eventType" e with Some(JString t) -> t | _ -> match field "type" e with Some(JString t) -> t | _ -> "?"
        t, (match e with JObject fs -> String.Join(",", fs |> List.map fst) | _ -> ""), (field "payload" e |> Option.map shape |> Option.defaultValue "-"))
    |> List.distinct
for (t, keys, payload) in eventShapes do printfn "EVENT %s envelope=[%s] payload=%s" t keys payload

// Enumerations actually used: distinct short values of each payload string field, per event type.
let isIdOrTime (v: string) = Causa.Domain.Prelude.Text.isUuid v || Causa.Domain.Prelude.Text.isTimestamp v
let enumValues =
    all
    |> List.collect (fun (_, f) -> f.Events.TrimEnd('\n').Split('\n') |> List.ofArray)
    |> List.choose (fun line -> parse line |> Result.toOption)
    |> List.collect (fun e ->
        let t = match field "eventType" e with Some(JString t) -> t | _ -> "?"
        let rec strs prefix j =
            match j with
            | JObject fs -> fs |> List.collect (fun (k, v) -> strs (prefix + "." + k) v)
            | JArray xs -> xs |> List.collect (strs (prefix + "[]"))
            | JString s when s.Length <= 48 && not (isIdOrTime s) -> [ prefix, s ]
            | _ -> []
        (strs (t + ".payload") (defaultArg (field "payload" e) JNull)) @ (strs (t + ".actor") (defaultArg (field "actor" e) JNull)))
    |> List.groupBy fst
    |> List.map (fun (k, vs) -> k, vs |> List.map snd |> List.distinct |> List.sort)
for (k, vs) in enumValues do
    if vs.Length <= 25 then printfn "ENUM %s = %s" k (String.Join(" | ", vs)) else printfn "FREE %s (%d distinct)" k vs.Length

// Snapshot entity shapes and the digest definitions.
for (id, f) in all |> List.truncate 1 do
    match parse f.Snapshot with
    | Ok snap ->
        match field "state" snap with
        | Some(JObject fs) ->
            for (k, v) in fs do
                match v with
                | JArray(x :: _) -> printfn "STATE %s[] = %s" k (shape x)
                | other -> printfn "STATE %s = %s" k (shape other)
            for k in [ "hypotheses"; "evidence"; "actions"; "causes"; "observations" ] do
                match fs |> List.tryFind (fst >> (=) k) |> Option.map snd with
                | Some(JArray(x :: _)) -> let t = compact x in printfn "SAMPLE %s: %s" k (if t.Length > 700 then t.Substring(0, 700) + "..." else t)
                | _ -> ()
        | _ -> ()
    | Error e -> printfn "snapshot unparseable: %s" e
let caseOf id =
    match parse catalogManifest |> Result.toOption |> Option.bind (field "cases") with
    | Some(JArray cs) -> cs |> List.tryFind (fun c -> field "investigationId" c = Some(JString id))
    | _ -> None
for (id, f) in all do
    let m = parse f.Manifest |> Result.defaultValue JNull
    let snap = parse f.Snapshot |> Result.defaultValue JNull
    let txt k j = match field k j with Some(JString s) -> s | _ -> "-"
    let c = caseOf id |> Option.defaultValue JNull
    printfn "DIGEST %s events raw=%b | snapshot canonical=%b raw=%b | state canonical=%b(expectedState) | manifest canonical=%b raw=%b | rawNewline snapshot=%b" id
        (sha256Hex (utf8 f.Events) = txt "eventsSha256" m)
        (canonicalSha256 snap = txt "snapshotSha256" m) (sha256Hex (utf8 f.Snapshot) = txt "snapshotSha256" m)
        (field "state" snap |> Option.map canonicalSha256 = Some(txt "expectedStateSha256" c))
        (canonicalSha256 m = txt "manifestSha256" c) (sha256Hex (utf8 f.Manifest) = txt "manifestSha256" c)
        (f.Snapshot.EndsWith "\n")
    printfn "MANIFEST %s %s" id (compact m)

let results =
    [ for (id, files) in all do
        match Causa.Domain.FileSet.verify id files with
        | Ok v -> printfn "VERIFIED %s/%s: %s (version %d, %s)" workspace id v.Investigation.Title v.Investigation.Version v.Investigation.Lifecycle; yield Ok id
        | Error e -> printfn "REFUSED %s/%s: %s" workspace id e; yield Error e ]

let verified = results |> List.filter Result.isOk |> List.length
printfn "verdict: %s (%d verified, %d refused)" (if verified = 4 && results.Length = 4 then "PASS" else "FAIL") verified (results.Length - verified)
exit (if verified = 4 && results.Length = 4 then 0 else 1)
