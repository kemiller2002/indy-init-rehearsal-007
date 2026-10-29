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

let head = read decodeHead (GitHub.head conn (Guid.NewGuid().ToString "N"))
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

// The active catalog names its workspace or catalog folder somewhere in its string values.
let rec strings = function JString s -> [ s ] | JArray xs -> List.collect strings xs | JObject fs -> fs |> List.collect (snd >> strings) | _ -> []
let named = active |> parse |> Result.map strings |> Result.defaultValue []
let chosen =
    workspaces
    |> List.map fst
    |> List.filter (fun ws -> named |> List.exists (fun s -> s.TrimEnd('/') = ws || s.StartsWith(ws + "/") || ws.EndsWith("/" + s.TrimEnd('/'))))
printfn "workspaces named by the active catalog: %s" (String.Join(", ", chosen))

let results =
    [ for ws in chosen do
        let prefix = ws + "/investigations/"
        let ids = treePaths |> List.filter (fun p -> p.StartsWith prefix && p.EndsWith "/manifest.json") |> List.map (fun p -> p.Substring(prefix.Length).Split('/').[0]) |> List.distinct
        for id in ids do
            let files : Causa.Domain.FileSet.Files =
                { Manifest = fileText (prefix + id + "/manifest.json"); Events = fileText (prefix + id + "/events.ndjson"); Snapshot = fileText (prefix + id + "/snapshot.json") }
            match Causa.Domain.FileSet.verify id files with
            | Ok v -> printfn "VERIFIED %s/%s: %s (version %d, %s)" ws id v.Investigation.Title v.Investigation.Version v.Investigation.Lifecycle; yield Ok id
            | Error e -> printfn "REFUSED %s/%s: %s" ws id e; yield Error e ]

let verified = results |> List.filter Result.isOk |> List.length
printfn "verdict: %s (%d verified, %d refused)" (if verified = 4 && results.Length = 4 then "PASS" else "FAIL") verified (results.Length - verified)
exit (if verified = 4 && results.Length = 4 then 0 else 1)
