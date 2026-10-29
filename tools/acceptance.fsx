// Acceptance checks that inspect artifacts rather than behaviour.
//   dotnet fsi tools/acceptance.fsx artifact <site-dir>       PAT-13: no Web Locks, no browser-store writes in shipped code
//   dotnet fsi tools/acceptance.fsx negative                  PAT-12: application code cannot construct Saved
//   dotnet fsi tools/acceptance.fsx provenance <candidate> <prior-repo-dir>...
//                                                             PAT-13: no >20-line source file byte-identical to a prior rehearsal
// Each prints a JSON verdict line and exits non-zero on FAIL. provenance with no prior repositories is BLOCKED.
open System
open System.IO
open System.Diagnostics

let verdict (check: string) (state: string) (details: string list) =
    let quote (s: string) = "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""
    printfn "{\"check\":%s,\"verdict\":%s,\"details\":[%s]}" (quote check) (quote state) (details |> List.map quote |> String.concat ",")
    exit (if state = "FAIL" then 1 else 0)

let textFiles (dir: string) =
    Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
    |> Array.filter (fun f -> [ ".js"; ".mjs"; ".html"; ".css" ] |> List.contains (Path.GetExtension f))
    |> List.ofArray

// Patterns that would coordinate tabs or keep investigation state in the browser (PERSIST-003, PERSIST-004).
let lockPatterns = [ "navigator.locks"; "locks.request"; "LockManager" ]
let storePatterns = [ "localStorage"; "sessionStorage"; "indexedDB"; "caches.open"; "BroadcastChannel"; "SharedWorker"; "serviceWorker"; "document.cookie"; "getDirectory" ]

/// The vendored Limen kernel implements the generic Storage effect and same-origin XSRF cookies; Causa's
/// engine cannot request either, because its effect type has only Http and Navigation (src/Causa.App/Limen.fs).
/// The .NET runtime's optional boot-resource cache stores framework assets only, never investigation content;
/// PAT-01 verifies at run time that Cache Storage stays empty (caches.keys() = []).
let vendoredAllowance =
    [ "limen/dist/kernel/browser-kernel.js", [ "localStorage"; "document.cookie" ]
      "limen/dist/generated/core.codec.js", [ "document.cookie" ]
      "_framework/dotnet.runtime.", [ "caches.open" ] ]

let artifact (site: string) =
    let findings =
        [ for f in textFiles site do
            let rel = Path.GetRelativePath(site, f).Replace('\\', '/')
            let text = File.ReadAllText f
            for p in lockPatterns do if text.Contains p then yield sprintf "%s references Web Locks (%s)" rel p
            let allowed = vendoredAllowance |> List.tryFind (fun (p, _) -> rel = p || (p.EndsWith "." && rel.StartsWith p)) |> Option.map snd |> Option.defaultValue []
            for p in storePatterns do if text.Contains p && not (List.contains p allowed) then yield sprintf "%s references a browser store (%s)" rel p ]
    let engine = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "../src/Causa.App/Limen.fs"))
    let kinds = Text.RegularExpressions.Regex.Matches(engine, "\"kind\", JString \"(\\w+)\"") |> Seq.map (fun m -> m.Groups.[1].Value) |> Seq.distinct |> List.ofSeq
    let effectFindings = kinds |> List.filter (fun k -> k <> "Http" && k <> "Navigation") |> List.map (sprintf "the engine can request a %s effect")
    let wasm = Directory.GetFiles(Path.Combine(site, "_framework"), "Causa.*.wasm") |> Array.map Path.GetFileName |> List.ofArray
    match findings @ effectFindings with
    | [] -> verdict "PAT-13 artifact" "PASS" ([ sprintf "%d shipped text files scanned" (textFiles site).Length; sprintf "engine effect kinds: %s" (String.Join(",", kinds)) ] @ wasm)
    | problems -> verdict "PAT-13 artifact" "FAIL" problems

let negative () =
    let p = Process.Start(ProcessStartInfo("dotnet", "build tests/negative -c Release", RedirectStandardOutput = true, UseShellExecute = false))
    let output = p.StandardOutput.ReadToEnd()
    p.WaitForExit()
    if p.ExitCode <> 0 && output.Contains "FS1093" then verdict "PAT-12 compile-time negative" "PASS" [ "constructing Confirmation outside its module fails with FS1093" ]
    else verdict "PAT-12 compile-time negative" "FAIL" [ "the negative project compiled, or failed for another reason" ]

let normalize (f: string) = File.ReadAllText(f).Replace("\r\n", "\n")

let sourceFiles (dir: string) =
    let extensions = [ ".fs"; ".fsx"; ".cs"; ".js"; ".mjs"; ".ts"; ".css"; ".html"; ".sh" ]
    Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
    |> Array.filter (fun f -> List.contains (Path.GetExtension f) extensions)
    |> Array.filter (fun f -> not (f.Contains "/node_modules/" || f.Contains "/bin/" || f.Contains "/obj/" || f.Contains "/.git/" || f.Contains "/dist/"))
    |> List.ofArray

let provenance (candidate: string) (priors: string list) =
    if priors.IsEmpty then verdict "PAT-13 provenance" "BLOCKED" [ "no prior rehearsal checkouts supplied; this agent is not permitted to access them (mission §2.2)" ]
    else
        let ours = sourceFiles candidate |> List.map (fun f -> f, normalize f) |> List.filter (fun (_, t) -> t.Split('\n').Length > 20)
        let theirs = priors |> List.collect sourceFiles |> List.map normalize |> Set.ofList
        match ours |> List.filter (fun (_, t) -> theirs.Contains t) |> List.map fst with
        | [] -> verdict "PAT-13 provenance" "PASS" [ sprintf "%d candidate files over 20 lines compared with %d prior files" ours.Length theirs.Count ]
        | copied -> verdict "PAT-13 provenance" "FAIL" (copied |> List.map (sprintf "byte-identical to a prior rehearsal file: %s"))

match fsi.CommandLineArgs |> List.ofArray |> List.tail with
| [ "artifact"; site ] -> artifact site
| [ "negative" ] -> negative ()
| "provenance" :: candidate :: priors -> provenance candidate priors
| _ -> failwith "usage: acceptance.fsx artifact <site> | negative | provenance <candidate> <prior>..."
