// Causa build: `dotnet fsi build.fsx <target>` (KD-008: FAKE 6 cannot run on the pinned .NET 10 SDK).
// Targets: site (publish the WebAssembly engine and assemble dist/site), test (offline F# suites),
// all (both). Each target fails the process on the first failing step.
open System
open System.Diagnostics
open System.IO

let root = __SOURCE_DIRECTORY__
let site = Path.Combine(root, "dist", "site")

let run (file: string) (args: string) =
    printfn "> %s %s" file args
    let p = Process.Start(ProcessStartInfo(file, args, WorkingDirectory = root, UseShellExecute = false))
    p.WaitForExit()
    if p.ExitCode <> 0 then failwithf "%s %s failed with exit code %d" file args p.ExitCode

let rec copyDir (src: string) (dst: string) =
    Directory.CreateDirectory dst |> ignore
    for f in Directory.GetFiles src do File.Copy(f, Path.Combine(dst, Path.GetFileName f), true)
    for d in Directory.GetDirectories src do copyDir d (Path.Combine(dst, Path.GetFileName d))

/// The Limen modules reachable from the entry points: only these ship (PAT-13 scans what ships).
let limenModules (dist: string) (entries: string list) =
    let pattern = Text.RegularExpressions.Regex("""(?:import|export)[^"']*?from\s*["']([^"']+)["']|import\(\s*["']([^"']+)["']\s*\)""")
    let rec walk (seen: Set<string>) (file: string) =
        if seen.Contains file then seen
        else
            let text = File.ReadAllText file
            [ for m in pattern.Matches text -> if m.Groups.[1].Success then m.Groups.[1].Value else m.Groups.[2].Value ]
            |> List.filter (fun spec -> spec.StartsWith ".")
            |> List.map (fun spec -> Path.GetFullPath(Path.Combine(Path.GetDirectoryName file, spec)))
            |> List.fold walk (seen.Add file)
    entries |> List.map (fun e -> Path.Combine(dist, e)) |> List.fold walk Set.empty |> Set.toList

let siteTarget () =
    let publish = Path.Combine(root, "dist", "publish")
    if Directory.Exists site then Directory.Delete(site, true)
    if Directory.Exists publish then Directory.Delete(publish, true)
    run "dotnet" (sprintf "publish src/Causa.Host -c Release -o \"%s\"" publish)
    copyDir (Path.Combine(publish, "wwwroot", "_framework")) (Path.Combine(site, "_framework"))
    for f in Directory.GetFiles(Path.Combine(root, "web")) do File.Copy(f, Path.Combine(site, Path.GetFileName f), true)
    let limen = Path.Combine(root, "node_modules", "@echelon-foundry", "limen", "dist")
    for f in limenModules limen [ "index.js"; "hosts/dotnet-wasm-transport.js" ] do
        let target = Path.Combine(site, "limen", "dist", Path.GetRelativePath(limen, f))
        Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore
        File.Copy(f, target, true)
    Directory.CreateDirectory(Path.Combine(site, "forma")) |> ignore
    File.Copy(Path.Combine(root, "node_modules", "@echelon-foundry", "design-system", "dist", "all.css"), Path.Combine(site, "forma", "all.css"), true)
    File.WriteAllText(Path.Combine(site, ".nojekyll"), "")
    let sha = Environment.GetEnvironmentVariable "GITHUB_SHA" |> Option.ofObj |> Option.defaultValue "local"
    File.WriteAllText(Path.Combine(site, "build.json"), sprintf "{\"sourceSha\":\"%s\"}\n" sha)
    printfn "site assembled at %s" site

let testTarget () = run "dotnet" "run -c Release --project tests/Causa.Tests -- --summary"

match fsi.CommandLineArgs |> Array.tryItem 1 |> Option.defaultValue "all" with
| "site" -> siteTarget ()
| "test" -> testTarget ()
| "all" -> testTarget (); siteTarget ()
| other -> failwithf "unknown target %s" other
