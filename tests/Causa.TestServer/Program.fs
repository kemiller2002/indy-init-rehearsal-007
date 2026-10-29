/// Browser-test host: serves the built site and the stateful GitHub fake over HTTP at /github,
/// with /__fake controls for the browser PAT tests. Test-only; never deployed.
module Causa.TestServer.Program

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.FileProviders
open Microsoft.Extensions.Logging
open Causa.Domain.Json
open Causa.Fake

let private fresh () = FakeGitHub.create "causa-test" "data" "test-token" []

[<EntryPoint>]
let main argv =
    let site = argv |> Array.tryItem 0 |> Option.defaultValue "dist/site" |> Path.GetFullPath
    let port = argv |> Array.tryItem 1 |> Option.defaultValue "4300"
    let state = ref (fresh ())
    let gate = obj ()
    let builder = WebApplication.CreateBuilder()
    builder.Logging.ClearProviders() |> ignore
    let app = builder.Build()
    app.Urls.Add("http://127.0.0.1:" + port)

    let readBody (ctx: HttpContext) =
        use reader = new StreamReader(ctx.Request.Body)
        reader.ReadToEndAsync().Result

    let write (ctx: HttpContext) (status: int) (json: Json) =
        ctx.Response.StatusCode <- status
        ctx.Response.ContentType <- "application/json"
        ctx.Response.Headers.CacheControl <- "no-store"
        ctx.Response.WriteAsync(compact json)

    app.Map("/github/{**rest}", Func<HttpContext, Threading.Tasks.Task>(fun ctx ->
        let path = ctx.Request.Path.Value.Substring("/github".Length) + ctx.Request.QueryString.Value
        let auth = ctx.Request.Headers.Authorization.ToString()
        let body = readBody ctx
        let answer = lock gate (fun () -> let next, a = FakeGitHub.handle state.Value ctx.Request.Method path auth body in state.Value <- next; a)
        match answer with
        | FakeGitHub.Respond(status, json) -> write ctx status json
        | FakeGitHub.NetworkFailure | FakeGitHub.AnswerLost ->
            ctx.Abort()
            Threading.Tasks.Task.CompletedTask)) |> ignore

    let control (name: string) (f: HttpContext -> Json) =
        app.MapPost("/__fake/" + name, Func<HttpContext, Threading.Tasks.Task>(fun ctx -> write ctx 200 (lock gate (fun () -> f ctx)))) |> ignore

    let status () =
        JObject [
            "head", JString state.Value.Head
            "mutations", jint (int64 (FakeGitHub.mutationCount state.Value))
            "compares", jint (int64 (FakeGitHub.readCount "compare" state.Value))
            "requests", jint (int64 state.Value.Log.Length)
        ]
    control "reset" (fun _ -> state.Value <- fresh (); status ())
    control "status" (fun _ -> status ())
    control "other-writer" (fun ctx ->
        let p = ctx.Request.Query.["path"].ToString()
        let content = readBody ctx
        state.Value <- FakeGitHub.commitAs state.Value "Another writer" [ p, content ]
        status ())
    control "append-line" (fun ctx ->
        // Another writer appends text to a file (used to change an investigation behind a tab's back).
        let p = ctx.Request.Query.["path"].ToString()
        let current = (FakeGitHub.headCommit state.Value).Tree |> Map.tryFind p |> Option.defaultValue ""
        state.Value <- FakeGitHub.commitAs state.Value "Another writer" [ p, current + "x" ]
        status ())
    control "network" (fun ctx ->
        let down = ctx.Request.Query.["down"].ToString() = "true"
        let reads = ctx.Request.Query.["reads"].ToString() = "true"
        state.Value <- { state.Value with Behaviour = { state.Value.Behaviour with NetworkDown = down; ReadsDown = reads } }
        status ())
    control "lose-next" (fun ctx ->
        let loss = if ctx.Request.Query.["when"].ToString() = "before" then FakeGitHub.LoseBeforeApply else FakeGitHub.LoseAfterApply
        state.Value <- { state.Value with Behaviour = { state.Value.Behaviour with LoseNext = state.Value.Behaviour.LoseNext @ [ loss ] } }
        status ())
    control "fault" (fun ctx ->
        let fault =
            match ctx.Request.Query.["kind"].ToString() with
            | "unauthorized" -> FakeGitHub.Unauthorized
            | "forbidden" -> FakeGitHub.Forbidden
            | "rate" -> FakeGitHub.RateLimited
            | _ -> FakeGitHub.Invalid
        state.Value <- { state.Value with Behaviour = { state.Value.Behaviour with NextCommitFault = Some fault } }
        status ())
    control "advance-always" (fun ctx ->
        state.Value <- { state.Value with Behaviour = { state.Value.Behaviour with AdvanceBeforeEveryMutation = ctx.Request.Query.["on"].ToString() = "true" } }
        status ())

    let files = new PhysicalFileProvider(site)
    let provider = Microsoft.AspNetCore.StaticFiles.FileExtensionContentTypeProvider()
    provider.Mappings.[".wasm"] <- "application/wasm"
    provider.Mappings.[".dat"] <- "application/octet-stream"
    provider.Mappings.[".blat"] <- "application/octet-stream"
    app.UseDefaultFiles(DefaultFilesOptions(FileProvider = files)) |> ignore
    app.UseStaticFiles(StaticFileOptions(FileProvider = files, ContentTypeProvider = provider, ServeUnknownFileTypes = true)) |> ignore
    app.Run()
    0
