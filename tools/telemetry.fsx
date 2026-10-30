// Token telemetry from the Claude Code session transcript (read-only). Usage is taken only from the
// provider's per-message `usage` records, de-duplicated by message id (the last record wins, since one
// message can be written several times while it streams). Nothing is estimated.
//   dotnet fsi tools/telemetry.fsx <transcript.jsonl[,more.jsonl]> <t0 ISO> <session id>
// Several transcripts (one session spread over working directories) are merged; message ids de-duplicate overlap.
#r "../src/Causa.Domain/bin/Release/net10.0/Causa.Domain.dll"
open System
open System.IO
open Causa.Domain.Json

let args = fsi.CommandLineArgs |> Array.tail
let transcript, t0, sessionId = args.[0], DateTime.Parse(args.[1]).ToUniversalTime(), args.[2]

let num name j = match field name j with Some(JNumber n) -> int64 n | _ -> 0L
let text name j = match field name j with Some(JString s) -> Some s | _ -> None

type Usage = { Id: string; Model: string; At: DateTime; Input: int64; Output: int64; CacheRead: int64; CacheWrite: int64; ToolUses: string list }

let records =
    transcript.Split(',')
    |> Seq.collect File.ReadLines
    |> Seq.choose (fun line -> match parse line with Ok j -> Some j | Error _ -> None)
    |> Seq.choose (fun j ->
        match field "message" j, text "timestamp" j with
        | Some m, Some ts ->
            match field "usage" m, text "id" m with
            | Some u, Some id ->
                let tools = match field "content" m with Some(JArray xs) -> xs |> List.filter (fun x -> text "type" x = Some "tool_use") |> List.choose (text "id") | _ -> []
                Some { Id = id; Model = defaultArg (text "model" m) "unknown"; At = DateTime.Parse(ts).ToUniversalTime()
                       Input = num "input_tokens" u; Output = num "output_tokens" u; CacheRead = num "cache_read_input_tokens" u
                       CacheWrite = num "cache_creation_input_tokens" u; ToolUses = tools }
            | _ -> None
        | _ -> None)
    |> Seq.groupBy (fun r -> r.Id)
    |> Seq.map (fun (_, rs) -> let last = Seq.last rs in { last with ToolUses = rs |> Seq.collect (fun r -> r.ToolUses) |> Seq.distinct |> List.ofSeq })
    |> List.ofSeq

let summary label (rs: Usage list) =
    JObject [
        "phase", JString label
        "messages", jint (int64 rs.Length)
        "inputTokens", jint (rs |> List.sumBy (fun r -> r.Input))
        "outputTokens", jint (rs |> List.sumBy (fun r -> r.Output))
        "cacheReadTokens", jint (rs |> List.sumBy (fun r -> r.CacheRead))
        "cacheWriteTokens", jint (rs |> List.sumBy (fun r -> r.CacheWrite))
        "toolCalls", jint (rs |> List.sumBy (fun r -> int64 r.ToolUses.Length))
        "models", JArray(rs |> List.map (fun r -> JString r.Model) |> List.distinct)
        "first", (rs |> List.tryHead |> Option.map (fun r -> JString(Causa.Domain.Prelude.Text.timestamp r.At)) |> Option.defaultValue JNull)
        "last", (rs |> List.tryLast |> Option.map (fun r -> JString(Causa.Domain.Prelude.Text.timestamp r.At)) |> Option.defaultValue JNull)
    ]

let sorted = records |> List.sortBy (fun r -> r.At)
let report =
    JObject [
        "schema", JString "causa.telemetry/1"
        "source", JString "Claude Code session transcripts (provider usage records), read-only"
        "transcripts", JArray(transcript.Split(',') |> Array.map (fun p -> JString(IO.Path.GetFileName(IO.Path.GetDirectoryName p) + "/" + IO.Path.GetFileName p)) |> List.ofArray)
        "sessionId", JString sessionId
        "completeness", JString "partial: this session's main agent only; subagents and CI runners are not included; cost not computed (no authoritative pricing in this session)"
        "preflight", summary "preflight (before T0)" (sorted |> List.filter (fun r -> r.At < t0))
        "timed", summary "timed run (from T0)" (sorted |> List.filter (fun r -> r.At >= t0))
    ]
printfn "%s" (canonical report)
