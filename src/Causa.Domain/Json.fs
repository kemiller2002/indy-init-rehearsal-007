/// A small immutable JSON tree, a strict parser and RFC 8785 (JCS) canonical output.
/// Numbers keep their source text so integers are never routed through floating point.
module Causa.Domain.Json

open System
open System.Text
open System.Text.Json

type Json =
    | JNull
    | JBool of bool
    | JNumber of string
    | JString of string
    | JArray of Json list
    | JObject of (string * Json) list

let rec private ofElement (e: JsonElement) : Json =
    match e.ValueKind with
    | JsonValueKind.Null -> JNull
    | JsonValueKind.True -> JBool true
    | JsonValueKind.False -> JBool false
    | JsonValueKind.Number -> JNumber(e.GetRawText())
    | JsonValueKind.String -> JString(e.GetString())
    | JsonValueKind.Array -> JArray [ for x in e.EnumerateArray() -> ofElement x ]
    | JsonValueKind.Object -> JObject [ for p in e.EnumerateObject() -> p.Name, ofElement p.Value ]
    | _ -> JNull

let private duplicateKeys (fields: (string * Json) list) =
    fields |> List.countBy fst |> List.exists (fun (_, n) -> n > 1)

let rec private hasDuplicateKeys =
    function
    | JObject fields -> duplicateKeys fields || fields |> List.exists (snd >> hasDuplicateKeys)
    | JArray items -> items |> List.exists hasDuplicateKeys
    | _ -> false

/// Parses one JSON document. Comments, trailing commas and duplicate keys are refused.
let parse (text: string) : Result<Json, string> =
    try
        let options = JsonDocumentOptions(AllowTrailingCommas = false, CommentHandling = JsonCommentHandling.Disallow)
        use doc = JsonDocument.Parse(text, options)
        let json = ofElement doc.RootElement
        if hasDuplicateKeys json then Error "duplicate object key" else Ok json
    with ex ->
        Error("invalid JSON: " + ex.Message)

let private escape (s: string) (sb: StringBuilder) =
    sb.Append('"') |> ignore
    for c in s do
        match c with
        | '"' -> sb.Append("\\\"") |> ignore
        | '\\' -> sb.Append("\\\\") |> ignore
        | '\b' -> sb.Append("\\b") |> ignore
        | '\f' -> sb.Append("\\f") |> ignore
        | '\n' -> sb.Append("\\n") |> ignore
        | '\r' -> sb.Append("\\r") |> ignore
        | '\t' -> sb.Append("\\t") |> ignore
        | c when c < ' ' -> sb.Append("\\u").Append((int c).ToString("x4")) |> ignore
        | c -> sb.Append(c) |> ignore
    sb.Append('"') |> ignore

let private canonicalNumber (raw: string) =
    match Int64.TryParse raw with
    | true, n -> string n
    | _ ->
        match Double.TryParse(raw, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
        | true, d when Math.Floor d = d && abs d < 9007199254740992.0 -> (int64 d).ToString()
        | true, d -> d.ToString("R", Globalization.CultureInfo.InvariantCulture).Replace("E+", "e+").Replace("E-", "e-")
        | _ -> raw

let rec private write (sort: bool) (sb: StringBuilder) (json: Json) =
    match json with
    | JNull -> sb.Append("null") |> ignore
    | JBool b -> sb.Append(if b then "true" else "false") |> ignore
    | JNumber raw -> sb.Append(canonicalNumber raw) |> ignore
    | JString s -> escape s sb
    | JArray items ->
        sb.Append('[') |> ignore
        items |> List.iteri (fun i x -> (if i > 0 then sb.Append(',') |> ignore); write sort sb x)
        sb.Append(']') |> ignore
    | JObject fields ->
        let ordered = if sort then fields |> List.sortWith (fun (a, _) (b, _) -> String.CompareOrdinal(a, b)) else fields
        sb.Append('{') |> ignore
        ordered
        |> List.iteri (fun i (k, v) ->
            (if i > 0 then sb.Append(',') |> ignore)
            escape k sb
            sb.Append(':') |> ignore
            write sort sb v)
        sb.Append('}') |> ignore

/// RFC 8785 canonical form: sorted keys (UTF-16 order), no whitespace.
let canonical (json: Json) : string =
    let sb = StringBuilder()
    write true sb json
    sb.ToString()

/// Compact output that keeps the given key order.
let compact (json: Json) : string =
    let sb = StringBuilder()
    write false sb json
    sb.ToString()

let utf8 (s: string) : byte[] = UTF8Encoding(false).GetBytes s

let sha256Hex (bytes: byte[]) : string =
    Security.Cryptography.SHA256.HashData(bytes) |> Convert.ToHexString |> fun s -> s.ToLowerInvariant()

let canonicalSha256 (json: Json) : string = canonical json |> utf8 |> sha256Hex

// ---- Accessors used by strict decoders ----

let field (name: string) (json: Json) : Json option =
    match json with
    | JObject fields -> fields |> List.tryFind (fst >> (=) name) |> Option.map snd
    | _ -> None

let keys (json: Json) : string list =
    match json with
    | JObject fields -> List.map fst fields
    | _ -> []

let str (name: string) (json: Json) : Result<string, string> =
    match field name json with
    | Some(JString s) -> Ok s
    | Some _ -> Error(name + " must be a string")
    | None -> Error(name + " is required")

let nonEmpty (name: string) (json: Json) : Result<string, string> =
    str name json |> Result.bind (fun s -> if String.IsNullOrWhiteSpace s then Error(name + " must be nonempty") else Ok s)

let strOrNull (name: string) (json: Json) : Result<string option, string> =
    match field name json with
    | Some(JString s) -> Ok(Some s)
    | Some JNull -> Ok None
    | Some _ -> Error(name + " must be a string or null")
    | None -> Error(name + " is required")

let int64Field (name: string) (json: Json) : Result<int64, string> =
    match field name json with
    | Some(JNumber raw) ->
        match Int64.TryParse raw with
        | true, n -> Ok n
        | _ -> Error(name + " must be an integer")
    | Some _ -> Error(name + " must be an integer")
    | None -> Error(name + " is required")

let strArray (name: string) (json: Json) : Result<string list, string> =
    match field name json with
    | Some(JArray items) ->
        let strings = items |> List.choose (function JString s -> Some s | _ -> None)
        if strings.Length = items.Length then Ok strings else Error(name + " must hold only strings")
    | Some _ -> Error(name + " must be an array")
    | None -> Error(name + " is required")

let onlyKeys (allowed: string list) (json: Json) : Result<unit, string> =
    match keys json |> List.filter (fun k -> not (List.contains k allowed)) with
    | [] -> Ok()
    | extra -> Error("unknown field(s): " + String.Join(", ", extra))

let jstr (s: string) = JString s
let jint (n: int64) = JNumber(string n)
let jstrOpt = function Some s -> JString s | None -> JNull
let jstrs (xs: string list) = xs |> List.map JString |> JArray
