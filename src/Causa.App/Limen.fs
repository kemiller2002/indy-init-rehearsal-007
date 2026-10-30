/// The Limen 1.x message protocol as F# values (decode inbound, encode outbound).
module Causa.App.Limen

open Causa.Domain.Json
open Causa.Persistence.GitHub

type Location = { Path: string; Query: string; Hash: string }

type Inbound =
    | Initialize of Location
    | Event of name: string * key: string option * value: string option
    | HttpResult of correlationId: string * Reply
    | LocationChanged of Location
    | Ignored

type ViewValue =
    | VText of string
    | VBool of bool
    | VNumber of int
    | VList of (string * ViewValue) list list

type Effect =
    | Http of correlationId: string * Request
    | Navigate of correlationId: string * url: string

let private text name json = match field name json with Some(JString s) -> Some s | _ -> None

let private location json =
    match json with
    | Some l -> { Path = defaultArg (text "path" l) "/"; Query = defaultArg (text "query" l) ""; Hash = defaultArg (text "hash" l) "" }
    | None -> { Path = "/"; Query = ""; Hash = "" }

let private reply (outcome: Json) : Reply =
    let status = match field "status" outcome with Some(JNumber n) -> (match System.Int32.TryParse n with | true, v -> Some v | _ -> None) | _ -> None
    match text "kind" outcome, text "reason" outcome with
    | Some "Success", _ -> Answered(defaultArg status 0, defaultArg (field "body" outcome) JNull)
    | Some "Failure", Some "network" -> Unsent
    | Some "Failure", _ -> Undecodable status
    | _ -> Lost

let decode (message: string) : Inbound =
    match parse message with
    | Error _ -> Ignored
    | Ok json ->
        match text "kind" json with
        | Some "Initialize" -> Initialize(location (field "location" json))
        | Some "LocationChanged" -> LocationChanged(location (field "location" json))
        | Some "Event" ->
            match field "event" json with
            | Some e -> Event(defaultArg (text "name" e) "", text "key" e, text "value" e)
            | None -> Ignored
        | Some "EffectResult" ->
            match field "result" json with
            | Some r when text "kind" r = Some "HttpResult" ->
                match text "correlationId" r, field "outcome" r with
                | Some id, Some o -> HttpResult(id, reply o)
                | _ -> Ignored
            | _ -> Ignored
        | _ -> Ignored

let rec private viewJson =
    function
    | VText s -> JString s
    | VBool b -> JBool b
    | VNumber n -> JNumber(string n)
    | VList items -> JArray(items |> List.map (fun fields -> JObject(fields |> List.map (fun (k, v) -> k, viewJson v))))

let private effectJson =
    function
    | Navigate(id, url) -> JObject [ "kind", JString "Navigation"; "correlationId", JString id; "operation", JString "push"; "url", JString url ]
    | Http(id, r) ->
        JObject(
            [ "kind", JString "Http"
              "correlationId", JString id
              "method", JString r.Method
              "url", JString r.Url
              "headers", JObject(r.Headers |> List.map (fun (k, v) -> k, JString v)) ]
            @ (match r.Body with Some b -> [ "body", JString b ] | None -> [])
            @ [ "timeoutMs", JNumber(string r.TimeoutMs) ])

let encode (view: (string * ViewValue) list) (effects: Effect list) : string =
    compact (
        JObject [
            "view", JObject(view |> List.map (fun (k, v) -> k, viewJson v))
            "effects", JArray(effects |> List.map effectJson)
            "cancellations", JArray []
        ])
