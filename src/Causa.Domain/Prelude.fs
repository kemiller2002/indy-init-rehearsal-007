[<AutoOpen>]
module Causa.Domain.Prelude

type ResultBuilder() =
    member _.Return x = Ok x
    member _.ReturnFrom(r: Result<_, _>) = r
    member _.Bind(r, f) = Result.bind f r
    member _.Zero() = Ok()

let result = ResultBuilder()

module Result =
    /// Applies f to each item in order, stopping at the first error.
    let traverse (f: 'a -> Result<'b, 'e>) (items: 'a list) : Result<'b list, 'e> =
        let folder acc x =
            match acc with
            | Error e -> Error e
            | Ok xs -> f x |> Result.map (fun y -> y :: xs)
        items |> List.fold folder (Ok []) |> Result.map List.rev

    let foldM (f: 's -> 'a -> Result<'s, 'e>) (state: 's) (items: 'a list) : Result<'s, 'e> =
        items |> List.fold (fun acc x -> Result.bind (fun s -> f s x) acc) (Ok state)

    let require (condition: bool) (error: 'e) : Result<unit, 'e> = if condition then Ok() else Error error

    let isOk = function Ok _ -> true | Error _ -> false

module Text =
    let isUuid (s: string) =
        s.Length = 36
        && s = s.ToLowerInvariant()
        && (match System.Guid.TryParseExact(s, "D") with | true, _ -> true | _ -> false)

    /// RFC 3339 UTC with exactly three fractional digits, e.g. 2026-10-22T12:00:00.000Z.
    let isTimestamp (s: string) =
        s.Length = 24
        && s.EndsWith "Z"
        && (match System.DateTime.TryParseExact(s, "yyyy-MM-dd'T'HH:mm:ss.fff'Z'", System.Globalization.CultureInfo.InvariantCulture, System.Globalization.DateTimeStyles.AdjustToUniversal) with
            | true, _ -> true
            | _ -> false)

    let timestamp (t: System.DateTime) = t.ToUniversalTime().ToString("yyyy-MM-dd'T'HH:mm:ss.fff'Z'", System.Globalization.CultureInfo.InvariantCulture)
