/// The Limen engine entry point. The model lives in this one cell; everything else is pure.
module Causa.App.Engine

open System
open Causa.Domain
open Causa.App

let private context : Engine.Context =
    { NewId = fun () -> Guid.NewGuid().ToString("D")
      Now = fun () -> Text.timestamp DateTime.UtcNow
      Actor = { ActorId = "causa-user"; DisplayName = None } }

let private env : App.Env = { Context = context; Nonce = fun () -> Guid.NewGuid().ToString("N").Substring(0, 12) }

/// A random identifier for this tab's page load, kept only in memory (PERSIST-014).
let private model = ref (App.init (Guid.NewGuid().ToString("N").Substring(0, 16)))

let step (e: App.Env) (m: App.Model) (message: string) : App.Model * string =
    let next, effects = App.update e m (Limen.decode message)
    next, Limen.encode (View.render e next) effects

let dispatch (message: string) : string =
    let next, response = step env model.Value message
    model.Value <- next
    response
