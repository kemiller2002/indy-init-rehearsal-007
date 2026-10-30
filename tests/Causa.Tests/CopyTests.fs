/// PAT-14 (PERSIST-053) and PERSIST-032: interface copy names the real cause and never claims persistence falsely.
module Causa.Tests.CopyTests

open System.IO
open System.Text.RegularExpressions
open Expecto
open Causa.Domain.FileSet
open Causa.Persistence
open Causa.App

let private files = { Manifest = "m"; Events = "e"; Snapshot = "s" }
let private c = Confirmation.ofReadBack "0123456789012345678901234567890123456789" files files "2026-10-29T23:00:00.000Z" |> Result.defaultWith failwith

let causes = [ RepositoryKeptChanging; CredentialRefused; PermissionDenied; RateLimited; Rejected "A path is not valid"; ReadBackMismatch; HistoryRewritten ]

let states = [ Saved c; Unsaved; Saving; Offline; Conflict; UnknownOutcome ] @ (causes |> List.map SaveFailed)

let forbiddenWords = Regex(@"\b(saved|synced|stored|safe)\b", RegexOptions.IgnoreCase)
let forbiddenPhrases = [ "saved locally"; "saved on this device"; "queued"; "will be saved later"; "synced"; "backed up" ]

let private session state : App.Session =
    { Id = "id"; Load = App.Ready; BaseCommit = Confirmation.commit c; BaseFiles = Some files; BaseState = None; Current = None
      Unsaved = []; Sent = 0; Save = state; Conflict = None; Refused = []; Fault = None }

let private indicatorOf state = View.indicator (App.init "s") (session state)

let tests =
    testList "PAT-14 copy" [
        for state in states do
            test (sprintf "%s copy makes no false persistence claim (%s)" (SaveState.name state) (match state with SaveFailed c -> View.causeText c | _ -> "")) {
                let i = indicatorOf state
                Expect.equal i.Name (SaveState.name state) "the indicator names the machine's state"
                if SaveState.name state <> "Saved" then Expect.isFalse (forbiddenWords.IsMatch i.Text) (sprintf "no forbidden word in: %s" i.Text)
                for phrase in forbiddenPhrases do Expect.isFalse (i.Text.ToLowerInvariant().Contains phrase) phrase
            }
        for cause in causes do
            test (sprintf "Save failed names its cause (%A)" cause) {
                let i = indicatorOf (SaveFailed cause)
                Expect.stringContains i.Text (View.causeText cause) "the message is the cause the machine determined"
                Expect.stringContains i.Text "kept in this tab" "says the changes are kept"
            }
        test "Unsaved and Offline say the change exists only in this tab" {
            for state in [ Unsaved; Offline ] do Expect.stringContains (indicatorOf state).Text "exist only in this tab" (SaveState.name state)
        }
        test "Retry and Check again appear only when they act" {
            Expect.isFalse (indicatorOf UnknownOutcome).CanCheckAgain "no Check again while automatic reads remain"
            Expect.isFalse (indicatorOf (SaveFailed CredentialRefused)).CanRetry "a refused credential needs Reconnect, not Retry"
            Expect.isTrue (indicatorOf (SaveFailed CredentialRefused)).CanReconnect "Reconnect"
        }
        test "GitHub's secondary rate limits are rate limits, not permission refusals" {
            let forbidden m = GitHub.Answered(200, Causa.Domain.Json.JObject [ "data", Causa.Domain.Json.JNull; "errors", Causa.Domain.Json.JArray [ Causa.Domain.Json.JObject [ "type", Causa.Domain.Json.JString "FORBIDDEN"; "message", Causa.Domain.Json.JString m ] ] ])
            Expect.equal (GitHub.classifyCommit (forbidden "was submitted too quickly")) GitHub.RateLimit "submitted too quickly"
            Expect.equal (GitHub.classifyCommit (GitHub.Answered(403, Causa.Domain.Json.JObject [ "message", Causa.Domain.Json.JString "You have exceeded a secondary rate limit." ]))) GitHub.RateLimit "secondary rate limit"
            Expect.equal (GitHub.classifyCommit (forbidden "Resource not accessible by personal access token")) GitHub.PermissionRefused "a real permission refusal stays one"
        }
        test "page copy has no forbidden persistence phrase" {
            let page = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "../../web/index.html")).ToLowerInvariant()
            for phrase in forbiddenPhrases do Expect.isFalse (page.Contains phrase) phrase
        }
    ]
