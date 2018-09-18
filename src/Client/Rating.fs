module Rating

open Shared
open Elmish
open Fable.PowerPack
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Utils

type Model = RatingEntry list option

type Msg =
    | Loading
    | Loaded of Result<RatingEntry list, exn>

let init () : Model * Cmd<Msg> =
  None, Cmd.ofMsg Loading

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
  match model, msg with
    | None, Loaded (Ok x) -> Some x, Cmd.none
    | _, Loading ->
      let cmd =
        Cmd.ofPromise
          (Fetch.fetchAs<RatingEntry list> "api/game/rating")
          []
          (Ok >> Loaded)
          (Error >> Loaded)
      None, cmd
    | _ -> model, Cmd.none

let topResultsStyle =
  Style [
    MarginLeft "1em"
  ]
let view (model : Model) (dispatch : Msg -> unit) =
  match model with
  | None -> str "Loading rating..."
  | Some rating ->
    div [] [
      span [topResultsStyle; Class "is-size-4"] [str "Top Results"]
      table
        [ Class "table" ] [
          tbody []
            (rating |> List.sortByDescending (fun x -> x.score.value) |> List.mapi (fun i x -> 
                tr [] [
                   th [] [ str (i |> increment |> string)]
                   td [] [ str x.userDisplayName ]
                   td [] [ str (string (int(x.score.value))) ]
                ]))
        ]
    ]
