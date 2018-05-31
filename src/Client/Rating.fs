module Rating

open Shared
open Elmish
open Fable.PowerPack
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Utils

type Model = Score list option

type Msg =
    | Loading
    | Loaded of Result<Score list, exn>
    | StoreResult of float * string * string

let init () : Model * Cmd<Msg> =
  None, Cmd.ofMsg Loading

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
  match model, msg with
    | None, Loaded (Ok x) -> Some x, Cmd.none
    | _, Loading ->
      let cmd =
        Cmd.ofPromise
          (Fetch.fetchAs<Score list> "api/rating")
          []
          (Ok >> Loaded)
          (Error >> Loaded)
      None, cmd
    | _, StoreResult (res, userId, gameId) when res > 0. ->
      let cmd =
        Cmd.ofPromise
          (Fetch.postRecord "api/rating" { name = userId; value = res; gameId = gameId })
          []
          (fun _ -> Loading)
          (Error >> Loaded)
      model, cmd
    | _ -> model, Cmd.none

let topResultsStyle =
  Style [
    MarginLeft "1em"
  ]
let view (model : Model) (userModel: User.Model) (dispatch : Msg -> unit) =
  match model with
  | None -> str "Loading rating..."
  | Some rating ->
    div [] [
      span [topResultsStyle; Class "is-size-4"] [str "Top Results"]
      table
        [ Class "table" ] [
          tbody []
            (rating |> List.sortByDescending (fun x -> x.value) |> List.mapi (fun i x -> 
                tr [] [
                   th [] [ str (i |> increment |> string)]
                   td [] [ str userModel.displayName ]
                   td [] [ str (string (int(x.value))) ]
                ]))
        ]
    ]
    