module Main

open Elmish

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable

type Model = {
  game: Game.Model
  rating: Rating.Model
  user: Auth.Model
}

type Msg =
  | GameMsg of Game.Msg
  | RatingMsg of Rating.Msg
  | AuthMsg of Auth.Msg 

let init () : Model * Cmd<Msg> =
  let game, gameCmd = Game.init()
  let rating, ratingCmd = Rating.init()
  let user, userCmd = Auth.init()
  { game = game; rating = rating; user = user }, Cmd.batch [ 
     Cmd.map GameMsg gameCmd
     Cmd.map RatingMsg ratingCmd
     Cmd.map AuthMsg userCmd
    ]

let handleGameMsg msg model =
  let res, cmd = Game.update msg model.game
  let cmd' =
    match msg with
    | Game.Finish game ->
      match game with
      | Game.EndSuccess game' | Game.EndFail game' ->
        match model.user with
        | Some user' ->
          let storeResultCmd = Cmd.ofMsg (Game.StoreScore {
              words = game'.initialWords
              round = game'.currentRound
              score = { userId = user'.id; value = game'.scoreForCurrentRound; gameId = game'.id }
              gameType = game'.gameType
            })
          Cmd.map GameMsg storeResultCmd
        | _ -> Cmd.none
      | _ -> Cmd.none
    | Game.StoredScore (Ok _) -> Cmd.map RatingMsg (Cmd.ofMsg Rating.Loading)
    | _ -> Cmd.none
  { model with game = res }, Cmd.batch [Cmd.map GameMsg cmd; cmd']

let update msg model : Model * Cmd<Msg> =
  match msg with
  | GameMsg msg' -> handleGameMsg msg' model
  | RatingMsg msg' ->
    let res, cmd = Rating.update msg' model.rating
    { model with rating = res }, Cmd.map RatingMsg cmd
  | AuthMsg msg' -> 
    let res, cmd = Auth.update msg' model.user
    { model with user = res }, Cmd.map AuthMsg cmd

let containerStyle = 
  Style [
      Width "100wh"
      Height "100vh"
      Display "flex"
      JustifyContent "center"
      FlexDirection "column"
  ]

let top =
    Style [
        Flex "1"
        Display "flex"
        JustifyContent "center"
        AlignItems "flex-end"
    ]
let bottom = 
    Style [
        Flex "1"
    ]
let topRightCorner =
  Style [
    Position "absolute"
    Top "0"
    Right "0"
  ]

let view (model : Model) (dispatch : Msg -> unit) =
  match model.user with
  | None -> str "Loading..."
  | Some _ ->
    match model.game with
    | Game.Playing _ -> str ""
    | _ ->
      div [containerStyle] [
        div [top] [ 
          Game.view model.game (GameMsg >> dispatch)
        ]
        div [bottom] [
          Rating.view model.rating (RatingMsg >> dispatch)
        ]
        div [topRightCorner] [
          Auth.view model.user (AuthMsg >> dispatch)
        ]
      ]
