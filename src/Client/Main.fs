module Main

open Elmish

open Fable.Helpers.React
open Fable.Helpers.React.Props

type Model = {
  game: Game.Model
  rating: Rating.Model
  user: User.Model
}

type Msg = 
  | GameMsg of Game.Msg
  | RatingMsg of Rating.Msg
  | UserMsg of User.Msg

let init () : Model * Cmd<Msg> =
  let game, gameCmd = Game.init()
  let rating, ratingCmd = Rating.init()
  let user, userCmd = User.init()
  { game = game; rating = rating; user = user }, Cmd.batch [ 
     Cmd.map GameMsg gameCmd
     Cmd.map RatingMsg ratingCmd
     Cmd.map UserMsg userCmd
    ]

let update msg model : Model * Cmd<Msg> =
  match msg with
  | GameMsg msg' ->
    let res, cmd = Game.update msg' model.game
    let cmd' = 
      match msg' with
      | Game.Finish game ->
        match game with
        | Game.EndSuccess game' | Game.EndFail game' ->
          let storeResultCmd = Cmd.ofMsg (Rating.StoreResult (game'.score, model.user.id))
          Cmd.map RatingMsg storeResultCmd
        | _ -> Cmd.none
      | _ -> Cmd.none
    { model with game = res }, Cmd.batch [Cmd.map GameMsg cmd; cmd']
  | RatingMsg msg' ->
    let res, cmd = Rating.update msg' model.rating
    { model with rating = res }, Cmd.map RatingMsg cmd
  | UserMsg msg' ->
    let res, cmd = User.update msg' model.user
    { model with user = res }, Cmd.map UserMsg cmd

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

let view (model : Model) (dispatch : Msg -> unit) =
  match model.game with
  | Game.Playing _ -> str ""
  | _ ->
    div [containerStyle] [
      div [top] [ 
        Game.view model.game (GameMsg >> dispatch)
      ]
      div [bottom] [
        Rating.view model.rating model.user (RatingMsg >> dispatch)
      ]
    ]