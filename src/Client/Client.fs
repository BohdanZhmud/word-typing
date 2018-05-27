module Client

open Elmish
open Elmish.React

open Win
open Keyboard

open Fable.Import.Browser
open GameData
open Utils

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Fulma
open Fable.PowerPack

type Word = {
  x: float
  y: float
  text: string
  typedCounter: int
}

type GameState = {
  words: Word list
  initialWordsCount: int
}

type Game =
  | NotStarted
  | Playing of GameState
  | EndSuccess of GameState
  | EndFail of GameState

type Msg =
  | GameStarted of Game
  | StartGame
  | Finish of Game

let fontSize = 30.
let initFromStorage (w, h) =
  let words =
    GameData.firstRoundWords()
    |> Utils.shuffleList
    |> List.take 20
    |> List.map (fun x ->
      let rand = Utils.random (w/3.) (2. * w / 3.)
      { x = rand; y  = 0. + fontSize; text = x; typedCounter = 0})
  let maxVisibleWords = 5.
  let margin = h / maxVisibleWords

  let words' =
    words
    |> List.mapi (fun i word -> {word with y = 0. - float(i) * margin})
  { words = words'; initialWordsCount = List.length words }

// taken from bulma css
let blackColor = "#363636"
let color = function
  | w when w.typedCounter <> 0 -> "red"
  | _ -> blackColor

// taken from bulma css
let fontFamily = "Segoe UI,Roboto,Oxygen,Ubuntu,Cantarell,Fira Sans,Droid Sans,Helvetica Neue,Helvetica,Arial,sans-serif"
let font = sprintf "%ipx %s" (int(fontSize)) fontFamily

let move word = { word with y = word.y + 3.}

let displayText word = word.text.Substring(word.typedCounter, word.text.Length - word.typedCounter)

let getCenterPositionX x (text: string) =
  let centerTextMargin = float(text.Length) / 4. * fontSize
  x - centerTextMargin

let handleTyping words =
  match words with
  | [] -> words
  | words ->
    let word = List.head words
    let text = displayText word
    let increment = if Keyboard.keyPresed (text.ToUpperInvariant()) then 1 else 0
    let word' = { word with typedCounter = word.typedCounter + increment }
    word' :: (List.tail words)

let drawText text color x y =
  let centerX = getCenterPositionX x text
  Win.drawText text color font (centerX, y)

let drawScore (w, h) game =
  let passed = game.initialWordsCount - List.length game.words
  let total = game.initialWordsCount
  let text = sprintf "%i/%i" passed total
  let y = fontSize * 2.;
  let x = w - float(text.Length) * fontSize - fontSize * 5.
  Win.drawText text blackColor font (x, y)

let renderLoopInterval = int(1000. / 60.)

let rec render (w, h) game dispatch () =
  let scheduleRender game' = window.setTimeout(render (w, h) game' dispatch, renderLoopInterval) |> ignore

  match game with
  | Playing game ->
    match game.words with
    | words when List.exists (fun x -> x.y > h) words ->
      Keyboard.clear()
      scheduleRender (EndFail game)
    | _ ->
      let words =
        game.words
        |> handleTyping
        |> List.map move
        |> List.filter (fun x -> ((displayText x).Length <> 0))
      let game' = { game with words = words }
      match words with
        | [] ->
          Keyboard.clear()
          scheduleRender (EndSuccess game')
        | words ->
          Win.clear()
          List.iter (fun word ->
            let fontColor = color word
            let text = displayText word
            drawText text fontColor word.x word.y)
            words
          scheduleRender (Playing game')
      drawScore (w, h) game
  | EndSuccess game -> dispatch (Finish (EndSuccess game))
  | EndFail game -> dispatch (Finish (EndSuccess game))
  | NotStarted -> ()

let w, h = Win.dimensions()
do Keyboard.init()

let timer initial =
    let sub dispatch = 
      do render (w, h) initial dispatch ()
    Cmd.ofSub sub

let init () : Game * Cmd<Msg> =
  NotStarted, Cmd.none

let update msg model =
  match msg with
  | GameStarted game -> game, Cmd.none
  | StartGame ->
    let sub dispatch =
      let game = initFromStorage (w, h)
      do render (w, h) (Playing game) dispatch ()
      dispatch (GameStarted (Playing game))
    model, Cmd.ofSub sub
  | Finish game -> game, Cmd.none

let endText model =
  match model with
  | EndSuccess _ -> "Success. Press ENTER to continue."
  | EndFail _ -> "Fail. Press ENTER to start again."
  | NotStarted _ -> "Press ENTER to start."
  | _ -> ""

let containerStyle = 
    Style [
        Width "100wh"
        Height "100vh"
        Display "flex"
        JustifyContent "center"
        AlignItems "center"
        FlexDirection "column"
    ]

let view (model : Game) (dispatch : Msg -> unit) =
  match model with
  | EndSuccess _ | EndFail _ | NotStarted _ ->
    div [containerStyle]
      [ span [ ClassName "is-size-4" ] [
          str (endText model)
        ];
       button [ ClassName "button is-success"; OnClick (fun _ -> dispatch StartGame); HTMLAttr.Type "submit"] [str "Start"]]
  | _ -> str ""

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.withSubscription timer
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run