module Game

open Elmish

open Win

open Fable.Import.Browser
open Utils

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Fable.PowerPack
open Shared

type Word = {
  x: float
  y: float
  text: string
  typedCounter: int
}

type GameState = {
  initialWords: string list
  initialWordsCount: int
  words: Word list
  currentRound: int
  score: float
  id: string
  framesPerSecond: int
  speed: float
}

type Game =
  | NotStarted
  | Loading
  | Loaded of Result<string list, exn>
  | Playing of GameState
  | EndSuccess of GameState
  | EndFail of GameState

type Msg =
  | LoadGame of int * float
  | StartGame of Result<GameState, exn>
  | GameStarted of Game
  | Finish of Game

type Model = Game

let fontSize = 30.

// taken from bulma css
let blackColor = "#363636"
let color = function
  | w when w.typedCounter <> 0 -> "red"
  | _ -> blackColor

// taken from bulma css
let fontFamily = "Segoe UI,Roboto,Oxygen,Ubuntu,Cantarell,Fira Sans,Droid Sans,Helvetica Neue,Helvetica,Arial,sans-serif"
let font = sprintf "%ipx %s" (int(fontSize)) fontFamily

let displayText word = word.text.Substring(word.typedCounter, word.text.Length - word.typedCounter)
let move (w, h) game =
  let margin = h * game.speed
  let words = 
    game.words 
    |> List.map (fun word -> { word with y = word.y + margin})
    |> List.filter (fun x -> ((displayText x).Length <> 0))
  { game with words = words }

let getCenterPositionX x (text: string) =
  let centerTextMargin = float(text.Length) / 4. * fontSize
  x - centerTextMargin

let handleTyping game =
  match game.words with
  | [] -> game
  | words ->
    let word = List.head words
    let text = displayText word
    let increment = if Keyboard.keyPresed (text.ToUpperInvariant()) then 1 else 0
    if increment = 1 then do Keyboard.clear()
    let word' = { word with typedCounter = word.typedCounter + increment }
    { game with words = word' :: (List.tail words); score = game.score + float(increment) }

let drawText text color x y =
  let centerX = getCenterPositionX x text
  Win.drawText text color font (centerX, y)

let drawScore (w, h) game =
  let passed = game.initialWordsCount - List.length game.words
  let total = game.initialWordsCount
  let text = sprintf "%i/%i" passed total
  let y = fontSize * 2.;
  let x = w - float(text.Length) * fontSize - fontSize * 5.
  do Win.drawText text blackColor font (x, y)

  let text' = sprintf "Round: %i" game.currentRound
  let x' = fontSize * 5. + float(text'.Length)
  do Win.drawText text' blackColor font (x', y)

let rec render (w, h) game (framesPerSecond: int) dispatch () =
  let scheduleRender game' =
    window.setTimeout(render (w, h) game' framesPerSecond dispatch, int(1000. / float(framesPerSecond))) |> ignore

  match game with
  | Playing game ->
    match game.words with
    | words when List.exists (fun x -> x.y > h) words ->
      Keyboard.clear()
      scheduleRender (EndFail game)
    | _ ->
      let game' =
        game
        |> handleTyping
        |> move (w, h)
      match game'.words with
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
  | EndFail game -> dispatch (Finish (EndFail game))
  | _ -> ()

let w, h = Win.dimensions()
do Keyboard.init()

let init () : Model * Cmd<Msg> =
  NotStarted, Cmd.none

let initGame (round: Round) score =
  let words' = round.words |> List.map (fun x ->
          let rand = random (w/3.) (2. * w / 3.)
          { x = rand; y  = 0. + fontSize; text = x; typedCounter = 0})
  let maxVisibleWords = 5.
  let margin = h / maxVisibleWords

  let words'' = words' |> List.mapi (fun i word -> {word with y = 0. - float(i) * margin})
  {
    words = words''
    initialWords = round.words
    initialWordsCount = List.length round.words
    currentRound = round.number
    score = score
    id = string (System.Guid.NewGuid())
    framesPerSecond = round.framesPerSecond
    speed = round.speed
  }

let update msg model =
  match msg with
  | LoadGame (r, s) ->
    let promise _ =
      Fetch.fetchAs<Round>(sprintf "/api/round/%i" r) []
      |> Promise.map (fun x -> initGame x s)
    model, Cmd.ofPromise promise [] (Ok >> StartGame) (Error >> StartGame)
  | StartGame (Ok game) ->
    let sub dispatch =
      do render (w, h) (Playing game) game.framesPerSecond dispatch () 
    Playing game, Cmd.ofSub sub
  | StartGame (Error _) ->
    model, Cmd.none
  | GameStarted game -> game, Cmd.none
  | Finish gameState -> gameState, Cmd.none

let getText model =
  match model with
  | EndSuccess game -> sprintf "Round %i successfully cleared. Current score: %i. Press ENTER to get to next round." game.currentRound (int(game.score))
  | EndFail game -> sprintf "Round %i. Game over. Current score: %i. Press ENTER to start again." game.currentRound (int(game.score))
  | NotStarted _ -> "Press ENTER to start."
  | Loading -> "Loading game..."
  | Loaded (Error _) -> "Failed to load game. Please try again."
  | _ -> ""

let containerStyle = 
    Style [
        Display "flex"
        JustifyContent "center"
        AlignItems "center"
        FlexDirection "column"
    ]

let startBtnId = "start-btn";
let getRound game =
  match game with
  | EndSuccess game -> game.currentRound + 1
  | Playing game -> game.currentRound
  | _ -> 1
let getScore game =
  match game with
  | Playing game | EndSuccess game -> game.score
  | _ -> 0.

let view (model : Game) (dispatch : Msg -> unit) =
  let round = getRound model
  let score = getScore model
  match model with
  | EndSuccess _ | EndFail _ | NotStarted _ | Loading | Loaded (Error _)->
    div [containerStyle]
      [ span [ Class "is-size-4" ] [
          str (getText model)
        ];
       button [ Id startBtnId; Class "button is-success"; OnClick (fun _ -> dispatch (LoadGame (round, score)))] [str "Start"]]
  | _ -> str ""

document.addEventListener_keydown(fun e ->
  let enter = 13.
  if (e.keyCode = enter) then
    let btn = document.getElementById(startBtnId)
    btn.click()
  )