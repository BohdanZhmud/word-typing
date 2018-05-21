module Client

open Win
open Keyboard

open Fable.Import.Browser
open GameData

let w, h = Win.dimensions()
Keyboard.init()

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

let random max min = Fable.Import.JS.Math.random() * (max - min) + min;

let fontSize = 30.
let initFromStorage (w, h) =
  let words =
    GameData.getWords()
    |> List.map (fun x ->
      let rand = random (w/3.) (2. * w / 3.)
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

let rec render (w, h) game () =
  let scheduleRender game' = window.setTimeout(render (w, h) game', renderLoopInterval) |> ignore

  let drawNotStarted () = drawText "Press any key to start" blackColor (w/2.) (h/2.)
  let drawSuccess game () = 
    drawText "Success. Press any key to start again" "green" (w/2.) (h/2.)
    drawScore (w, h) game
  let drawFail game () = 
    drawText "Game over. Press any key to start again" "red" (w/2.) (h/2.)
    drawScore (w, h) game

  let handleNotStarted draw game =
    Win.clear()
    draw()
    if Keyboard.anyKeyPressed() 
      then
        Keyboard.clear()
        let game = initFromStorage (w, h)
        scheduleRender (Playing game)
      else
        scheduleRender game

  match game with
  | NotStarted -> handleNotStarted drawNotStarted game
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
  | EndSuccess game -> handleNotStarted (drawSuccess game) (EndSuccess game)
  | EndFail game -> handleNotStarted (drawFail game) (EndFail game)
let game = initFromStorage (w, h)
render (w, h) NotStarted ()