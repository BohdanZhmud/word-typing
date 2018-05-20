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

type Status =
  | NotStarted
  | Playing
  | EndSuccess
  | EndFail

type GameState = {
  words: Word list
  status: Status
}

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
  { words = words'; status = NotStarted }

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
    let words' = List.skip 1 words
    List.append [word'] words'

let drawText text color font x y =
  let centerX = getCenterPositionX x text
  Win.drawText text color font (centerX, y)

let renderLoopInterval = int(1000. / 60.)

let rec render (w, h) game () =
  let handleNotStarted draw =
    if Keyboard.anyKeyPressed()
        then window.setTimeout(render(w, h) {game with status = Playing}, renderLoopInterval) |> ignore
        else
          Win.clear()
          draw()
          let game = initFromStorage (w, h)
          window.setTimeout(render (w, h) game, renderLoopInterval) |> ignore

  let drawNotStarted () = drawText "Press any key to start" blackColor font (w/2.) (h/2.)
  let drawSuccess () = drawText "Success. Press any key to start" "green" font (w/2.) (h/2.)
  let drawFail () = drawText "Fail. Press any key to start" "red" font (w/2.) (h/2.)

  match game.status with
  | NotStarted ->
    handleNotStarted drawNotStarted
  | Playing ->
    match game.words with
    | words when List.exists (fun x -> x.y > h) words ->
      render (w, h) { game with status = EndFail } ()
    | _ ->
      let words =
        game.words
        |> handleTyping
        |> List.map move
        |> List.filter (fun x -> ((displayText x).Length <> 0))
      match words with
        | [] -> render (w, h) { game with status = EndSuccess } ()
        | words ->
          Win.clear()
          List.iter (fun word ->
            let fontColor = color word
            let text = displayText word
            drawText text fontColor font word.x word.y)
            words
          window.setTimeout(render (w, h) { game with words = words }, renderLoopInterval) |> ignore
  | EndSuccess -> 
    Win.clear()
    drawSuccess()
  | EndFail -> 
    Win.clear()
    drawSuccess()

let game = initFromStorage (w, h)
render (w, h) game ()