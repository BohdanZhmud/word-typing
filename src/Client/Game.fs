module Game

open Shared
open Elmish

open Fable.Import.Browser
open Utils

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Fable.PowerPack
open Shared
open System

type Word = {
  x: float
  y: float
  text: string
  typedCounter: int
}

type GameState = {
  initialWords: Word list
  initialWordsCount: int
  words: Word list

  currentMaxVisibleWords: float
  desiredMaxVisibleWords: float

  desirableLettersCount: float
  currentLettersCount: float

  currentRound: int
  totalScore: float
  scoreForCurrentRound: float
  id: string
  framesPerSecond: int
  speed: float
  speedUpdated: DateTimeOffset
  minimumSpeed: float
  gameType: GameType
  keyPressedTimestamps: int64 list
  data: Data
  started: DateTimeOffset
}

type Game =
  | NotStarted
  | Loading
  | Loaded of Result<string list, exn>
  | Playing of GameState
  | EndSuccess of GameState
  | EndFail of GameState


type Msg =
  | LoadGame of int * float * string * GameType
  | StartGame of Result<GameState, exn>
  | GameStarted of Game
  | Finish of Game

  | StoreScore of GameReplay
  | StoredScore of Result<float, exn>

type Model = Game

let fontSize = 30.

// taken from bulma css
let blackColor = "#363636"
let color = function
  | w when w.typedCounter <> 0 -> "red"
  | _ -> blackColor
let fontFamily = "Segoe UI,Roboto,Oxygen,Ubuntu,Cantarell,Fira Sans,Droid Sans,Helvetica Neue,Helvetica,Arial,sans-serif"
let font = sprintf "%ipx %s" (int(fontSize)) fontFamily

let popWords data length count =
  let words, data =
    match length with
    | 3 -> (data.threeLettersWords |> List.take count, { data with threeLettersWords = data.threeLettersWords |> List.skip count })
    | 4 -> (data.fourLettersWords |> List.take count, { data with fourLettersWords = data.fourLettersWords |> List.skip count }) 
    | 5 -> (data.fiveLettersWords |> List.take count, { data with fiveLettersWords = data.fiveLettersWords |> List.skip count })
    | 6 -> (data.fiveLettersWords |> List.take count, { data with  fiveLettersWords = data.fiveLettersWords |> List.skip count })
    | 7 -> (data.sevenLettersWords |> List.take count, { data with sevenLettersWords = data.sevenLettersWords |> List.skip count })
    | _ -> failwith "incorrect length"
  (words, data)

let pushWords data words l =
  match l with
  | 3 -> { data with threeLettersWords = words @ data.threeLettersWords }
  | 4 -> { data with fourLettersWords = words @ data.fourLettersWords }
  | 5 -> { data with fiveLettersWords = words @ data.fiveLettersWords }
  | 6 -> { data with fiveLettersWords = words @ data.fiveLettersWords }
  | 7 -> { data with sevenLettersWords = words @ data.sevenLettersWords }
  | _ -> failwith "incorrect length"

let initWords margin =
  List.map (fun x ->
    let rand = random (1./3.) (2./3.) // >= 1/3 and <= 2/3 of screen
    { x = rand; y  = 0.; text = x; typedCounter = 0 })
  >> List.mapi (fun i word -> { word with y = 0. - float(i) * margin })

let displayText word = word.text.Substring(word.typedCounter, word.text.Length - word.typedCounter)
let move game =
  let margin = game.speed
  let notTyped = displayText >> String.length >> (=) 0 >> not
  let notTypedOnly =
    game.words
    |> List.map (fun word -> { word with y = word.y + margin})
    |> List.filter notTyped

  let splitByVisibility words =
    let isVisible word = word.y > 0.
    let index = words |> List.tryFindIndex (isVisible >> not)
    match index with
    | Some x -> words |> List.splitAt x
    | None -> (words, [])

  let minVisibleWords = 3.
  let maxVisibleWords = 5.
  let desiredMaxVisibleWords = game.desiredMaxVisibleWords |> inRange minVisibleWords maxVisibleWords

  let minLettersCount = 3
  let maxLettersCount = 7
  let desirableLettersCount = game.desirableLettersCount |> int |> inRange minLettersCount maxLettersCount
  let currentLettersCount = game.currentLettersCount |> int
  let words', data =
    match notTypedOnly with
    | [] ->
      let words, data = popWords game.data desirableLettersCount 5
      (initWords (1. / desiredMaxVisibleWords) words, data)
    | _ ->
      let words, data = 
        let visible, notVisible = notTypedOnly |> splitByVisibility
        let texts = notVisible |> List.map (fun x -> x.text)
        let data' = pushWords game.data texts currentLettersCount
        let newTexts, data'' = popWords data' desirableLettersCount (List.length notVisible)

        let notVisible' = List.zip notVisible newTexts |> List.map (fun (x, y) -> { x with text = y })
        (visible @ notVisible', data'')

      let words' =
        let visible, notVisible = words |> splitByVisibility
        let margin = 1. / desiredMaxVisibleWords - 1. / game.currentMaxVisibleWords
        visible @ (notVisible |> List.map (fun x -> { x with y = min (x.y - margin) 0.}))
      (words', data)

  let head = words' |> List.head
  let margin = if (head.y < 0.) then -head.y else 0.
  let words'' = words' |> List.map (fun x -> { x with y = x.y + margin })
  { game with words = words''; data = data; currentLettersCount = desirableLettersCount |> float; currentMaxVisibleWords = desiredMaxVisibleWords }

let getCenterPosition x (text: string) =
  let centerTextMargin = float(text.Length) / 4. * fontSize
  x - centerTextMargin

let handleTyping game =
  match game.words with
  | [] -> game
  | words ->
      let word = List.head words
      let text = displayText word
      let pressedKey = text.ToUpperInvariant() |> Keyboard.keyPresed
      let increment =
          match pressedKey with
          | Some _ ->
              do Keyboard.clear()
              1
          | None _ -> 0
      let keyPressedTimestamps =
          match pressedKey with
          | Some timestamp -> game.keyPressedTimestamps @ [timestamp]
          | None -> game.keyPressedTimestamps
      let userSpeed last first count =
        let difference = (last - first) / float(TimeSpan.TicksPerMillisecond)
        count / difference

      let (speed, maxVisibleWords, lettersCount, updated) =
        match (keyPressedTimestamps, game.speedUpdated) with
        | (_, updated) when (updated > DateTimeOffset.UtcNow.Subtract(TimeSpan.FromMilliseconds(100.))) -> (game.speed, game.desiredMaxVisibleWords, game.desirableLettersCount, game.speedUpdated)
        | ([], _) | ([_], _) -> (game.speed, game.desiredMaxVisibleWords, game.desirableLettersCount, game.speedUpdated)
        | _ ->
          let keyPressedTimestamps' = keyPressedTimestamps |> lastN 20
          let first = keyPressedTimestamps' |> List.head |> float
          let last = keyPressedTimestamps' |> List.last |> float
          let userSpeed =
            min
              (userSpeed last first (float(keyPressedTimestamps'.Length)))
              (userSpeed (float(DateTimeOffset.UtcNow.Ticks)) first (float(keyPressedTimestamps'.Length + 1)))
          let currentSpeed = game.desirableLettersCount / ((1000 / game.framesPerSecond |> float) / game.speed)
          let wordsNumber = game.words.Length |> float
          let margin, last =
            match game.words with
            | [] -> (0., 0.)
            | [head] -> (1., (1. - head.y))
            | head :: tail -> (head.y, tail |> List.map (fun x -> x.y) |> List.last |> abs)
          let minumumRequiredSpeed = currentSpeed / ((1. + last - margin) / wordsNumber)

          // let maxSpeed = 0.001
          // let minSpeed = 0.00003
          let changePercentage =  userSpeed / minumumRequiredSpeed |> pow (1. / 3.)

          let desiredVisibleWords =
            game.desiredMaxVisibleWords * changePercentage

          let desiredLettersCount =
            game.desirableLettersCount * changePercentage

          (game.speed * changePercentage, desiredVisibleWords, desiredLettersCount, DateTimeOffset.UtcNow)

      let word' = { word with typedCounter = word.typedCounter + increment }
      { game with
          words = word' :: (List.tail words)
          scoreForCurrentRound = game.scoreForCurrentRound + float(increment)
          keyPressedTimestamps = keyPressedTimestamps
          speed = speed
          minimumSpeed = game.speed
          desiredMaxVisibleWords = maxVisibleWords
          desirableLettersCount = lettersCount }

let drawText (w, h) (x, y) text color =
  let absoluteX = w * x
  let centerAbsoluteX = getCenterPosition absoluteX text
  let absoluteY = h * y
  Win.drawText text color font (centerAbsoluteX, absoluteY)

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
 
let rec render game (framesPerSecond: int) dispatch () =
  let w, h = Win.dimensions()

  let scheduleRender game' =
    window.setTimeout(render game' framesPerSecond dispatch, 1000 / framesPerSecond) |> ignore

  match game with
  | Playing game ->
    match game.words with
    | words when List.exists (fun x -> x.y > (1.)) words ->
      Keyboard.clear()
      scheduleRender (EndFail game)
    | _ ->
      let game' =
        game
        |> handleTyping
        |> move
      match game'.words with
        | [] ->
          Keyboard.clear()
          scheduleRender (EndSuccess game')
        | words ->
          Win.clear()
          List.iter (fun word ->
            let fontColor = color word
            let text = displayText word
            drawText (w, h) (word.x, word.y) text fontColor)
            words
          scheduleRender (Playing game')
      drawScore (w, h) game
  | EndSuccess game -> dispatch (Finish (EndSuccess game))
  | EndFail game -> dispatch (Finish (EndFail game))
  | _ -> ()


do Keyboard.init()

let init () : Model * Cmd<Msg> =
  NotStarted, Cmd.none

let initGame (data: Data) score gameId gameType =
  let words, data = popWords data 3 5 

  let maxVisibleWords = 3.
  let margin = 1. / maxVisibleWords
  let words' = initWords margin words
  printfn "init"
  {
    words = words'
    initialWords = words'
    initialWordsCount = 100
    currentMaxVisibleWords = maxVisibleWords
    desiredMaxVisibleWords = maxVisibleWords
    desirableLettersCount = 3.
    currentLettersCount = 3.
    currentRound = 0
    scoreForCurrentRound = 0.
    totalScore = score
    id = gameId
    framesPerSecond =  50 //round.framesPerSecond
    speed = 2. / 1000.
    speedUpdated = DateTimeOffset.UtcNow
    minimumSpeed = 2. / 1000.
    gameType = gameType
    keyPressedTimestamps = []
    data = data
    started = DateTimeOffset.UtcNow
  }

let update msg model =
  match msg with
  | LoadGame (round, score, gameId, gameType) ->
    match model with
    | Loading -> model, Cmd.none
    | _ ->
      let promise _ =
        Fetch.fetchAs<Data>("/api/game/data") []
        |> Promise.map (fun x -> initGame x score gameId gameType)
      model, Cmd.ofPromise promise [] (Ok >> StartGame) (Error >> StartGame)
  | StartGame (Ok game) ->
    let sub dispatch =
      do render (Playing game) game.framesPerSecond dispatch () 
    Playing game, Cmd.ofSub sub
  | StartGame (Error _) -> model, Cmd.none
  | GameStarted game -> game, Cmd.none
  | Finish gameState -> gameState, Cmd.none 
  | StoreScore gameReplay when gameReplay.score.value > 0. ->
      let cmd =
        Cmd.ofPromise
          (fun _ -> promise {
            let! res = Fetch.postRecord "api/game/score" gameReplay []
            let! text = res.text()
            return float(text)
          })
          ()
          (Ok >> StoredScore)
          (Error >> StoredScore)
      model, cmd
  | StoredScore (Ok score) ->
    let model' = 
      match model with
      | EndSuccess game -> EndSuccess ({ game with totalScore = score })
      | EndFail game -> EndFail ({ game with totalScore = score })
      | _ -> model
    model', Cmd.none
  | _ -> model, Cmd.none

let getText model =
  match model with
  | EndSuccess game -> sprintf "Round %i successfully cleared. Current score: %i. Press ENTER to get to next round." game.currentRound (int(game.totalScore))
  | EndFail game -> sprintf "Round %i. Game over. Current score: %i. Press ENTER to start again." game.currentRound (int(game.totalScore))
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

let buttonsContainerStyle =
  Style [
    Display "flex"
  ]

let startBtnId = "start-btn"
let getRound game =
  match game with
  | EndSuccess game -> game.currentRound + 1
  | Playing game -> game.currentRound
  | _ -> 1
let getScore game =
  match game with
  | Playing game | EndSuccess game -> game.totalScore
  | _ -> 0.

let startButtonStyle =
  Style [
    Margin "10px"
  ]
let startGameButton model gameType round text dispatch id className =
  let score = getScore model
  let gameId =
    match model with
    | EndSuccess game | EndFail game -> game.id
    | _ -> string (System.Guid.NewGuid())
  button [ Id id;
           Class className;
           startButtonStyle
           OnClick (fun _ -> dispatch (LoadGame (round, score, gameId, gameType)))]
         [str text]

let getStartGameButtons model dispatch =
  let startNormalGameButton id className text = startGameButton model UsualGame (getRound model) text dispatch id className
  let restartLastRoundButton game id =
    let text = sprintf "Restart last round (-%i%% score)" (int(Constants.percentageChargeForRestart * 100.))
    startGameButton model RestartLastRound game.currentRound text dispatch id "button is-success"
  match model with
  | EndFail game when game.currentRound <> 1 ->
    [ restartLastRoundButton game startBtnId; startNormalGameButton "" "button" "Start new" ]
  | EndSuccess _ | EndFail _ | NotStarted | Loading | Loaded (Error _) ->
    [ startNormalGameButton startBtnId "button is-success" "Start"]
  | _ -> [ str "" ]

let view (model : Game) (dispatch : Msg -> unit) =
  let buttons = getStartGameButtons model dispatch
  match model with
  | EndSuccess _ | EndFail _ | NotStarted | Loading | Loaded (Error _) ->
    div [containerStyle]
      [ span [ Class "is-size-4" ] [ str (getText model) ];
        div [buttonsContainerStyle] buttons]
  | _ -> str ""

document.addEventListener_keydown(fun e ->
  let enter = 13.
  if (e.keyCode = enter) then
    let btn = document.getElementById(startBtnId)
    btn.click()
  )
