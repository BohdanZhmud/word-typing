module Game

open Shared
open Elmish

open Fable.Import.Browser
open Utils

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Fable.PowerPack
open System
open Keyboard

type Coordinates = {
  x: float
  y: float
}

type WordWithText = {
  coordinates: Coordinates
  text: string
  typedCounter: int
}

type Word =
  | NotVisibleWord of Coordinates
  | VisibleWord of WordWithText

type GameState = {
  initialWords: Word list
  initialWordsCount: int
  words: Word list

  currentRound: int
  totalScore: float
  scoreForCurrentRound: float
  id: string  
  framesPerSecond: int
  koefStore: (float * float) list
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
  | LoadData of int * float * string * GameType
  | LoadedData of Result<GameState, exn>
  | GameStarted of Game
  | Finish of Game

  | StoreScore of GameReplay
  | StoredScore of Result<float, exn>
  | Tick
  | KeyPressed of KeyType

type Model = Game

let minSpeed = 0.0015
let maxSpeed = 0.0055
let minVisibleWords = 3.
let maxVisibleWords = 3.125
let minLettersCount = 3.
let maxLettersCount = 9.

let getSpeed game = game.koefStore |> List.last |> fst |> (*) minSpeed |> inRange minSpeed maxSpeed
let getVisibleWords game = game.koefStore |> List.last |> fst |> (*) minVisibleWords |> inRange minVisibleWords maxVisibleWords
let getLettersCount game = game.koefStore |> List.last |> fst |> (*) minLettersCount |> Math.Ceiling |> inRange minLettersCount maxLettersCount

let WORDS_COUNT = 50

let fontSize = 30.

// taken from bulma css
let blackColor = "#363636"
let color = function
  | w when w.typedCounter <> 0 -> "red"
  | _ -> blackColor
let fontFamily = "Segoe UI,Roboto,Oxygen,Ubuntu,Cantarell,Fira Sans,Droid Sans,Helvetica Neue,Helvetica,Arial,sans-serif"
let font = sprintf "%ipx %s" (int(fontSize)) fontFamily

let popTexts data length count =
  match data |> Map.tryFind length with
  | Some value -> 
    let r = if value |> List.length >= count then value |> List.take count |> Some else None
    match r with 
    | Some x -> (Some x, data |> Map.add length (value |> List.skip count))
    | None -> (None, data)
  | None -> (None, data)

let initWords margin count =
  let rand = random (1./3.) (2./3.) // >= 1/3 and <= 2/3 of screen
  { x = rand; y  = 0. }
  |> List.replicate count
  |> List.mapi (fun i word -> { word with y = 0. - float(i) * margin })

let displayText word = word.text.Substring(word.typedCounter, String.length word.text - word.typedCounter)

let filterOutTypedWords game =
  let words = game.words |> List.filter (fun x ->
    match x with
    | NotVisibleWord _ -> true
    | VisibleWord y when y |> displayText |> String.length |> (fun x -> x > 0) -> true
    | VisibleWord _ -> false)
  { game with words = words }

let getY word =
  match word with
  | NotVisibleWord w -> w.y
  | VisibleWord w -> w.coordinates.y

let calculateMinimumRequiredSpeed game =
  let speed = getSpeed game
  let wordsCount = game.words |> List.length |> float
  let currentSpeed = // letters per sec
    match speed with
    | 0. -> 0.
    | _ ->
      let avarageLettersPerWordCount =
        let sum =
          game.words
          |> List.sumBy (fun x -> 
            match x with
            | NotVisibleWord _ -> getLettersCount game
            | VisibleWord w -> String.length w.text - w.typedCounter |> float)
        sum / wordsCount
      avarageLettersPerWordCount / ((1000 / game.framesPerSecond |> float) / speed)
  match currentSpeed with
  | 0. -> 0.
  | _ ->
    let margin, last =
      match game.words with
      | [] -> 0., 0.
      | [head] -> 0., (1. - (head |> getY))
      | head :: tail -> head |> getY, tail |> List.map getY |> List.last |> (-) 1. |> abs
    currentSpeed / ((last - margin) / wordsCount)

let move game =
  match game.words with
  | [] ->
    let margin = 1. / getVisibleWords game
    let minimumRequiredSpeed = calculateMinimumRequiredSpeed game
    let koef, minimumRequiredSpeed' = game.koefStore |> List.last
    { game with words = initWords margin WORDS_COUNT |> List.map NotVisibleWord; koefStore = [game.koefStore |> List.last |> (fun x -> fst x / 10., snd x) ] }
  | _ ->
    let changeMarginBetweenWords margin word =
      match word with
      | NotVisibleWord w when w.y < 0. -> NotVisibleWord { w with y = min (w.y - margin) 0.}
      | _ -> word
    let moveWordDown margin word =
      let moveDown c = { c with y = c.y + margin }
      match word with
      | NotVisibleWord c -> NotVisibleWord ( moveDown c)
      | VisibleWord w -> VisibleWord { w with coordinates = moveDown w.coordinates }

    let pair =
        game.words
        |> List.pairwise
        |> List.filter (fun (x, y) ->
          match (x, y) with
          | (VisibleWord _, NotVisibleWord _) -> true
          | (NotVisibleWord _, NotVisibleWord _) -> true
          | _ -> false)
        |> List.tryHead
    let changeMargin =
      match pair with
      | Some (VisibleWord w1, NotVisibleWord w2)  ->
        let currentMargin = w1.coordinates.y - w2.y
        let desiredMargin = 1. / (getVisibleWords game)
        let difference = desiredMargin - currentMargin
        max w2.y difference
      | Some (NotVisibleWord w1, NotVisibleWord w2)  ->
        let currentMargin = w1.y - w2.y
        let desiredMargin = 1. / (getVisibleWords game)
        let difference = desiredMargin - currentMargin
        max w1.y difference
      | _ -> 0.
    let speed = getSpeed game
    let words =
      game.words
      |> List.map (changeMarginBetweenWords changeMargin >> moveWordDown speed)
    let words' =
      match words with
      | [] -> words
      | head :: _ ->
        let margin =
          match head with
          | NotVisibleWord w when w.y < 0. -> -w.y
          | _ -> 0.
        words |> List.map (moveWordDown margin)
    { game with words = words' }

let setTexts game =
  let setText (data, words) word =
    match word with
    | NotVisibleWord c when c.y >= 0. ->
      let desirableLettersCount = getLettersCount game
      let length =
        match desirableLettersCount with
        | 3. -> ThreeLetters
        | 4. -> FourLetters
        | 5. -> FiveLetters
        | 6. -> SixLetters
        | 7. -> SevenLetters
        | 8. -> EightLetters
        | 9. -> EightLetters
        | _ -> failwith "Not expected letters count"
      let text, data' = popTexts game.data length 1
      match text with
      | Some text' -> data', words @ [VisibleWord { coordinates = c; text = text' |> List.head; typedCounter = 0 }]
      | None -> data', words @ [NotVisibleWord c]
    | _ -> data, words @ [word]
  let r = game.words |> List.fold setText (game.data, [])
  { game with words = snd r; data = fst r }

let getCenterPosition x (text: string) =
  let centerTextMargin = float(text.Length) / 4. * fontSize
  x - centerTextMargin

let handleTyping pressedKey game =
  match game.words with
  | [] -> game
  | w :: tail ->
    match w with
    | VisibleWord word ->
      let text = displayText word
      let increment =
          match pressedKey with
          | Some key when (String.length text > 0) && text.ToUpper().StartsWith(key)  ->
              1
          | _ -> 0
      match pressedKey, increment  with
      | (Some _, 0) -> game
      | _ ->
        let keyPressedTimestamps =
            match pressedKey with
            | Some _ -> game.keyPressedTimestamps @ [DateTimeOffset.UtcNow.Ticks]
            | None -> game.keyPressedTimestamps
        let userSpeed last first count =
          let difference = (last - first) / float(TimeSpan.TicksPerMillisecond)
          count / difference

        let koef, updated =
          match (keyPressedTimestamps, game.speedUpdated) with
          | ([], _) | ([_], _) -> game.koefStore, game.speedUpdated
          | _ ->
            let keyPressedTimestamps' = keyPressedTimestamps |> lastN 20
            let first = keyPressedTimestamps' |> List.head |> float
            let last = keyPressedTimestamps' |> List.last |> float
            let userSpeed =
              min
                (userSpeed last first (float(keyPressedTimestamps'.Length)))
                (userSpeed (float(DateTimeOffset.UtcNow.Ticks)) first (float(keyPressedTimestamps'.Length + 1)))

            let minumumRequiredSpeed = calculateMinimumRequiredSpeed game
            match minumumRequiredSpeed with
            | 0. -> game.koefStore, game.speedUpdated
            | _ ->
              let changePercentage =  userSpeed / minumumRequiredSpeed |> pow (1. / 3.)
              let koef = game.koefStore |> List.last |> fst
              [(changePercentage * koef, minumumRequiredSpeed)], DateTimeOffset.UtcNow

        let word' = VisibleWord { word with typedCounter = word.typedCounter + increment }
        { game with
            words = word' :: tail
            koefStore = koef
            scoreForCurrentRound = game.scoreForCurrentRound + float(increment)
            keyPressedTimestamps = keyPressedTimestamps |> lastN 20 }
    | _ -> game

let init () : Model * Cmd<Msg> =
  do Keyboard.init()
  NotStarted, Cmd.none

let initGame (data: Data) score gameId gameType =
  let maxVisibleWords = 3.
  let margin = 1. / maxVisibleWords
  let words' = initWords margin WORDS_COUNT |> List.map NotVisibleWord
  let game = {
    words = words'
    initialWords = words'
    initialWordsCount = 100
    currentRound = 0
    scoreForCurrentRound = 0.
    totalScore = score
    id = gameId
    framesPerSecond =  60
    koefStore = [(1., 0.)]
    speedUpdated = DateTimeOffset.UtcNow
    minimumSpeed = 2. / 1000.
    gameType = gameType
    keyPressedTimestamps = []
    data = data
    started = DateTimeOffset.UtcNow
  }
  let minimumRequiredSpeed = calculateMinimumRequiredSpeed game
  {game with koefStore = [(1., minimumRequiredSpeed)]}

let update msg model =
  let tick dispatch =
      do window.setTimeout((fun _ -> dispatch Tick), 1000 / 60) |> ignore
  match msg with
  | LoadData (round, score, gameId, gameType) ->
    match model with
    | Loading -> model, Cmd.none
    | Playing game ->
      let promise _ =
        Fetch.fetchAs<Data>("/api/game/data") []
        |> Promise.map (fun x -> { game with data = joinMap game.data x })
      model, Cmd.ofPromise promise [] (Ok >> LoadedData) (Error >> LoadedData)
    | _ ->
      let promise _ =
        Fetch.fetchAs<Data>("/api/game/data") []
        |> Promise.map (fun x -> initGame x score gameId gameType)
      model, Cmd.ofPromise promise [] (Ok >> LoadedData) (Error >> LoadedData)
  | LoadedData (Ok game) -> game |> setTexts |> Playing, Cmd.ofSub tick
  | KeyPressed key ->
    match model, key with
    | Playing game, Letter key -> (game |> handleTyping (Some key) |> Playing), Cmd.none
    | NotStarted, Enter -> model, Cmd.ofMsg (LoadData (1, 0., System.Guid.NewGuid().ToString(), UsualGame))
    | _ -> model, Cmd.none
  | Tick ->
    match model with
    | Playing game ->
      match game.words with
      | words when List.exists (fun x ->
          match x with
          | VisibleWord w when w.coordinates.y > (1.) -> true
          | _ -> false) words -> EndFail game, Cmd.none
      | _ ->
        let game' =
          game
          |> handleTyping None
          |> filterOutTypedWords
          |> move
          |> setTexts
        let isSetTextSuccessful = 
          game'.words
          |> List.exists (fun w ->
            match w with
            | NotVisibleWord c when c.y > 0. -> true
            | _ -> false)
        match isSetTextSuccessful with
        | true -> model, Cmd.ofMsg (LoadData (1, 0., game'.id, UsualGame))
        | _ -> Playing game', Cmd.ofSub tick
    | _ -> model, Cmd.none
  | LoadedData (Error _) -> model, Cmd.none
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

let drawText (w, h) (x, y) text color =
  let absoluteX = w * x
  let centerAbsoluteX = getCenterPosition absoluteX text
  let absoluteY = h * y
  Win.drawText text color font (centerAbsoluteX, absoluteY)

let drawScore (w, h) game =
  let passed = game.initialWordsCount - List.length game.words
  let total = game.initialWordsCount
  let text = sprintf "%f" (game.koefStore |> List.last |> fst)
  let y = fontSize * 2.;
  let x = w - float(text.Length) * fontSize - fontSize * 5.
  do Win.drawText text blackColor font (x, y)

  let text' = sprintf "Round: %i" game.currentRound
  let x' = fontSize * 5. + float(text'.Length)
  do Win.drawText text' blackColor font (x', y)

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
           OnClick (fun _ -> dispatch (LoadData (round, score, gameId, gameType)))]
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

let getVisibleWordsOnly =
  List.filter(fun x ->
    match x with
    | VisibleWord _ -> true
    | _ -> false)
  >> List.map (fun x ->
    match x with
    | VisibleWord y -> y
    | _ -> failwith "")

let view (model : Game) (dispatch : Msg -> unit) =
  Keyboard.registerKeyPressed (KeyPressed >> dispatch)
  let canvasView = str "" // TODO: canvas is rendered outside react element, take a look at react-canvas to use instead
  let w, h = Win.dimensions()
  let buttons = getStartGameButtons model dispatch
  match model with
  | EndSuccess _ | EndFail _ | NotStarted | Loading | Loaded (Error _) ->
    div [containerStyle]
      [ span [ Class "is-size-4" ] [ str (getText model) ];
        div [buttonsContainerStyle] buttons]
  | Playing game ->
    match game.words with
    | [] -> str ""
    | words ->
      Win.clear()
      words 
        |> getVisibleWordsOnly
        |> List.iter (fun word ->
          let fontColor = color word
          let text = displayText word
          drawText (w, h) (word.coordinates.x, word.coordinates.y) text fontColor)
      drawScore (w, h) game
      canvasView
  | _ ->
    str ""


