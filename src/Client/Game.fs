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
  totalScore: float
  scoreForCurrentRound: float
  id: string
  framesPerSecond: int
  speed: float
  gameType: GameType
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
    { game with words = word' :: (List.tail words); scoreForCurrentRound = game.scoreForCurrentRound + float(increment) }

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

let initGame (round: Round) score gameId gameType =
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
    scoreForCurrentRound = 0.
    totalScore = score
    id = gameId
    framesPerSecond = round.framesPerSecond
    speed = round.speed
    gameType = gameType
  }

let update msg model =
  match msg with
  | LoadGame (round, score, gameId, gameType) ->
    let promise _ =
      Fetch.fetchAs<Round>(sprintf "/api/round/%i" round) []
      |> Promise.map (fun x -> initGame x score gameId gameType)
    model, Cmd.ofPromise promise [] (Ok >> StartGame) (Error >> StartGame)
  | StartGame (Ok game) ->
    let sub dispatch =
      do render (w, h) (Playing game) game.framesPerSecond dispatch () 
    Playing game, Cmd.ofSub sub
  | StartGame (Error _) -> model, Cmd.none
  | GameStarted game -> game, Cmd.none
  | Finish gameState -> gameState, Cmd.none 
  | StoreScore gameReplay when gameReplay.score.value > 0. ->
      let cmd =
        Cmd.ofPromise
          (fun _ -> promise {
            let! res = Fetch.postRecord "api/score" gameReplay []
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

let startBtnId = "start-btn";
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
    match gameType with
    | RestartLastRound ->
      match model with
      | EndSuccess game | EndFail game -> game.id
      | _ -> string (System.Guid.NewGuid)
    | UsualGame -> string (System.Guid.NewGuid)
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