module Game

open Shared
open Validation
open Redis
open Giraffe.Common
open Saturn
open Giraffe

let private framesPerSecond = 60

let private getRoundWorsSetKey = function
    | 1 | 2 -> threeLetterSetKey
    | 3 | 4 | 5 | 6 -> fourLetterSetKey
    | 7 | 8 -> fiveLetterSetKey
    | 9 -> sixLetterSetKey
    | _ -> sevenLetterSetKey

let getRound round = task {
    if (round < 1) then failwith (sprintf "incorrect round: %i" round)
    let! words = getWords (getRoundWorsSetKey round) 20L
    let speedInPixelsPer1000 =
        match round with
        | 1 -> 2.
        | 2 -> 3.
        | 3 -> 2.5
        | 4 -> 2.7
        | 5 -> 2.9
        | 6 -> 3.2
        | 7 -> 2.2
        | 8 -> 2.8
        | 9 -> 2.8
        | round' -> (1. + float(round')/4.)
    return {
        number = round
        words = words
        framesPerSecond = framesPerSecond
        speed = speedInPixelsPer1000 / 1000.
    }
}

let private validateReplay (gameReplay: GameReplay) =
    match gameReplay.gameType with
    | RestartLastRound when gameReplay.round = 1 -> task {
            return NotValid
        }
    | RestartLastRound | UsualGame _ -> validate gameReplay.words (getRoundWorsSetKey gameReplay.round)

let getRating () = Redis.getRating 10L

let storeScore gameReplay = task {
    let setScore gameReplay' calculateValueToSet = task {
        let score = gameReplay'.score
        let! currentScore = Redis.getUserScore score.userId gameReplay'.score.gameId
        let valueToSet = calculateValueToSet currentScore
        do! Redis.setScore score.userId score.gameId valueToSet
        return valueToSet
    }

    return! gameReplay |> validateReplay |> Validation.bindT (fun _ ->
        task {
            let! r = 
                match gameReplay.gameType with
                | UsualGame -> setScore gameReplay (fun currentScore -> (currentScore + gameReplay.score.value))
                | RestartLastRound ->
                    let score = gameReplay.score
                    setScore gameReplay (fun currentScore -> (currentScore + (score.value * (1. - Constants.percentageChargeForRestart))))
            return Valid (r)
        })
}

let gameRouter = router {
  getf "/round/%i" (fun round next ctx ->
    task {
      let! words = getRound round
      return! Successful.OK words next ctx
    }
  )

  get "/rating" (fun next ctx ->
    task {
      let! rating = getRating()
      return! Successful.OK rating next ctx
    })

  post "/score" (fun next ctx ->
    task {
      let! gr = ctx.BindModelAsync<GameReplay>()
      let! validationResult = storeScore gr
      return!
        match validationResult with
        | Valid score -> Successful.OK score next ctx
        | NotValid -> RequestErrors.BAD_REQUEST None next ctx
    })
}
