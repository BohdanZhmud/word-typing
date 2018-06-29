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
    validate gameReplay.words (getRoundWorsSetKey gameReplay.round)

let getRating () = Redis.getRating 10L

let storeRating gameReplay = task {
    let score = gameReplay.score
    let! currentScore = Redis.getUserRating score.name
    return!
        match currentScore.GetValueOrDefault() < score.value with
        | true -> validateReplay gameReplay |> Validation.bindT (fun _ ->
            task {
                let! _ =  Redis.setRating score.name score.gameId score.value
                return Valid
            })
        | false ->
            task {
                return Valid
            }
}

let gameRouter = scope {
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

  post "/rating" (fun next ctx ->
    task {
      let! gr = ctx.BindModelAsync<GameReplay>()
      let! validationResult = storeRating gr
      return!
        match validationResult with
        | Valid -> Successful.OK None next ctx
        | NotValid -> RequestErrors.BAD_REQUEST None next ctx
    })
}
