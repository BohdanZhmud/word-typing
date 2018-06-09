module Game

open Shared
open Validation
open Redis
open Giraffe.Common
open Saturn
open Giraffe

let getWords round =
    if (round < 1) then failwith (sprintf "incorrect round: %i" round)

    match round with
        | 1 -> getThreeLetterWords 20L
        | 2 -> getFourLetterWords 20L
        | 3 -> getFiveLetterWords 20L
        | _ -> getSixLetterWords 20L

let private validateReplay gameReplay =
    match gameReplay.round with
    | 1 -> validate threeLetterSetKey gameReplay.words
    | 2 -> validate fourLetterSetKey gameReplay.words
    | 3 -> validate fiveLetterSetKey gameReplay.words
    | _ -> validate sixLetterSetKey gameReplay.words

let getRating = Redis.getRating 10L

let storeRating gameReplay = task {
    let score = gameReplay.score
    let! currentScore = Redis.getUserRating score.name
    return!
        match currentScore.GetValueOrDefault() < score.value with
        | true -> validateReplay gameReplay |> Validation.bindT (fun _ ->
            task {
                let! _ = Redis.setRating score.name score.gameId score.value
                return Valid
            })
        | false -> 
            task {
                return Valid
            }
}

let gameRouter = scope {
  getf "/words/%i" (fun round next ctx ->
    task {
      let! words = getWords round
      return! Successful.OK words next ctx
    }
  )
}
