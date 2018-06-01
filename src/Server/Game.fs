module Game

open Shared
open Giraffe.Common

let getWords round =
    if (round < 1) then failwith (sprintf "incorrect round: %i" round)

    match round with
        | 1 -> Redis.getThreeLetterWords 20L
        | 2 -> Redis.getFourLetterWords 20L
        | 3 -> Redis.getFiveLetterWords 20L
        | _ -> Redis.getSixLetterWords 20L

let getRating = Redis.getRating 10L

let storeRating score = task {
    let! currentScore = Redis.getUserRating score.name
    if currentScore.GetValueOrDefault() < score.value
        then 
          let! _ = Redis.setRating score.name score.gameId score.value 
          ()
}