module Game

open Shared
open Validation
open Redis
open Giraffe.Common
open Saturn
open Giraffe
open System.Security.Claims
open Redis
open Redis
open Redis

let private framesPerSecond = 60

let private getRoundWorsSetKey =
    function 
    | 1 | 2 -> threeLetterSetKey
    | 3 | 4 | 5 | 6 -> fourLetterSetKey
    | 7 | 8 -> fiveLetterSetKey
    | 9 -> sixLetterSetKey
    | _ -> sevenLetterSetKey

let private getWords _ = task {
    //System.Threading.Tasks.Task.Delay(System.TimeSpan.FromSeconds(3.)).GetAwaiter().GetResult()
    let! threeLettersWords = getWords threeLetterSetKey 50L
    let! fourLettersWords = getWords fourLetterSetKey 50L
    let! fiveLettersWords = getWords fiveLetterSetKey 50L
    let! sixLettersWords = getWords sixLetterSetKey 50L
    let! sevenLettersWords = getWords sevenLetterSetKey 50L
    let! eightLettersWords = getWords eightLetterSetKey 50L
    let! nineLettersWords = getWords nineLettersSetKey 50L

    return [ ThreeLetters, threeLettersWords
             FourLetters, fourLettersWords
             FiveLetters, fiveLettersWords
             SixLetters, sixLettersWords
             SevenLetters, sevenLettersWords 
             EightLetters, eightLettersWords
             NineLetters, nineLettersWords ] |> Map.ofList
}


let private validateReplay (gameReplay : GameReplay) =
    match gameReplay.gameType with
    | RestartLastRound when gameReplay.round = 1 -> task { return NotValid }
    | RestartLastRound | UsualGame _ -> 
        validate gameReplay.words (getRoundWorsSetKey gameReplay.round)

let getRating() = Redis.getRating 10L

let storeScore gameReplay userDisplayName =
    task { 
        let setScore gameReplay' calculateValueToSet =
            task { 
                let score = gameReplay'.score
                let! currentScore = Redis.getUserScore score.userId 
                                        userDisplayName gameReplay'.score.gameId
                let valueToSet = calculateValueToSet currentScore
                do! Redis.setScore score.userId userDisplayName score.gameId 
                        valueToSet
                return valueToSet
            }
        return! gameReplay
                |> validateReplay
                |> Validation.bindT (fun _ -> 
                       task { 
                           let! r = match gameReplay.gameType with
                                    | UsualGame -> 
                                        setScore gameReplay 
                                            (fun currentScore -> 
                                            (currentScore 
                                             + gameReplay.score.value))
                                    | RestartLastRound -> 
                                        let score = gameReplay.score
                                        setScore gameReplay 
                                            (fun currentScore -> 
                                            (currentScore 
                                             + (score.value 
                                                * (1. 
                                                   - Constants.percentageChargeForRestart))))
                           return Valid(r)
                       })
    }

let gameRouter =
    router { 
        get "/data" 
            (fun next ctx -> task { let! words = getWords ()
                                    return! Successful.OK words next ctx })
        get "/rating" 
            (fun next ctx -> task { let! rating = getRating()
                                    return! Successful.OK rating next ctx })
        post "/score" (fun next ctx -> 
            task { 
                let! gr = ctx.BindModelAsync<GameReplay>()
                let userDisplayName =
                    match ctx.User.Identity.IsAuthenticated with
                    | true -> 
                        ctx.User.Claims
                        |> Seq.filter 
                               (fun claim -> claim.Type = ClaimTypes.Name)
                        |> Seq.map (fun claim -> claim.Value)
                        |> Seq.head
                    | false -> Constants.guestName
                let! validationResult = storeScore gr userDisplayName
                return! match validationResult with
                        | Valid score -> Successful.OK score next ctx
                        | NotValid -> RequestErrors.BAD_REQUEST None next ctx
            })
    }
