module Redis

open System
open Shared
open Types
open StackExchange.Redis
open Giraffe.Common

let client = ConnectionMultiplexer.Connect(Environment.GetEnvironmentVariable "redis_connection_string")

let private db = client.GetDatabase()

let private getKey (key: string) = RedisKey.op_Implicit key
let private getValue (value: string) = RedisValue.op_Implicit value

let private sortedSetKey = getKey "rating:sortedSet"
let private generateKey userId gameId = sprintf "%s:%s" userId gameId

let setRating (userId: string) gameId rating = task {
    let entry = SortedSetEntry(RedisValue.op_Implicit(generateKey userId gameId), rating)
    return! db.SortedSetAddAsync(sortedSetKey, [|entry|])
}

let private extractFromKey (key: string) =
    let l = key.Split(":") |> Array.toList
    match l with
    | [userId] -> userId, String.Empty
    | [userId;gameId] -> userId, gameId
    | _ -> failwith (sprintf "Incorrect value storred in db: %s" key)

let getRating top = task {
    let! values = db.SortedSetRangeByScoreWithScoresAsync (sortedSetKey, skip = 0L, take = top, order = Order.Descending)
    return values |> Array.map (fun x ->
        let (userId, gameId) = extractFromKey (x.Element.ToString())
        { name = userId; value = x.Score; gameId = gameId })
}

let getUserRating (userId: string) = task {
    return! db.SortedSetScoreAsync(sortedSetKey, RedisValue.op_Implicit(userId))
}

let private getRandomSetEntries setKey count = task {
    let key = getKey setKey 
    let! values = db.SetRandomMembersAsync(key, count)
    return values |> Array.map (fun x -> x.ToString()) |> Array.toList
}

let threeLetterSetKey = "threeLetterWords:set"
let fourLetterSetKey = "fourLetterWords:set"
let fiveLetterSetKey = "fiveLetterWords:set"
let sixLetterSetKey = "sixLetterWords:set"
let sevenLetterSetKey = "sevenLetterWords:set"


let getThreeLetterWords count = getRandomSetEntries threeLetterSetKey count
let getFourLetterWords count = getRandomSetEntries fourLetterSetKey count
let getFiveLetterWords count = getRandomSetEntries fiveLetterSetKey count
let getSixLetterWords count = getRandomSetEntries sixLetterSetKey count
let getSevenLetterWords count = getRandomSetEntries sevenLetterSetKey count

let validate key words =
    let isInvalid = words |> List.exists (fun x -> not (db.SetContains((getKey key), (getValue x))))
    if isInvalid then NotValid else Valid