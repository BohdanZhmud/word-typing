module Redis

open System
open Shared
open Validation
open StackExchange.Redis
open Giraffe.Common
open System.Threading.Tasks

let client = ConnectionMultiplexer.Connect(Environment.GetEnvironmentVariable "redis_connection_string")

let private db = client.GetDatabase()

let private getKey (key: string) = RedisKey.op_Implicit key
let private getValue (value: string) = RedisValue.op_Implicit value

let private sortedSetKey = getKey "rating:sortedSet"
let private generateKey userId gameId = sprintf "%s:%s" userId gameId

let setScore (userId: string) gameId rating = task {
    let entry = SortedSetEntry(RedisValue.op_Implicit(generateKey userId gameId), rating)
    let! _ = db.SortedSetAddAsync(sortedSetKey, [|entry|])
    return ()
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
        { userId = userId; value = x.Score; gameId = gameId })
}

let getUserScore (userId: string) gameId = task {
    let! score = db.SortedSetScoreAsync(sortedSetKey, RedisValue.op_Implicit(generateKey userId gameId))
    return if score.HasValue then score.Value else 0.
}

let getWords setKey count = task {
    let key = getKey setKey 
    let! values = db.SetRandomMembersAsync(key, count)
    return values |> Array.map (fun x -> x.ToString()) |> Array.toList
}

let threeLetterSetKey = "threeLetterWords:set"
let fourLetterSetKey = "fourLetterWords:set"
let fiveLetterSetKey = "fiveLetterWords:set"
let sixLetterSetKey = "sixLetterWords:set"
let sevenLetterSetKey = "sevenLetterWords:set"

let validate words key = task {
    let tasks = words |> List.map (fun x -> db.SetContainsAsync((getKey key), (getValue x)))
    let! results =  Task.WhenAll tasks
    return if results |> Array.contains false then NotValid else Valid ()
}
