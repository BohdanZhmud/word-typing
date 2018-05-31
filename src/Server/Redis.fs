module Redis

open System
open Shared
open StackExchange.Redis
open Giraffe.Common
open System.Reflection.Emit

let client = ConnectionMultiplexer.Connect(Environment.GetEnvironmentVariable "redis_connection_string")

let private db = client.GetDatabase()

let private sortedSetKey = RedisKey.op_Implicit("rating:sortedSet")
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
