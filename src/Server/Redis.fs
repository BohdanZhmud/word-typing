module Redis

open System
open Shared
open StackExchange.Redis
open Giraffe.Common

let client = ConnectionMultiplexer.Connect(Environment.GetEnvironmentVariable "redis-connection-string")

let private db = client.GetDatabase()

let private sortedSetKey = RedisKey.op_Implicit("rating:sortedSet")

let setRating (userId: string) rating = task {
    let entry = SortedSetEntry(RedisValue.op_Implicit(userId), rating)
    return! db.SortedSetAddAsync(sortedSetKey, [|entry|])
}

let getRating top = task {
    let! values = db.SortedSetRangeByScoreWithScoresAsync (sortedSetKey, skip = 0L, take = top)
    return values |> Array.map (fun x -> { name = x.Element.ToString(); value = x.Score })
}

let getUserRating (userId: string) = task {
    return! db.SortedSetScoreAsync(sortedSetKey, RedisValue.op_Implicit(userId))
}
