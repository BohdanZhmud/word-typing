module Redis

open System
open Shared
open StackExchange.Redis
open Giraffe.Common

let client = ConnectionMultiplexer.Connect(Environment.GetEnvironmentVariable "redis-connection-string")
let db = client.GetDatabase()

let setRating (userName: string) rating = task {
    let entry = SortedSetEntry(RedisValue.op_Implicit(userName), rating)
    db.SortedSetAddAsync(RedisKey.op_Implicit("rating:sortedSet"), [|entry|]) |> ignore
}

let getRating top = task {
    let! values = db.SortedSetRangeByScoreWithScoresAsync (RedisKey.op_Implicit("rating:sortedSet"), 0., top)
    return values |> Array.map (fun x -> { name = x.Element.ToString(); value = x.Score })
}
