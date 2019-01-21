module Redis

open System
open Shared
open Validation
open StackExchange.Redis
open Giraffe.Common
open System.Threading.Tasks
open Newtonsoft.Json

let client = ConnectionMultiplexer.Connect("localhost") 
let private db = client.GetDatabase()
let private getKey (key : string) = RedisKey.op_Implicit key
let private getValue (value : string) = RedisValue.op_Implicit value
let private sortedSetKey = getKey "rating:sortedSet"
let private generateKey userId gameId userDisplayName =
    sprintf "%s:%s:%s" userId gameId userDisplayName

let setScore (userId : string) userDisplayName gameId rating =
    task { 
        let entry =
            SortedSetEntry
                (RedisValue.op_Implicit 
                     (generateKey userId gameId userDisplayName), rating)
        let! _ = db.SortedSetAddAsync(sortedSetKey, [| entry |])
        return ()
    }

let private extractFromKey (key : string) =
    let l = key.Split(":") |> Array.toList
    match l with
    | [ userId; gameId ] -> Some(userId, gameId, Constants.guestName)
    | [ userId; gameId; displayName ] -> Some(userId, gameId, displayName)
    | _ -> None

let getRating top =
    task { 
        let! values = db.SortedSetRangeByScoreWithScoresAsync
                          (sortedSetKey, skip = 0L, take = top, 
                           order = Order.Descending)
        return values
               |> Array.map (fun x -> 
                      extractFromKey (x.Element.ToString())
                      |> Option.map (fun (userId, gameId, displayName) -> 
                             { score =
                                   { userId = userId
                                     value = x.Score
                                     gameId = gameId }
                               userDisplayName = displayName }))
               |> Array.filter Option.isSome
               |> Array.map Option.get
    }

let getUserScore (userId : string) userDisplayName gameId =
    task { 
        let! score = db.SortedSetScoreAsync
                         (sortedSetKey, 
                          RedisValue.op_Implicit 
                              (generateKey userId gameId userDisplayName))
        return if score.HasValue then score.Value
               else 0.
    }

let getWords setKey count =
    task { 
        let key = getKey setKey
        let! values = db.SetRandomMembersAsync(key, count)
        return values
               |> Array.map (fun x -> x.ToString())
               |> Array.toList
    }

let threeLetterSetKey = "threeLetterWords:set"
let fourLetterSetKey = "fourLetterWords:set"
let fiveLetterSetKey = "fiveLetterWords:set"
let sixLetterSetKey = "sixLetterWords:set"
let sevenLetterSetKey = "sevenLetterWords:set"

let validate words key =
    task {
        let tasks =
            words 
            |> List.map 
                   (fun x -> db.SetContainsAsync((getKey key), (getValue x)))
        let! results = Task.WhenAll tasks
        return if results |> Array.contains false then NotValid
               else Valid()
    }

let private getUserKey (id : string) = getKey ("users:" + id)

let storeUser user =
    task { 
        let user' = JsonConvert.SerializeObject(user)
        let serializedUser = RedisValue.op_Implicit (user')
        let! _ = db.StringSetAsync((getUserKey user.id), serializedUser)
        return ()
    }

let getUser id =
    task { 
        let! serializedUser = db.StringGetAsync(getUserKey id)
        let x = JsonConvert.DeserializeObject(serializedUser.ToString())
        return (box x) :?> User
    }

let storeWords words key =
    task {
        let key' = getKey key
        let values = words |> List.map getValue |> List.toArray
        let! _ = db.SetAddAsync(key', values)
        return ()
    }
