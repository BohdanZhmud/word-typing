module Admin

open Validation
open Giraffe.Common
open Saturn
open Giraffe
open System.Security.Claims
open Microsoft.AspNetCore.Http

let validateArgs words lettersCount =
    if (words |> List.exists (fun (w:string) -> w.Length <> lettersCount)) then NotValid else Valid ()

let storeWords lettersCount words =
    validateArgs words lettersCount
        |> Validation.bind (fun () ->
            match lettersCount with
            | 3 -> Valid (Redis.threeLetterSetKey)
            | 4 -> Valid (Redis.fourLetterSetKey)
            | 5 -> Valid (Redis.fiveLetterSetKey)
            | 6 -> Valid (Redis.sixLetterSetKey)
            | 7 -> Valid (Redis.sevenLetterSetKey)
            | _ -> NotValid)
        |> Validation.bind (fun key ->
            Valid (task {
                do! Redis.storeWords words key
            }))

type StoreWordsRequest = {
    lettersCount: int
    words: string list
}

let getUserId (ctx: HttpContext) =
    let id =
        ctx.User.Claims
        |> Seq.find
            (fun claim ->
            claim.Type = ClaimTypes.NameIdentifier)
    id.Value

let adminRouter =
    router {
        pipe_through (Auth.requireAuthentication Cookies)
        post "/words/store" (fun next ctx ->
            task {
                let id = getUserId ctx
                let admins = (System.Environment.GetEnvironmentVariable("admins")).Split(";")
                return! 
                    match admins |> Array.contains id with
                    | true -> task {
                            let! x = ctx.BindModelAsync<StoreWordsRequest>()
                            let r = storeWords x.lettersCount x.words
                            return!
                                match r with
                                | Valid store -> task {
                                        do! store
                                        return! Successful.OK None next ctx
                                    }
                                | NotValid -> task {
                                        return! RequestErrors.BAD_REQUEST None next ctx
                                    }
                        }
                    | false -> task {
                        let unauthenticated = 401
                        return! setStatusCode unauthenticated next ctx
                    }
            })
    }
