module Admin

open Validation
open FSharp.Control.Tasks

let validateArgs lettersCount words =
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
            | _ -> NotValid
        |> Validation.bindT (fun key ->
            task {
                do! Redis.storeWords words key
                return Valid()
            })
 