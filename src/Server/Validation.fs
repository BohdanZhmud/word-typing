module Validation
open Giraffe.Common

type ValidationResult = 
    | Valid
    | NotValid

let bind f x =
    match x with 
    | NotValid -> NotValid
    | Valid -> f x

let bindT f vT = task {
    let! v = vT
    return! 
        match v with
        | NotValid -> 
            task {
                return NotValid
            }
        | _ -> f v
}
