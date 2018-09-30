module Validation

open Giraffe.Common

type ValidationResult<'T> =
    | Valid of 'T
    | NotValid

let bind f x =
    match x with
    | NotValid -> NotValid
    | Valid x' -> f x'

let bindT f vT =
    task { 
        let! v = vT
        return! match v with
                | NotValid -> task { return NotValid }
                | _ -> f v
    }
