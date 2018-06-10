module Validation
open Giraffe.Common

type ValidationResult = 
    | Valid
    | NotValid

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
