module Utils

open System

let rand = System.Random()

let random max min = Fable.Import.JS.Math.random() * (max - min) + min;

let swap (a: _[]) x y =
    let tmp = a.[x]
    a.[x] <- a.[y]
    a.[y] <- tmp

// shuffle an array (in-place)
let shuffle a =
    let c = Array.copy a
    Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) c

let shuffleList l =
    let x = List.toArray l
    do shuffle x
    Array.toList x

let increment = (+) 1

let lastN n xs =
    xs |> List.skip (List.length xs - abs n)

let pow root x = Math.Pow(x, root)

let inRange minimum maximum = max minimum >> min maximum