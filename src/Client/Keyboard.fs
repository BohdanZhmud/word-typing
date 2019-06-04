module Keyboard

open System
open Fable.Import.Browser

let mutable keysPressed = Map.empty

// Returns 1 if key with given code is pressed
let code x =
  if keysPressed.ContainsKey(x) then 1 else 0

// Update the state of the map for given key event
let update (e : KeyboardEvent, pressed) =
    let keyCode = int e.keyCode
    let op key =  if pressed then Map.add key DateTimeOffset.UtcNow.Ticks else Map.remove key
    keysPressed <- op keyCode keysPressed
    (null)

let keyPresed (s: string) =
    let c = Fable.Import.JS.String.Create(s).charCodeAt(0.)
    keysPressed |> Map.tryFind (int(c))

let anyKeyPressed () = not keysPressed.IsEmpty

let init () =
  document.addEventListener_keydown(fun e -> update(e, true))
  document.addEventListener_keyup(fun e -> update(e, false))

let clear () = keysPressed <- Map.empty
