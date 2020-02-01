module Keyboard

open Fable.Import.Browser

type KeyType =
  | Letter of string
  | Enter

let mutable private onKeyPressed = (fun (key: KeyType) -> ())
let private callCallback (e : KeyboardEvent, _) =
    let key = 
      match e.keyCode  with
      | 13. -> Enter
      | _ -> Fable.Import.JS.String.fromCharCode(e.keyCode) |> Letter
    do onKeyPressed key
    (null)

let registerKeyPressed callback =
  onKeyPressed <- callback

let init () =
  document.addEventListener_keydown(fun e -> callCallback(e, true))
