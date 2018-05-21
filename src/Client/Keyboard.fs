module Keyboard

open Fable.Import.Browser

module Keyboard =
    let mutable keysPressed = Set.empty

    /// Returns 1 if key with given code is pressed
    let code x =
      if keysPressed.Contains(x) then 1 else 0

    /// Update the state of the set for given key event
    let update (e : KeyboardEvent, pressed) =
        let keyCode = int e.keyCode
        let op =  if pressed then Set.add else Set.remove
        keysPressed <- op keyCode keysPressed
        (null)

    let keyPresed (s: string) =
        let c = Fable.Import.JS.String.Create(s).charCodeAt(0.)
        keysPressed.Contains(int(c))

    let anyKeyPressed () = not keysPressed.IsEmpty

    let init () =
      document.addEventListener_keydown(fun e -> update(e, true))
      document.addEventListener_keyup(fun e -> update(e, false))

    let clear () = keysPressed <- Set.empty