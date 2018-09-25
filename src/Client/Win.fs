module Win

open Fable.Core
open Fable.Import.Browser

[<Emit("fullScreenCanvas")>]
let fullScreenCanvas: unit -> unit = jsNative
fullScreenCanvas()
window.onresize <- fun _ -> fullScreenCanvas()

let canvas = document.getElementsByTagName_canvas().[0]
let context = canvas.getContext_2d()

let drawText text color font position =
    let ctx = context
    ctx.fillStyle <- U3.Case1 color
    ctx.font <- font
    ctx.fillText(text, (fst position), (snd position))

let dimensions() =
  canvas.width / window.devicePixelRatio, canvas.height / window.devicePixelRatio

let clear () =
    let w, h = dimensions()
    context.clearRect(0., 0., w, h)
    ()
