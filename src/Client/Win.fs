module Win

open Fable.Core
open Fable.Import.Browser

let canvas = document.getElementsByTagName_canvas().[0]
canvas.width <- window.innerWidth;
canvas.height <- window.innerHeight;

let context = canvas.getContext_2d()

let drawText text color font position =
    let ctx = context
    ctx.fillStyle <- U3.Case1 color
    ctx.font <- font
    ctx.fillText(text, (fst position), (snd position))

let dimensions() =
  canvas.width, canvas.height

let clear () =
    let ctx = canvas.getContext_2d()
    let w, h = dimensions()
    ctx.clearRect(0., 0., w, h)
    ()
