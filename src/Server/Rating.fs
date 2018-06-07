module Rating

open Giraffe
open Saturn

open Shared
open Types

let ratingRouter = scope {
  get "/rating" (fun next ctx ->
    task {
      let! rating = Game.getRating
      return! Successful.OK rating next ctx
    })

  post "/rating" (fun next ctx ->
    task {
      let! gr = ctx.BindModelAsync<GameReplay>()
      let! validationResult = Game.storeRating gr
      return! 
        match validationResult with
        | Valid -> Successful.OK None next ctx
        | NotValid -> RequestErrors.BAD_REQUEST None next ctx
    })
}
