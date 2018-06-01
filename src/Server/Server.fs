open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Saturn

open Giraffe.Serialization
open Shared
open System.IO

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let webApp = scope {
  get "/api/rating" (fun next ctx ->
    task {
      let! rating = Game.getRating
      return! json rating next ctx
    })

  post "/api/rating" (fun next ctx ->
    task {
      let! score = ctx.BindModelAsync<Score>()
      do! Game.storeRating score
      return! Successful.OK None next ctx
    })

  getf "/api/words/%i" (fun round next ctx ->
    task {
      let! words = Game.getWords round
      return! json words next ctx
    }
  )
}

let configureSerialization (services:IServiceCollection) =
  let fableJsonSettings = Newtonsoft.Json.JsonSerializerSettings()
  fableJsonSettings.Converters.Add(Fable.JsonConverter())
  services.AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer fableJsonSettings)


let configureApp (app:IApplicationBuilder) =
  app.UseDefaultFiles()

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    router webApp
    app_config configureApp
    memory_cache
    use_static publicPath
    service_config configureSerialization
    use_gzip
}

run app