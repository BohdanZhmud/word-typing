open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Saturn

open Giraffe.Serialization
open System.IO
open Game

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let webApp = router {
  forward "/api" gameRouter
}

let configureSerialization (services:IServiceCollection) =
  let fableJsonSettings = Newtonsoft.Json.JsonSerializerSettings()
  fableJsonSettings.Converters.Add(Fable.JsonConverter())
  services.AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer fableJsonSettings)

let configureApp (app:IApplicationBuilder) =
  app.UseDefaultFiles()

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_gzip
    use_router webApp
    app_config configureApp
    memory_cache
    use_static publicPath
    service_config configureSerialization
}

run app