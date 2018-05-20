open System.IO

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Saturn

open Giraffe.Serialization

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let webApp = scope {
  get "/test" (fun next ctx ->
    task {
      return! Successful.OK 3 next ctx
    })
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