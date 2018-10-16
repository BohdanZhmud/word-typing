open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Saturn
open Giraffe.Serialization
open System.IO
open Game
open Auth
open System.Security.Claims
open System
open Admin

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let webApp = router {
  forward "/api/game" gameRouter
  forward "/api/auth" authApiRouter
  forward "/api/admin" adminRouter
  forward "/auth" authRouter
}

let configureSerialization (services:IServiceCollection) =
  let fableJsonSettings = Newtonsoft.Json.JsonSerializerSettings()
  fableJsonSettings.Converters.Add(Fable.JsonConverter())
  services.AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer fableJsonSettings)

let configureApp (app:IApplicationBuilder) =
  app.UseDefaultFiles()

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_github_oauth
      (Environment.GetEnvironmentVariable "github_client_id")
      (Environment.GetEnvironmentVariable "github_client_secret")
      "/oauth_callback_github" ["id", ClaimTypes.NameIdentifier; "login", ClaimTypes.Name; ]
    use_google_oauth
      (Environment.GetEnvironmentVariable "google_client_id")
      (Environment.GetEnvironmentVariable "google_client_secret")
      "/oauth_callback_google" ["id", ClaimTypes.NameIdentifier; "displayName", ClaimTypes.Name]
    use_gzip
    use_router webApp
    app_config configureApp
    memory_cache
    use_static publicPath
    service_config configureSerialization
}

run app
