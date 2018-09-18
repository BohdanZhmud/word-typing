module Auth 
 
open Saturn 
open Giraffe 
open Microsoft.AspNetCore.Http 
open Microsoft.AspNetCore.Authentication 
open FSharp.Control.Tasks.ContextInsensitive 
open System.Security.Claims 
 
open Shared 
 
let private challenge (scheme : string) (redirectUri : string) : HttpHandler = 
    fun (next : HttpFunc) (ctx : HttpContext) -> 
        task { 
            do! ctx.ChallengeAsync(
                    scheme,
                    AuthenticationProperties(RedirectUri = redirectUri))
            return! next ctx
        }
let private googleAuth = challenge "Google" "/"
let private githubAuth = challenge "GitHub" "/"

let authApiRouter = router {
  get "/info" (fun next ctx ->
    task {
        let user =
            if ctx.User.Identity.IsAuthenticated
                then
                    let id = ctx.User.Claims |> Seq.find (fun claim -> claim.Type = ClaimTypes.NameIdentifier)
                    let name = ctx.User.Claims |> Seq.find (fun claim -> claim.Type = ClaimTypes.Name)
                    { id = id.Value; displayName = name.Value; authState = Authenticated }
                else
                    { id = System.Guid.NewGuid().ToString(); displayName = Constants.guestName; authState = NotAuthenticated }
        return! Successful.OK user next ctx
    })
}

let authRouter = router {
    get "/google" googleAuth
 
    get "/github" githubAuth

    get "/signOut" (Giraffe.Auth.signOut "Cookies" >=> redirectTo false "/")
}
