module Auth

open Shared
open Elmish
open Fable.PowerPack

open Fable.Helpers.React
open Fable.Helpers.React.Props

type Model = User option

type Msg =
    | Loading
    | Loaded of Result<User, exn>

let init () : Model * Cmd<Msg> =
  None, Cmd.ofMsg Loading

let update msg model =
    match msg with
    | Loading ->
      let promise _ = Fetch.fetchAs<User> "/api/auth/info" []
      model, Cmd.ofPromise promise [] (Ok >> Loaded) (Error >> Loaded)
    | Loaded (Ok user) -> Some user, Cmd.none
    | Loaded (Error _) -> None, Cmd.none

let authIconStyle =
  Style [
    MarginRight "10px"
  ]

let authButtonStyle =
  Style [
    MarginTop "10px"
    MarginRight "10px"
  ]

let signIncontainerStyle =
  Style [
    Display "flex"
    FlexDirection "column"
  ]

let signOutContainerStyle =
  Style [
    Display "flex"
    AlignItems "center"
  ]

let displayNameStyle =
  Style [
    MarginTop "10px"
    MarginRight "10px"
  ]

let view (model : Model) (dispatch : Msg -> unit) = 
  match model with
  | None -> str "Loading..." 
  | Some user -> 
    match user.authState with 
    | NotAuthenticated ->
        div [ signIncontainerStyle ] [
            a [ ClassName "button"; Href "auth/google"; authButtonStyle ] [
                i [ ClassName "fa fa-google"; authIconStyle ] []
                str "Sign in with Google" ]
            a [ ClassName "button"; Href "auth/github"; authButtonStyle ] [
                i [ ClassName "fa fa-github"; authIconStyle ] []
                str "Sign in with Github" ]
        ]
    | Authenticated ->
        div [ signOutContainerStyle ] [
            span [ displayNameStyle ] [ str user.displayName ]
            a [ ClassName "button"; Href "auth/signOut"; authButtonStyle ] [str "Sign out"]
        ]
