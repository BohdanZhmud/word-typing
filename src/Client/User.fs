module User
    open Elmish
    open Shared
    open Utils

    type Model = {
        id: string
        displayName: string
    }

    type Msg = None

    let delimiter = '~'
    let generateRandomId () = Constants.guestName + string delimiter + string (System.Guid.NewGuid())

    let getDisplayName (id: string) = 
      if id.Contains(toString delimiter) && id.Split(delimiter).[0] = Constants.guestName
      then Constants.guestName
      else id

    let init () : Model * Cmd<Msg> =
        let id = generateRandomId()
        { id = id; displayName = getDisplayName id }, Cmd.ofMsg None

    let update msg model : Model * Cmd<Msg> =
        model, Cmd.none