module App exposing(..)

import Html exposing (Html, h1, text, div, table, thead, tbody, tr, th, td, button)
import Html.Events exposing (onClick)


type alias Model =
  { users: List User
  , lastId: Int
  }

type alias Flags =
  {
  }

type alias User =
  { id: Int
  , firstName: String
  , lastName: String
  }

createUser : Int -> User
createUser id =
  { id = id
  , firstName = "George Gordon"
  , lastName = "Byron"
  }

type Msg
  = CreateUser
  | RemoveUser Int

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


init : Flags -> (Model, Cmd Msg)
init flags =
  ( { lastId = 1
    , users = [ createUser 1 ]
    }
  , Cmd.none
  )

view : Model -> Html Msg
view model =
  div
    []
    [ h1
        []
        [ text "Hello from Elm" ]
    , div
        []
        [ table
            []
            [ thead
                []
                [ tr
                    []
                    [ th
                        []
                        [ text "Id" ]
                    , th
                        []
                        [ text "First Name" ]
                    , th
                        []
                        [ text "Last Name" ]
                    , th
                        []
                        [ text "" ]
                    ]
                ]
            , tbody
                []
                ( List.map
                    (\u ->
                       tr
                        []
                        [ td
                            []
                            [ text (toString u.id) ]
                        , td
                            []
                            [ text u.firstName ]
                        , td
                            []
                            [ text u.lastName ]
                        , td
                            []
                            [ button
                                [ onClick (RemoveUser u.id)]
                                [ text "Remove" ]
                            ]
                        ]
                    )
                    model.users
                )

            ]
        , button
            [ onClick CreateUser
            ]
            [ text "Create user" ]
        ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    CreateUser ->
      (
        let
            newLastId = model.lastId + 1
        in
        { model |
            users = (List.append model.users [(createUser newLastId)])
          , lastId = newLastId
        }
      , Cmd.none
      )

    RemoveUser userId ->
      let
          newUsers = List.filter (\u -> u.id /= userId ) model.users
      in
      ( { model | users = newUsers }
      , Cmd.none
      )

main : Program Flags Model Msg
main =
  Html.programWithFlags
    {
      init = init,
      view = view,
      update = update,
      subscriptions = subscriptions
    }
