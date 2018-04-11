port module App exposing(..)

import Html exposing (Html, h1, h2, text, div, table, thead, tbody, tr, th, td, button)
import Html.Events exposing (onClick)

port ping : Int -> Cmd msg
port pong : (String -> msg) -> Sub msg


type alias Model =
  { users: List User
  , lastId: Int
  , messages: List String
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
  | Pong String

subscriptions : Model -> Sub Msg
subscriptions model =
  pong (\message -> Pong message)


init : Flags -> (Model, Cmd Msg)
init flags =
  let
      lastId = 1
  in
  ( { lastId = lastId
    , users = [ createUser 1 ]
    , messages = []
    }
  , ping lastId
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

        , div
            []
            [ h2 [] [ text "Interop messages" ]
            , div
                []
                ( List.map (\m -> div [] [ text m ]) model.messages )
            ]
        ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    CreateUser ->
      let
          newLastId = model.lastId + 1
      in
      (
        { model |
            users = (List.append model.users [(createUser newLastId)])
          , lastId = newLastId
        }
      , ping newLastId
      )

    RemoveUser userId ->
      let
          newUsers = List.filter (\u -> u.id /= userId ) model.users
      in
      ( { model | users = newUsers }
      , Cmd.none
      )

    Pong message ->
      ( { model | messages = (List.append model.messages [message])  }, Cmd.none )

main : Program Flags Model Msg
main =
  Html.programWithFlags
    {
      init = init,
      view = view,
      update = update,
      subscriptions = subscriptions
    }
