module Main exposing (..)

import Html exposing (..)
import Http
import Material
import Material.Options as Options exposing (css)
import Material.Tabs as Tabs
import Json.Decode as JD exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { mdl : Material.Model
    , tab : Int
    , post : Post
    }


type alias Post =
    { userId : Int
    , id : Int
    , title : String
    , body : String
    }


emptyPost : Post
emptyPost =
    Post -1 -1 "" ""


type Msg
    = SelectTab Int
    | Mdl (Material.Msg Msg)
    | GetPost (Result Http.Error Post)


init : ( Model, Cmd Msg )
init =
    let
        model =
            { mdl = Material.model
            , tab = 0
            , post = emptyPost
            }
    in
        ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SelectTab idx ->
            case idx of
                0 ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | tab = idx }
                    , getPost 1
                    )

        GetPost (Ok post) ->
            ( { model | post = post }, Cmd.none )

        GetPost (Err _) ->
            ( model, Cmd.none )

        Mdl msg ->
            Material.update Mdl msg model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Tabs.render Mdl
        [ 0 ]
        model.mdl
        [ Tabs.ripple
        , Tabs.onSelectTab SelectTab
        , Tabs.activeTab model.tab
        ]
        [ Tabs.label
            [ Options.center ]
            [ Options.span [ css "width" "4px" ] []
            , text "Home"
            ]
        , Tabs.label
            [ Options.center ]
            [ Options.span [ css "width" "4px" ] []
            , text "Data Sets"
            ]
        ]
        []


decodePost : Decoder Post
decodePost =
    JD.map4 Post
        (field "userId" int)
        (field "id" int)
        (field "title" string)
        (field "body" string)


getPost : Int -> Cmd Msg
getPost id =
    Http.send GetPost (Http.get ("https://jsonplaceholder.typicode.com/posts/" ++ toString id) decodePost)
