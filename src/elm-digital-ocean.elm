----------------------------------------------------------------------
--
-- ElmDigitalOcean.elm
-- Elm client to Digital Ocean's API
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

--port
module ElmDigitalOcean exposing (..)

import Html exposing ( Html, Attribute
                     , div, p, h2, h3, h4, text, blockquote
                     , table, tr, td, th
                     , input, button, a, img, span, fieldset, label
                     )
import Html.Attributes exposing ( style, align, value, size
                                , href, target, src, title, alt
                                , width, height
                                , type_, size, placeholder
                                , name, checked
                                , colspan, disabled
        )
import Html.Events exposing (onClick, onInput)

{-   No ports yet
-- (key, value)
port set : (String, Maybe String) -> Cmd msg
-}

-- Temporary
set : (String, Maybe String) -> Cmd msg
set (key, value) =
    Cmd.none

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }

-- MODEL

type alias Account =
    { name : String
    , token : String
    }

type Page
    = Accounts
    | Domains
    | DomainRecords

type alias EditingAccount =
    { oldName : String
    , account : Account
    }

type PageState
    = AccountsState (Maybe EditingAccount)
    | DomainsState
    | DomainRecordsState

type Field
    = AccountField
    | TokenField

type alias UpdateFunction =
    Field -> String -> Model -> ( Model, Cmd Msg )

type alias CommitFunction =
    Bool -> Model -> ( Model, Cmd Msg )

type Updater =
    Update UpdateFunction CommitFunction

type alias Model =
    { accounts: List Account
    , account : Maybe Account
    , page : Page
    , pageState : PageState
    , updater : Updater
    }

accountsUpdater : Updater
accountsUpdater =
    Update updateAccountsField commitAccounts

initialAccountsState : PageState
initialAccountsState =
    AccountsState Nothing

initialModel : Model
initialModel =
    { accounts = []
    , account = Nothing
    , page = Accounts
    , pageState = initialAccountsState
    , updater = accountsUpdater
    }

-- init : String -> ( Model, Cmd Msg )
-- init json =
init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.none
    )
    
-- UPDATE

type Msg = Set Field String
         | Commit
         | Abort
    
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.updater of
        Update updater committer ->
            case msg of
                Set field string ->
                    updater field string model
                Commit ->
                    committer True model
                Abort ->
                    committer False model

updateAccountsField : Field -> String -> Model -> ( Model, Cmd Msg )
updateAccountsField field string model =
    ( model, Cmd.none )

commitAccounts : Bool -> Model -> ( Model, Cmd Msg )
commitAccounts doit model =
    ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    case model.page of
        Accounts -> viewAccounts model
        _ -> text ""

viewAccounts : Model -> Html Msg
viewAccounts model =
    h2 [] [ text "Accounts" ]
