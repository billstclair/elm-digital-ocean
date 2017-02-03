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

import DigitalOceanAccounts exposing ( Account )
import Style exposing ( style, SClass(..), SId(..), id, class )
import Entities exposing ( nbsp, copyright )

import Html exposing ( Html, Attribute
                     , div, p, h2, h3, h4, text, blockquote
                     , table, tr, td, th
                     , input, button, a, img, span, fieldset, label
                     )
import Html.Attributes exposing ( align, value, size, autofocus
                                , href, target, src, title, alt
                                , width, height
                                , type_, size, placeholder
                                , name, checked
                                , colspan, disabled
        )
import Html.Events exposing (onClick, onInput)
import List.Extra as LE
import Debug exposing (log)

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
    = NameField
    | TokenField
    | NoField

type alias UpdateFunction =
    Field -> String -> Model -> ( Model, Cmd Msg )

type alias CommitFunction =
    Bool -> Model -> ( Model, Cmd Msg )

-- Since there are Models in here, it needs to be tagged with a type.
type Updater =
    Update UpdateFunction CommitFunction

type alias Model =
    { message : Maybe String
    , accounts: List Account
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
    { message = Nothing
    , accounts = []
    , account = Nothing
    , page = Accounts
    , pageState = initialAccountsState
    , updater = accountsUpdater
    }

-- init : String -> ( Model, Cmd Msg )
-- init json =
init : ( Model, Cmd Msg )
init =
    let accounts = DigitalOceanAccounts.testAccounts
        model = { initialModel | accounts = accounts }
    in
        ( model
        , Cmd.none
        )
    
-- UPDATE

type Msg = Set Field String
         | Commit
         | Abort
         | EditAccount Account
         | NewAccount
    
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
                EditAccount account ->
                    setAccountPageState account model
                NewAccount ->
                    setAccountPageState blankAccount model

setAccountPageState : Account -> Model -> ( Model, Cmd Msg )
setAccountPageState account model =
    ( { model
          | pageState = AccountsState (Just { oldName = account.name
                                            , account = account
                                            }
                                      )
      }
    , Cmd.none
    )                            

updateAccountsField : Field -> String -> Model -> ( Model, Cmd Msg )
updateAccountsField field value model =
    let m = case model.pageState of
                AccountsState mea ->
                    case mea of
                        Nothing -> model
                        Just ea ->
                            let account = ea.account
                                account_ = case field of
                                               NameField ->
                                                   { account | name = value }
                                               TokenField ->
                                                   { account | token = value }
                                               _ -> account
                            in
                                { model
                                    | pageState =
                                      AccountsState
                                        (Just { ea | account = account_ })
                                }
                _ -> model
    in
        ( m, Cmd.none )

commitAccounts : Bool -> Model -> ( Model, Cmd Msg )
commitAccounts doit model =
    let m = case model.pageState of
                AccountsState mea ->
                    case mea of
                        Nothing -> model
                        Just ea ->
                            if not doit then
                                model
                            else
                                let oldName = ea.oldName
                                in
                                    if oldName == "" then
                                        addAccount ea.account model
                                    else
                                        changeAccount oldName ea.account model
                _ -> model
    in
        -- May need to write accounts to the database,
        -- and test that it's a valid token
        ( { m | pageState = initialAccountsState }
        , Cmd.none )

addAccount : Account -> Model -> Model
addAccount account model =
    case LE.find (\a -> a.name == account.name) model.accounts of
        Just _ ->
            { model | message = Just "New name is a duplicate." }
        Nothing ->
            let accounts = account :: model.accounts
            in
                { model | accounts = List.sortBy .name accounts }

changeAccountLoop : String -> Account -> List Account -> List Account -> List Account
changeAccountLoop oldName account accounts res =
    case accounts of
        [] -> res
        head :: tail ->
            if oldName == head.name then
                let tl = List.append tail res
                in
                    if account.name == "" then
                        tl
                    else
                        account :: tl
            else
                changeAccountLoop oldName account tail (head :: res)

changeAccount : String -> Account -> Model -> Model
changeAccount oldName account model =
    let accounts = model.accounts
    in
        case if oldName == account.name then
                 Nothing
             else
                 LE.find (\a -> a.name == account.name) accounts
        of
            Just _ ->
                { model
                    | message
                      = Just
                      "There is already another account with the new name."
                }
            Nothing ->
                let accs = changeAccountLoop oldName account accounts []
                in
                    { model | accounts = List.sortBy .name accs }

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ style
        , div [ id OuterDiv
              , class AutoMargins
              , class Centered
              ]
            [ h2 [ class Centered ] [ text "Elm Digital Ocean API" ]
            , case model.page of
                  Accounts -> viewAccounts model
                  _ -> text ""
            ]
        ]

viewAccounts : Model -> Html Msg
viewAccounts model =
    case model.pageState of
        AccountsState editingAccount ->
            viewAccountsInternal editingAccount model
        state ->
            text ("Bad pageState: " ++ (toString state))

blankAccount : Account
blankAccount =
    { name = ""
    , token = ""
    }

aWidth : String -> Attribute msg
aWidth w =
    Html.Attributes.style [ ( "width", w ) ]

viewAccountsInternal : Maybe EditingAccount -> Model -> Html Msg
viewAccountsInternal editingAccount model =
    let (isEditing, oldName, account) =
            case editingAccount of
                Just ea ->
                    (True, ea.oldName, ea.account )
                _ ->
                    (False, "", blankAccount)
    in
        div [ class AutoMargins ]
            [ h3 [ class Centered ] [ text "Accounts" ]
            , table [ class AutoMargins
                    , class PrettyTable
                    ]
                ( List.append
                      (( tr [] [ th [ aWidth "10em" ] [ text "Name" ]
                               , th [] [ text "Operation" ]
                               ]
                       ) ::
                           (List.map
                                (\account ->
                                     renderAccountRow
                                       account isEditing False oldName)
                                model.accounts
                           )
                      )
                      (if isEditing && (oldName == "") then
                           [ renderAccountRow account True True "" ]
                       else
                           []
                      )
                )
            , br
            , if isEditing then
                  renderAccountEditor oldName account
              else
                  button [ onClick NewAccount ]
                      [ text "New Account" ]
            ]

renderAccountRow : Account -> Bool -> Bool -> String -> Html Msg
renderAccountRow account isEditing isNew oldName =
    tr [] [ td [] [ text account.name ]
          , td [] [ if isEditing then
                        if isNew then
                            text "New"
                        else if oldName == account.name then
                            text "Editing"
                        else
                            text nbsp
                    else
                        button  [ onClick (EditAccount account) ]
                                [ text "Edit" ]
                  ]
          ]

br : Html msg
br =
    Html.br [] []

boldText : String -> Html msg
boldText string =
    span [ class Bold ] [ text string ]

renderAccountEditor : String -> Account -> Html Msg
renderAccountEditor oldName account =
    div []
        [ table [ class AutoMargins ]
            [ tr []
               [ td [ class AlignRight
                    ]
                     [ boldText "Name: " ]
               , td [ class AlignLeft ]
                   [ input [ onInput (Set NameField)
                           , size 20
                           , value account.name
                           -- Try Dom.focus in elm-lang.Dom
                           -- http://stackoverflow.com/questions/31901397/how-to-set-focus-on-an-element-in-elm
                           -- , autofocus True      -- Works only first time
                           ]
                         []
                   , if oldName == "" then
                         text ""
                     else
                         text " (Set empty to delete)"
                   ]
               ]
            , tr []
               [ td [ class AlignRight ]
                     [ boldText "Token: " ]
               , td [ class AlignLeft ]
                   [ input [ onInput (Set TokenField)
                           , size 68
                           , value account.token
                           ]
                         []
                   ]
               ]
            , tr []
                [ td [ class AlignLeft
                     , colspan 2]
                      [ button [ onClick Commit ] [ text "Save" ]
                      , text " "
                      , button [ onClick Abort ] [ text "Cancel" ]
                      ]
                ]
            ]
        ]
