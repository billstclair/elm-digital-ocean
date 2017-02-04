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
import DigitalOcean exposing ( AccountInfo, AccountInfoResult )
import Style exposing ( style, SClass(..), SId(..), id, class )
import Entities exposing ( nbsp, copyright )

import Http exposing ( Error )
import Html exposing ( Html, Attribute
                     , div, p, h2, h3, h4, text, blockquote
                     , table, tr, td, th
                     , input, button, a, img, span, fieldset, label
                     )
import Html.Attributes exposing ( align, value, size, autofocus
                                , href, target, src, title, alt
                                , width, height
                                , type_, name, value, size, placeholder
                                , name, checked, selected
                                , colspan, disabled
        )
import Html.Events exposing (onClick, onInput, onCheck)
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
    let accounts = List.sortBy .name DigitalOceanAccounts.testAccounts
        account = List.head accounts --this should be persistent
        model = { initialModel
                    | accounts = accounts
                    , account = account
                }
    in
        ( model
        , verifyAccounts model
        )
    
-- UPDATE

type Msg = Nop
         | Set Field String
         | Commit
         | Abort
         | EditAccount Account
         | NewAccount
         | SelectAccount Account
         | AccountVerified AccountInfoResult Account (List Account)
    
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.updater of
        Update updater committer ->
            case msg of
                Nop ->
                    ( model, Cmd.none )
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
                SelectAccount account ->
                    ( { model | account = Just account }
                    , Cmd.none
                    )
                AccountVerified info account accounts ->
                    accountVerified info account accounts model

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
                                { model
                                    | message = Nothing
                                    , pageState = initialAccountsState }
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
        ( m
        , if doit then
              verifyAccounts m
          else
              Cmd.none
        )

addAccount : Account -> Model -> Model
addAccount account model =
    case LE.find (\a -> a.name == account.name) model.accounts of
        Just _ ->
            { model | message = Just "New name is a duplicate." }
        Nothing ->
            if account.name == "" then
                { model | message = Just "Name may not be blank." }
            else
                let accounts = account :: model.accounts
                in
                    { model
                        | accounts = List.sortBy .name accounts
                        , message = Nothing
                        , pageState = initialAccountsState
                    }

changeAccount : String -> Account -> Model -> Model
changeAccount oldName account model =
    let accounts = model.accounts
        pred = (\name a -> a.name == name)
    in
        case if oldName == account.name then
                 Nothing
             else
                 LE.find (pred account.name) accounts
        of
            Just _ ->
                { model
                    | message
                      = Just "There is already another account with the new name."
                }
            Nothing ->
                let accs =
                        if account.name == "" then
                            LE.filterNot (pred oldName) accounts
                        else
                            LE.replaceIf (pred oldName) account accounts
                    selectedAccount = case model.account of
                                          Nothing -> Nothing
                                          Just sel ->
                                              if sel.name == oldName then
                                                  Just account
                                              else
                                                  Just sel
                in
                    { model
                        | accounts = List.sortBy .name accs
                        , account = selectedAccount
                        , message = Nothing
                        , pageState = initialAccountsState
                    }

accountVerified : AccountInfoResult -> Account -> List Account -> Model -> (Model, Cmd Msg)
accountVerified info account accounts model =
    let acct = { account | info = Just info }
        name = acct.name
        accts = LE.replaceIf (\a -> a.name == name) acct model.accounts
        m = { model | accounts = accts }
    in
        ( m, verifyNextAccount accounts model )

verifyNextAccount : List Account -> Model -> Cmd Msg
verifyNextAccount accounts model =
    case accounts of
        [] -> Cmd.none
        account :: tail ->
            case account.info of
                Nothing ->
                    DigitalOcean.getAccount
                        account.token
                        (\info -> AccountVerified info account tail)
                Just _ ->
                    verifyNextAccount tail model

verifyAccounts : Model -> Cmd Msg
verifyAccounts model =
    verifyNextAccount model.accounts model

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
            , p [ class Centered
                , class ErrorClass]
                [ case model.message of
                      Just m -> text m
                      Nothing -> text ""
                ]
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
    , info = Nothing
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
        selectedName = case model.account of
                           Nothing -> ""
                           Just acct ->
                               acct.name
    in
        div [ class AutoMargins ]
            [ h3 [ class Centered ] [ text "Accounts" ]
            , table [ class AutoMargins
                    , class PrettyTable
                    ]
                ( List.append
                      (( tr [] [ th [ aWidth "1em" ] [ text nbsp ]
                               , th [ aWidth "10em" ] [ text "Name" ]
                               , th [] [ text "Operation" ]
                               ]
                       ) ::
                           (List.concatMap
                                (\account ->
                                     let isChecked = (selectedName == account.name)
                                     in
                                         renderAccountRow
                                             account isEditing False isChecked oldName
                                )
                                model.accounts
                           )
                      )
                      (if isEditing && (oldName == "") then
                           renderAccountRow account True True False ""
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

accountInfoString : AccountInfoResult -> String
accountInfoString infoRes =
    case infoRes of
        Err error ->
            toString error      --should process this
        Ok info ->
            "[" ++ info.status ++ "] " ++ info.email

renderAccountRow : Account -> Bool -> Bool -> Bool -> String -> List (Html Msg)
renderAccountRow account isEditing isNew isChecked oldName =
    [ tr [] [ td []
                  [ input [ type_ "radio"
                          , name "account"
                          , value account.name
                          , onCheck (\sel ->
                                         if sel then
                                             SelectAccount account
                                         else
                                             Nop)
                          , checked isChecked
                          ]
                        []
                  ]
            , td [] [ text account.name ]
            , td [] [ if isEditing then
                          if isNew then
                              text "New"
                          else if oldName == account.name then
                                   text "Editing"
                               else
                                   text nbsp
                      else
                          button  [ onClick <| EditAccount account ]
                          [ text "Edit" ]
                    ]
            ]
    , tr [] [ td [ colspan 3 ]
                  [ case account.info of
                        Nothing ->
                            text "No server information."
                        Just info ->
                            text (accountInfoString info)
                  ]
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
                           , autofocus True      -- Works only first time
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
