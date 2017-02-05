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
import DigitalOcean exposing ( AccountInfo, AccountInfoResult
                             , Domain, DomainsResult
                             , DomainRecord, DomainRecordsResult )
import Style as S exposing ( style, SClass, SId, id, class )
import Entities exposing ( nbsp, copyright )

import Http exposing ( Error )
import Html exposing ( Html, Attribute
                     , div, p, h2, h3, h4, text, blockquote, pre
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

type alias EditingDomain =
    { name : String
    , ip : String
    }

type PageState
    = AccountsState (Maybe EditingAccount)
    | DomainsState (Maybe (List Domain)) (Maybe EditingDomain)
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
    , domain: Maybe Domain
    , page : Page
    , pageState : PageState
    , updater : Updater
    }

nullUpdater : Updater
nullUpdater =
    Update updateNullField commitNull

updateNullField : Field -> String -> Model -> ( Model, Cmd Msg )
updateNullField field value model =
    ( model, Cmd.none )

commitNull : Bool -> Model -> ( Model, Cmd Msg )
commitNull doit model =
    ( model, Cmd.none )

accountsUpdater : Updater
accountsUpdater =
    Update updateAccountsField commitAccounts

initialAccountsState : PageState
initialAccountsState =
    AccountsState Nothing

type alias InitialPageStateGetter =
    Model -> (PageState, Updater, Cmd Msg)

getInitialAccountsState : InitialPageStateGetter
getInitialAccountsState model =
    ( initialAccountsState, accountsUpdater, Cmd.none )

domainsUpdater : Updater
domainsUpdater =
    --Update updateDomainsField commitDomains
    nullUpdater

fetchDomainsCmd : Model -> Cmd Msg
fetchDomainsCmd model =
    case model.account of
        Nothing -> Cmd.none
        Just account ->
            DigitalOcean.getDomains
                account.token
                (\result -> DomainsReceived result)

getInitialDomainsState : InitialPageStateGetter
getInitialDomainsState model =
    ( DomainsState Nothing Nothing
    , domainsUpdater
    , fetchDomainsCmd model
    )

domainRecordsUpdater : Updater
domainRecordsUpdater =
    --Update updateDomainRecordsField commitDomainRecords
    nullUpdater

getInitialDomainRecordsState : InitialPageStateGetter
getInitialDomainRecordsState model =
    ( DomainRecordsState, domainRecordsUpdater, Cmd.none )

initialModel : Model
initialModel =
    { message = Nothing
    , accounts = []
    , account = Nothing
    , domain = Nothing
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
         | SetPage Page
         | Set Field String
         | Commit
         | Abort
         | EditAccount Account
         | NewAccount
         | SelectAccount Account
         | SelectDomain Domain
         | FetchDomains
         | AccountVerified AccountInfoResult Account (List Account)
         | DomainsReceived DomainsResult
    
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.updater of
        Update updater committer ->
            case msg of
                Nop ->
                    ( model, Cmd.none )
                SetPage page ->
                    let props = getPageProperties page
                        (state, updater, cmd) = props.initialStateGetter model
                    in
                        ( { model
                              | page = page
                              , pageState = state
                              , updater = updater
                          }
                        , cmd
                        )
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
                    ( { model
                          | account = Just account
                          , domain = Nothing
                      }
                    , Cmd.none
                    )
                FetchDomains ->
                    ( { model | pageState = DomainsState Nothing Nothing }
                    , fetchDomainsCmd model
                    )
                SelectDomain domain ->
                    ( { model | domain = Just domain }
                    , Cmd.none
                    )
                AccountVerified info account accounts ->
                    accountVerified info account accounts model
                DomainsReceived domains ->
                    domainsReceived domains model

-- Update Accounts page

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

-- Update Domains page

domainsReceived : DomainsResult -> Model -> ( Model, Cmd Msg )
domainsReceived result model =
    case result of
        Err error ->
            ( { model | message = Just (toString error) }
            , Cmd.none
            )
        Ok domains ->
            case model.pageState of
                DomainsState _ editing ->
                    ( { model
                          | pageState = DomainsState (Just domains) editing
                          , domain = case model.domain of
                                         Nothing ->
                                             List.head domains
                                         Just dom ->
                                             LE.find
                                                 (\d -> d.name == dom.name)
                                                 domains
                      }
                    , Cmd.none
                    )
                _ ->
                    ( model, Cmd.none )

-- VIEW

type alias PageProperties =
    { page: Page
    , title : String
    , initialStateGetter : InitialPageStateGetter
    , viewer : Model -> Html Msg
    }

accountsPage : PageProperties
accountsPage =
    PageProperties
        Accounts "Accounts" getInitialAccountsState viewAccounts

pages : List PageProperties
pages =
    [ accountsPage
    , PageProperties
        Domains "Domains" getInitialDomainsState viewDomains
    , PageProperties
        DomainRecords "DNS Records" getInitialDomainRecordsState viewDomainRecords
    ]

getPageProperties : Page -> PageProperties
getPageProperties page =
    case LE.find (\p -> p.page == page) pages of
        Nothing -> accountsPage
        Just p -> p

view : Model -> Html Msg
view model =
    let props = getPageProperties model.page
    in
        div []
            [ style
            , div [ id S.OuterDiv
                  , class S.AutoMargins
                  , class S.Centered
                  ]
                  [ h2 [ class S.Centered ] [ text "Elm Digital Ocean API" ]
                  , p [ class S.Centered
                      , class S.ErrorClass
                      ]
                        [ case model.message of
                              Just m -> text m
                              Nothing -> text ""
                        ]
                  , renderNavigationLine props model
                  , props.viewer model
                  ]
            ]

nbsp2 : String
nbsp2 =
    nbsp ++ nbsp

renderNavigationLine : PageProperties -> Model -> Html Msg
renderNavigationLine page model =
    let elts = List.map (\p -> renderNavigationElement p page) pages
    in
        p [ class S.Centered ]
          <| List.intersperse (text nbsp2) elts

renderNavigationElement : PageProperties -> PageProperties -> Html Msg
renderNavigationElement props page =
    if page == props then
        span [ class S.SelectedPageLabel ]
            [ text page.title ]
    else
        a [ href "#"
          , onClick (SetPage props.page)
          ]
          [ text props.title ]

---
--- Accounts page
---

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
        div [ class S.AutoMargins ]
            [ table [ class S.AutoMargins
                    , class S.PrettyTable
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
    span [ class S.Bold ] [ text string ]

renderAccountEditor : String -> Account -> Html Msg
renderAccountEditor oldName account =
    div []
        [ table [ class S.AutoMargins ]
            [ tr []
               [ td [ class S.AlignRight
                    ]
                     [ boldText "Name: " ]
               , td [ class S.AlignLeft ]
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
               [ td [ class S.AlignRight ]
                     [ boldText "Token: " ]
               , td [ class S.AlignLeft ]
                   [ input [ onInput (Set TokenField)
                           , size 68
                           , value account.token
                           ]
                         []
                   ]
               ]
            , tr []
                [ td [ class S.AlignLeft
                     , colspan 2]
                      [ button [ onClick Commit ] [ text "Save" ]
                      , text " "
                      , button [ onClick Abort ] [ text "Cancel" ]
                      ]
                ]
            ]
        ]

---
--- Domains page
---

viewDomains : Model -> Html Msg
viewDomains model =
    div []
      (
        case model.pageState of
            DomainsState domains editing ->
                [ p [] [ case model.account of
                             Nothing -> text "No account. Shouldn't happen."
                             Just account ->
                               span []
                                   [ span [ class S.Bold ]
                                         [ text <| "Account: " ]
                                   , text account.name
                                   ]
                       ]
                , case domains of
                      Nothing ->
                          text "Fetching domains..."
                      Just doms ->
                        renderDomainsList doms model
                , p []
                    [ button [ onClick FetchDomains ]
                          [ text "Refresh" ]
                    ]
                , case domains of
                      Nothing -> text ""
                      Just _ ->
                          case model.domain of
                              Nothing -> text ""
                              Just dom ->
                                  pre [ class S.AlignLeft ]
                                      [ text dom.zoneFile ]
                ]
            state ->
                [ text ("Bad pageState: " ++ (toString state))
                ]
      )

renderDomainsList : List Domain -> Model -> Html Msg
renderDomainsList domains model =
    table [ class S.PrettyTable
          , class S.AutoMargins ]
        ((tr []
              [ th [ aWidth "1em" ] [ text nbsp ]
              , th [] [ text "Domain" ]
              , th [] [ text "TTL" ]
              ]
         ) ::
             (List.map (\domain -> renderDomainRow domain model) domains)
        )

renderDomainRow : Domain -> Model -> Html Msg
renderDomainRow domain model =
    tr [ class S.AlignLeft ]
        [ td [] [ input [ type_ "radio"
                        , name "domain"
                        , value domain.name
                        , onCheck (\sel ->
                                       if sel then
                                           SelectDomain domain
                                       else
                                           Nop
                                  )
                        , checked (case model.domain of
                                       Nothing -> False
                                       Just dom -> dom.name == domain.name
                                  )
                        ]
                      []
                ]
        , td [] [ text domain.name ]
        , td [] [ text <| toString domain.ttl ]
        ]

viewDomainRecords : Model -> Html Msg
viewDomainRecords model =
    text "DNS Records viewer not yet implemented."
