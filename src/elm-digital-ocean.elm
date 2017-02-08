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
                             , DomainRecord, DomainRecordsResult
                             , Droplet, DropletsResult, Networks, Network
                             )
import Style as S exposing ( style, SClass, SId, id, class
                           , labeledTableStyle
                           )
import Entities exposing ( copyright, ellipsis )

import Http exposing ( Error )
import Html exposing ( Html, Attribute
                     , div, p, h2, h3, h4, text, blockquote, pre
                     , table, tr, td, th, colgroup, col
                     , input, button, a, img, span, fieldset, label
                     , select, option
                     )
import Html.Attributes exposing ( align, value, size, autofocus
                                , href, target, src, title, alt
                                , width, height
                                , type_, name, value, size, placeholder
                                , name, checked, selected
                                , colspan, disabled
        )
import Html.Events exposing (onClick, onInput, onCheck)
import Json.Decode as JD
import List.Extra as LE
import Debug exposing (log)
import Dict exposing (Dict)
import Task

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
    = AccountsPage
    | DomainsPage
    | DomainRecordsPage
    | CopyDomainPage

type alias EditingAccount =
    { oldName : String
    , account : Account
    }

type alias EditingDomain =
    { name : String
    , ip : String
    }

type alias CopyDomainStorage =
    { droplets : Maybe (List Droplet)
    , originalDomainRecords : Maybe (List DomainRecord)
    , domainRecords : Maybe (List DomainRecord)
    , toAccount : Maybe Account
    , toDomainName : String
    , toDroplets : Maybe (List Droplet)
    , toDroplet : Maybe Droplet
    , verifyDelete : Bool
    }

initialCopyDomainStorage : Model -> CopyDomainStorage
initialCopyDomainStorage model =
    { droplets = Nothing
    , originalDomainRecords = Nothing
    , domainRecords = Nothing
    , toAccount = model.account
    , toDomainName = ""
    , toDroplets = Nothing
    , toDroplet = Nothing
    , verifyDelete = False
    }

type PageState
    = AccountsState (Maybe EditingAccount)
    | DomainsState (Maybe (List Domain)) (Maybe EditingDomain)
    | DomainRecordsState
    | CopyDomainState CopyDomainStorage

type Field
    = NameField
    | TokenField
    | ToDropletField
    | ToAccountField
    | ToDomainField
    | DomainRecordField Int
    | VerifyDeleteField
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

getInitialCopyDomainState : InitialPageStateGetter
getInitialCopyDomainState model =
    ( CopyDomainState <| initialCopyDomainStorage model
    , copyDomainUpdater
    , Cmd.batch
        [ fetchDropletsCmd FromDroplets model.account
        , fetchDomainRecordsCmd model.account model.domain
        ]
    )

copyDomainUpdater : Updater
copyDomainUpdater =
    Update updateCopyDomainField commitCopyDomain

type WhichDroplets
    = FromDroplets
    | ToDroplets

fetchDropletsCmd : WhichDroplets -> Maybe Account -> Cmd Msg
fetchDropletsCmd whichDroplets account =
    case account of
        Nothing -> Cmd.none
        Just acct ->
            DigitalOcean.getDroplets
                acct.token
                (\result -> DropletsReceived result whichDroplets)

fetchDomainRecordsCmd : Maybe Account -> Maybe Domain -> Cmd Msg
fetchDomainRecordsCmd account domain =
    case account of
        Nothing -> Cmd.none
        Just acct ->
            case domain of
                Nothing -> Cmd.none
                Just dom ->
                    DigitalOcean.getDomainRecords
                        acct.token dom.name DomainRecordsReceived

initialModel : Model
initialModel =
    { message = Nothing
    , accounts = []
    , account = Nothing
    , domain = Nothing
    , page = AccountsPage
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
         | ErrorMessage String
         | SetPage Page
         | Set Field String
         | Commit
         | Abort
         | EditAccount Account
         | NewAccount
         | ShowDomains Account
         | SelectAccount Account
         | SelectDomain Domain
         | FetchDomains
         | CopyDomain Domain
         | AccountVerified AccountInfoResult Account (List Account)
         | DomainsReceived DomainsResult
         | DropletsReceived DropletsResult WhichDroplets
         | DomainRecordsReceived DomainRecordsResult
         | CopyDomainRecords (List DomainRecord) Account Domain
         | CopyDomainError Error
         | CopyDomainComplete Account Domain

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.updater of
        Update updater committer ->
            case msg of
                Nop ->
                    ( model, Cmd.none )
                ErrorMessage message ->
                    ( { model | message = Just message }
                    , Cmd.none
                    )
                SetPage page ->
                    setPage page model
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
                ShowDomains account ->
                    let m = { model
                                | account = Just account
                                , domain = Nothing
                            }
                    in
                        setPage DomainsPage m
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
                CopyDomain domain ->
                    setPage CopyDomainPage { model | domain = Just domain }
                SelectDomain domain ->
                    ( { model | domain = Just domain }
                    , Cmd.none
                    )
                AccountVerified info account accounts ->
                    accountVerified info account accounts model
                DomainsReceived domains ->
                    domainsReceived domains model
                DropletsReceived domains whichDroplets ->
                    dropletsReceived domains whichDroplets model
                DomainRecordsReceived records ->
                    domainRecordsReceived records model
                CopyDomainRecords records account domain ->
                    copyDomainRecords records account domain model
                CopyDomainError error ->
                    ( { model | message = Just <| toString error }
                    , Cmd.none
                    )
                CopyDomainComplete account domain ->
                    copyDomainComplete account domain model

-- Update Accounts page

setPage : Page -> Model -> ( Model, Cmd Msg )
setPage page model =
    let props = getPageProperties page
        (state, updater, cmd) = props.initialStateGetter model
    in
        ( { model
              | message = Nothing
              , page = page
              , pageState = state
              , updater = updater
          }
        , cmd
        )

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

-- Update copy domains page

dropletsReceived : DropletsResult -> WhichDroplets -> Model -> ( Model, Cmd Msg )
dropletsReceived result whichDroplets model =
    case result of
        Err error ->
            ( { model | message = Just (toString error) }
            , Cmd.none
            )
        Ok droplets ->
            case model.pageState of
                CopyDomainState storage ->
                    let (fromDroplets, toDroplets) =
                        case whichDroplets of
                            FromDroplets ->
                                let toDroplets =
                                        case storage.toAccount of
                                            Nothing -> Just droplets
                                            Just _ -> storage.toDroplets
                                in
                                    (Just droplets, toDroplets)
                            ToDroplets ->
                                (storage.droplets, Just droplets)
                        toDroplet = case storage.toDroplet of
                                        Nothing ->
                                            case toDroplets of
                                                Nothing -> Nothing
                                                Just ds ->
                                                    List.head ds
                                        Just d -> Just d
                        storage2 = { storage
                                       | droplets = fromDroplets
                                       , toDroplets = toDroplets
                                       , toDroplet = toDroplet
                                   }
                    in
                        ( { model
                              | pageState
                                = CopyDomainState
                                { storage2
                                    | domainRecords
                                      = domainRecordMagic toDroplet storage
                                }
                          }
                        , case whichDroplets of
                              FromDroplets ->
                                  fetchDropletsCmd ToDroplets storage.toAccount
                              ToDroplets ->
                                  Cmd.none
                        )
                _ ->
                    ( model, Cmd.none )

domainRecordsReceived : DomainRecordsResult -> Model -> ( Model, Cmd Msg)
domainRecordsReceived result model =
    case result of
        Err error ->
            ( { model | message = Just (toString error) }
            , Cmd.none
            )
        Ok records ->
            case model.pageState of
                CopyDomainState storage ->
                    ( { model
                          | pageState = CopyDomainState
                                        { storage
                                            | originalDomainRecords = Just records
                                            , domainRecords = Just records
                                        }
                      }
                    , Cmd.none
                    )
                _ ->
                    ( model, Cmd.none )
updateCopyDomainField : Field -> String -> Model -> ( Model, Cmd Msg )
updateCopyDomainField field value model =
    case model.pageState of
        CopyDomainState storage ->
            updateCopyDomainStorage storage field value model
        _ ->
            ( model, Cmd.none )

updateCopyDomainStorage : CopyDomainStorage -> Field -> String -> Model -> ( Model, Cmd Msg )
updateCopyDomainStorage storage field value model =
    case field of
        ToAccountField ->
            let (account, toDroplets, toDroplet)
                    = case LE.find (\a -> a.name == value) model.accounts of
                          Nothing ->
                              ( Nothing, Nothing, Nothing )
                          Just acct ->
                              if (case model.account of
                                      Nothing -> False
                                      Just ma -> (ma.name == acct.name)
                                 )
                              then
                                  ( Just acct
                                  , storage.droplets
                                  , case storage.droplets of
                                        Nothing -> Nothing
                                        Just ds -> List.head ds
                                  )
                              else
                                  ( Just acct , Nothing , Nothing )
            in                    
                ( { model
                      | pageState = CopyDomainState
                                    { storage
                                        | toAccount = account
                                        , toDroplets = toDroplets
                                        , toDroplet = toDroplet
                                        , domainRecords
                                          = domainRecordMagic toDroplet storage
                                        , verifyDelete = False
                                    }
                  }
                , case toDroplets of
                      Just _ -> Cmd.none
                      Nothing ->
                          fetchDropletsCmd ToDroplets account
                )
        ToDropletField ->
            case storage.toDroplets of
                Nothing -> ( model, Cmd.none )
                Just droplets ->
                    let droplet = LE.find (\a -> a.name == value) droplets
                        domainRecords = domainRecordMagic droplet storage
                    in
                        ( { model
                              | pageState = CopyDomainState
                                            { storage
                                                | toDroplet = droplet
                                                , domainRecords = domainRecords
                                            }
                          }
                        , Cmd.none
                        )
        ToDomainField ->
            ( { model
                  | pageState = CopyDomainState
                                { storage
                                    | toDomainName = value
                                    , verifyDelete = False
                                }
              }
            , Cmd.none
            )
        DomainRecordField id ->
            case storage.domainRecords of
                Nothing -> ( model, Cmd.none )
                Just records ->
                    ( { model
                          | pageState = CopyDomainState
                                        { storage
                                            | domainRecords =
                                              Just
                                              <| LE.updateIf
                                                  (\r -> id == r.id)
                                                  (\r -> { r | data = value })
                                                  records
                                        }
                      }
                    , Cmd.none
                    )
        VerifyDeleteField ->
            ( { model
                    | pageState = CopyDomainState
                                  { storage | verifyDelete = (value == "True") }
              }
            , Cmd.none
            )
        _ ->
            ( model, Cmd.none )

copyDomainComplete : Account -> Domain -> Model -> ( Model, Cmd Msg )
copyDomainComplete account domain model =
    setPage DomainsPage
        { model
            | account = Just account
            , domain = Just domain
        }

-- Replace the A & AAAA values in storage.originalDomainRecords
-- by changing values found in the networks of storage.droplets
-- to the corresponding values in the networks of toDroplet
-- TODO
domainRecordMagic : Maybe Droplet -> CopyDomainStorage -> Maybe (List DomainRecord)
domainRecordMagic toDroplet storage =
    case storage.originalDomainRecords of
        Nothing -> Nothing
        Just drs ->
            case storage.droplets of
                Nothing -> Just drs
                Just droplets ->
                    case toDroplet of
                        Nothing -> Just drs
                        Just droplet ->
                            let updater = (\dr ->
                                            updateDomainRecord
                                               dr droplets droplet
                                          )
                            in
                                Just <| List.map updater drs

updateDomainRecord : DomainRecord -> (List Droplet) -> Droplet -> DomainRecord
updateDomainRecord record droplets droplet =
    if not (List.member record.recordType updateableRecordTypes) then
        record
    else
        let (accessor, index) = findNetworkAddress record.data droplets
        in
            if index < 0 then
                record
            else
                let ips = publicNetworkIps <| accessor droplet.networks
                    idx = min index <| (List.length ips) - 1
                in
                    if idx < 0 then
                        record
                    else
                        case LE.getAt idx ips of
                            Nothing -> record
                            Just data ->
                                { record | data = data }

emptyNetworks : Networks -> List Network
emptyNetworks _ =
    []

findPublicIpIndex : String -> List Network -> Maybe Int
findPublicIpIndex address networks =
    LE.findIndex (\ip -> ip == address) <| publicNetworkIps networks

findNetworkAddress : String -> (List Droplet) -> ( Networks -> List Network, Int )
findNetworkAddress address droplets =
    case droplets of
        [] -> ( emptyNetworks, -1 )
        droplet :: tail ->
            let networks = droplet.networks
            in
                case findPublicIpIndex address networks.v4 of
                    Just idx -> (.v4, idx)
                    Nothing ->
                        case findPublicIpIndex address networks.v6 of
                            Just idx -> (.v6, idx)
                            Nothing -> ( emptyNetworks, -1 )                        

emptyAccount : Account
emptyAccount =
    { name = ""
    , token = ""
    , info = Nothing
    }

copyDomainStuff : Model -> (Bool, CommitType, Account, List DomainRecord, Account, String)
copyDomainStuff model =
    let no = (False, Copy, emptyAccount, [], emptyAccount, "")
    in
        case model.pageState of
            CopyDomainState storage ->
                let (copyType, _) = copyDomainMessage storage model
                    domainName = storage.toDomainName
                in
                    case model.account of
                        Nothing -> no
                        Just fromAccount ->
                            case storage.domainRecords of
                                Nothing -> no
                                Just records ->
                                    case storage.toAccount of
                                        Nothing -> no
                                        Just toAccount ->
                                            ( domainName /= ""
                                            , copyType
                                            , fromAccount
                                            , records
                                            , toAccount
                                            , domainName
                                            )
            _ -> no

commitCopyDomain : Bool -> Model -> ( Model, Cmd Msg )
commitCopyDomain doit model =
    let (doit, commitType, fromAccount, domainRecords, toAccount, domainName)
            = copyDomainStuff model
    in
        if not doit then
            ( model, Cmd.none )
        else
            let operator = case commitType of
                               Copy -> doCopyDomain
                               Move -> doMoveDomain
                               Overwrite -> doOverwriteDomain
            in
                ( model
                , operator fromAccount domainRecords toAccount domainName
                )

ignoredDomainRecordTypes : List String
ignoredDomainRecordTypes =
    [ "NS"
    ]

copyDomainRecords : List DomainRecord -> Account -> Domain -> Model -> (Model, Cmd Msg)
copyDomainRecords records account domain model =
    case records of
        [] -> copyDomainComplete account domain model
        record :: tail ->
            if List.member record.recordType ignoredDomainRecordTypes then
                copyDomainRecords tail account domain model
            else
                let toMsg = (\res ->
                                 case res of
                                     Err error -> CopyDomainError error
                                     Ok _ ->
                                         CopyDomainRecords tail account domain
                            )
                in
                    ( model
                    , DigitalOcean.createDomainRecord
                        account.token domain.name record toMsg
                    )

msgToCmd : Msg -> Cmd Msg
msgToCmd msg =
    Task.perform identity <| Task.succeed msg

-- This process could leave the domain partially populated.
-- Should probably delete it in that case, so the user can retry
-- For doMoveDomain, however, the old domain is gone, so what
-- we've got in memory is the only record.
-- It probably won't happen in practice, so I'm not going to worry about it.
doCopyDomain : Account -> List DomainRecord -> Account -> String -> Cmd Msg
doCopyDomain _ domainRecords account domainName =
    let aRecord = LE.find (\a -> a.recordType == "A") domainRecords
    in
        case aRecord of
            Nothing ->
                msgToCmd <| ErrorMessage "No A record. Can't create domain."
            Just record ->
                let newDomain = { name = domainName
                                , ip = record.data
                                }
                    -- The first A record is added by createDomain()
                    records = LE.remove record domainRecords
                    toMsg = (\res ->
                                 case res of
                                     Err error -> CopyDomainError error
                                     Ok domain ->
                                       CopyDomainRecords
                                           records account domain
                            )
                in
                    DigitalOcean.createDomain account.token newDomain toMsg

doMoveDomain = doCopyDomain
doOverwriteDomain = doCopyDomain


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
        AccountsPage "Accounts" getInitialAccountsState viewAccounts

domainsPage : PageProperties
domainsPage =
    PageProperties
        DomainsPage "Domains" getInitialDomainsState viewDomains

domainRecordsPage : PageProperties
domainRecordsPage =
    PageProperties
        DomainRecordsPage "DNS Records" getInitialDomainRecordsState viewDomainRecords

copyDomainPage : PageProperties
copyDomainPage =
    PageProperties
        CopyDomainPage "Copy Domain" getInitialCopyDomainState viewCopyDomain

pages : List PageProperties
pages =
    [ accountsPage
    , domainsPage
    , domainRecordsPage
    , copyDomainPage
    ]

navigationPages : List PageProperties
navigationPages =
    [ accountsPage
    , domainsPage
    --, domainRecordsPage
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

nbsp2 : Html msg
nbsp2 =
    text <| Entities.nbsp ++ Entities.nbsp

nbsp : Html msg
nbsp =
    text Entities.nbsp

renderNavigationLine : PageProperties -> Model -> Html Msg
renderNavigationLine page model =
    let elts = List.map (\p -> renderNavigationElement p page) navigationPages
    in
        p [ class S.Centered ]
          <| List.intersperse nbsp2 elts

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
                      (( tr [] [ th [ aWidth "1em" ] [ nbsp ]
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
                                   nbsp
                      else
                          span []
                              [ button  [ onClick <| EditAccount account ]
                                    [ text "Edit" ]
                              , nbsp
                              , button [ onClick <| ShowDomains account ]
                                    [ text "Show Domains" ]
                              ]
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
                    , nbsp
                    , button [ onClick <| SetPage AccountsPage ]
                        [ text "Cancel" ]
                    ]
                , case domains of
                      Nothing -> text ""
                      Just _ ->
                          case model.domain of
                              Nothing -> text ""
                              Just dom ->
                                  case dom.zoneFile of
                                      Nothing -> div [] []
                                      Just zoneFile ->
                                          div []
                                              [ h3 [ class S.Centered ]
                                                    [ text "Zone File" ]
                                              , pre [ class S.AlignLeft ]
                                                  [ text zoneFile ]
                                              ]
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
              [ th [ aWidth "1em" ] [ nbsp ]
              , th [] [ text "Domain" ]
              , th [] [ text "TTL" ]
              , th [] [ text "Operation" ]
              ]
         ) ::
             (List.map (\domain -> renderDomainRow domain model) domains)
        )

domainBaseUrl : String
domainBaseUrl =
    "https://cloud.digitalocean.com/networking/domains/"

domainUrl : Domain -> String
domainUrl domain =
    domainBaseUrl ++ domain.name

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
        , td [] [ a [ href <| domainUrl domain
                    , target "_blank"
                    ]
                      [ text domain.name ]
                ]
        , td [] [ text <| toString domain.ttl ]
        , td [] [ button [ onClick <| CopyDomain domain ]
                      [ text "Copy" ]
                ]
        ]

-- This can't yet be called
viewDomainRecords : Model -> Html Msg
viewDomainRecords model =
    text "DNS Records viewer not yet implemented."

viewCopyDomain : Model -> Html Msg
viewCopyDomain model =
    div []
        [ labeledTableStyle
        , h3 [ class S.Centered ] [ text "Copy Domain" ]
        , case model.pageState of
              CopyDomainState storage ->
                  viewCopyDomainBody storage model
              _ ->
                  p [] [ text "Bad storage. Shouldn't happen." ]
        ]

viewCopyDomainBody : CopyDomainStorage -> Model -> Html Msg
viewCopyDomainBody storage model =
    div []
        [ table [ class S.AutoMargins
                ]
              <| viewCopyDomainRows storage model
        , p []
            [ case storage.domainRecords of
                  Nothing -> text "Fetching domain records..."
                  Just records ->
                      table [ class S.PrettyTable
                            , class S.AutoMargins ]
                      ( List.append
                            [ tr []
                                  [ th [] [ text "Type" ]
                                  , th [] [ text "Name" ]
                                  , th [] [ text "Data" ]
                                  ]
                            ]
                            <| List.concatMap domainRecordRows records
                      )
            ]
        ]

truncate : Int -> String ->String
truncate length string =
    if (String.length string) > length then
        (String.left (length-1) string) ++ ellipsis
    else
        string

updateableRecordTypes : List String
updateableRecordTypes =
    [ "A", "AAAA" ]

domainRecordRows : DomainRecord -> List (Html Msg)
domainRecordRows record =
    let hasExtraRow = (record.priority /= Nothing)
                      || (record.srvPort /= Nothing)
                      || (record.srvWeight /= Nothing)
        firstRow = tr []
                   [ td [] [ text record.recordType ]
                   , td [] [ text record.name ]
                   , td [] [ if List.member
                                 record.recordType updateableRecordTypes
                             then
                                 input [ type_ "text"
                                       , size 30
                                       , value record.data
                                       , onInput
                                             (Set <| DomainRecordField record.id)
                                       ]
                                 []
                             else
                                 text <| truncate 50 record.data ]
                   ]
    in
        if not hasExtraRow then
            [ firstRow ]
        else
            let el = (\x label ->
                          case x of
                              Nothing -> []
                              Just c ->
                                  [ label ++ (toString c) ]
                     )
                extra = List.concat
                        [ el record.srvWeight "weight:"
                        , el record.srvPort "port:"
                        , el record.priority "priority:"
                        ]
            in
                [ firstRow
                -- This undisplayed row makes the additional properties row
                -- have the same background color as the main properties row.
                , tr [ class S.DisplayNone ]
                    [ td [ colspan 3 ] []
                    ]
                , tr [] [ td [] [ nbsp ]
                        , td [ colspan 2]
                              [ text
                                    <| String.concat
                                    <| List.intersperse ", " extra
                              ]
                        ]
                ]

selector : String -> List a -> (a -> String) -> Field -> Html Msg
selector default list getName field =
    select [ onInput <| Set field
           ]
    <| List.map (\name ->
                     option [ value name
                            , selected <| default == name
                            ]
                     [ text name ]
                )
        <| List.map getName list

dropletSelector : String -> List Droplet -> Html Msg
dropletSelector default droplets =
    selector default droplets .name ToDropletField

accountSelector : String -> List Account -> Html Msg
accountSelector default accounts =
    selector default accounts .name ToAccountField

thtdRow : String -> List (Html Msg) -> Html Msg
thtdRow label values =
    tr []
        [ th [] [ text label ]
        , td [] values
        ]

publicNetworkIps : List Network -> List String
publicNetworkIps networks =
    List.map .ip
        <| List.filter (\n -> "public" == n.networkType) networks

renderIps : List String -> List String -> List (Html msg)
renderIps a aaaa =
    let isper = (\l a ->
                     if a == [] then
                         []
                     else
                         [l ++ (String.concat <| List.intersperse ", " a)]
                )
    in
        List.concat [ isper "A: " a
                    , isper "AAAA:" aaaa
                    ]
            |> List.map text
            |> List.intersperse br

overwriteDomainMessage : String
overwriteDomainMessage =
    "Clicking the \"Overwrite Domain\" button will overwrite the domain."

moveDomainMessage : String
moveDomainMessage =
    "Clicking the \"Move Domain\" button will move the domain to the \"To account\"."

isSameAccount : Maybe Account -> Maybe Account -> Bool
isSameAccount acct1 acct2 =
    if acct1 == acct2 then
        True
    else
        case acct1 of
            Nothing -> False
            Just a1 ->
                case acct2 of
                    Nothing -> False
                    Just a2 ->
                        a1.name == a2.name

type CommitType
    = Copy
    | Move
    | Overwrite

copyDomainMessage : CopyDomainStorage -> Model -> (CommitType, String)
copyDomainMessage storage model =
    case storage.toDomainName of
        "" -> (Copy, "")
        toDomainName ->
            case model.domain of
                Nothing -> (Copy, "")
                Just domain ->
                    if toDomainName /= domain.name
                        || model.account == Nothing
                            || storage.toAccount == Nothing
                    then
                        (Copy, "")
                    else if isSameAccount model.account storage.toAccount
                    then
                        (Overwrite, overwriteDomainMessage)
                    else
                        (Move, moveDomainMessage)
                                       
copyDomainVerificationRows : CopyDomainStorage -> Model -> (CommitType, List (Html Msg))
copyDomainVerificationRows storage model =
    let (commitType, message) = copyDomainMessage storage model
        html = if commitType == Copy then
                   []
               else
                   [ tr [] [ td [ colspan 2 ]
                                 [ text message
                                 , br
                                 , text "Do you really want to do that?"
                                 ]
                           ]
                   , tr [ class S.DisplayNone ]
                       [ td [ colspan 2 ] [] ]
                   , thtdRow
                         ""
                         [ input [ type_ "checkbox"
                                 , onCheck
                                       (\x ->
                                            Set VerifyDeleteField
                                            <| if x then "True" else "False"
                                       )
                                 , checked storage.verifyDelete
                                 ]
                               []
                         , text "Yes. Do it!"
                         ]
                   ]
    in
        ( commitType, html )

viewCopyDomainRows : CopyDomainStorage -> Model -> List (Html Msg)
viewCopyDomainRows storage model =
    let fromAccount = case model.account of
                          Nothing -> "<blank>"
                          Just acct -> acct.name
        fromDomain = case model.domain of
                         Nothing -> "<blank>"
                         Just domain -> domain.name
        toAccount = case storage.toAccount of
                        Nothing -> fromAccount
                        Just acct -> acct.name
        toDroplet = case storage.toDroplet of
                        Nothing -> ""
                        Just d -> d.name
        (a, aaaa) = case storage.toDroplet of
                        Nothing -> ([], [])
                        Just d ->
                            let networks = d.networks
                            in
                                ( publicNetworkIps networks.v4
                                , publicNetworkIps networks.v6
                                )
        ipHtml = renderIps a aaaa
        (commitType, checkRows) = copyDomainVerificationRows storage model
    in
        ( List.append
              [ thtdRow "From account:" [ text fromAccount ]
              , thtdRow "From domain:" [ text fromDomain ]
              , thtdRow "To account:" [ accountSelector toAccount model.accounts ]
              , thtdRow "To droplet:" [ case storage.toDroplets of
                                            Nothing -> text "Fetching droplets..."
                                            Just droplets ->
                                            dropletSelector toDroplet droplets
                                      ]
              , thtdRow "" ( case ipHtml of
                                 [] -> [ nbsp ]
                                 _ -> ipHtml
                           )
              , thtdRow "To domain:" [ input [ type_ "text"
                                             , onInput <| Set ToDomainField
                                             , size 30
                                             , value storage.toDomainName
                                             , autofocus True
                                             ]
                                           []
                                     ]
              , thtdRow ""
                  [ button [ disabled
                             <| storage.toDomainName == ""
                             || (checkRows /= [] && (not storage.verifyDelete))
                           , onClick Commit
                           ]
                        [ text <| case commitType of
                                      Copy -> "Copy Domain"
                                      Move -> "Move Domain"
                                      Overwrite -> "Overwrite Domain"
                        ]
                  , nbsp
                  , button [ onClick <| SetPage DomainsPage ]
                      [ text "Cancel" ]
                  ]
              ]
              checkRows
        )
