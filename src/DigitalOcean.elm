----------------------------------------------------------------------
--
-- DigitalOcean.elm
-- HTTP and JSON for Digital Ocean API v2
-- https://developers.digitalocean.com/documentation/v2/
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module DigitalOcean exposing ( AccountInfo, AccountInfoResult, getAccount
                             , Domain, DomainsResult, DomainResult, DeleteResult
                             , NewDomain
                             , getDomains, getDomain, deleteDomain, createDomain
                             , DomainRecord, DomainRecordUpdate
                             , DomainRecordsResult, DomainRecordResult
                             , getDomainRecords, getDomainRecord, createDomainRecord
                             , updateDomainRecord, deleteDomainRecord
                             , Droplet, Networks, Network, DropletsResult
                             , getDroplets
                             , AccountWriteableResult
                             , testAccountWriteable
                             )

{-| This module is a client to part of the [HTTP API](https://developers.digitalocean.com/documentation/v2/) for [Digital Ocean](https://www.digitalocean.com/).

It includes just enough to move and copy domains between accounts,
a feature that's missing from Digital Ocean's web interface.

The functions all take a `token` as their first argument.
You can create tokens [here](https://cloud.digitalocean.com/settings/api/tokens) for the logged-in account.

The getters will work with a read-only token.
Anything that makes a change requires a read/write token.

More to come, in my copious spare time, and from your generous pull requests.

# Classes
@docs AccountInfo, AccountInfoResult
@docs Domain, DomainsResult, DomainResult, DeleteResult
@docs NewDomain
@docs DomainRecord, DomainRecordUpdate
@docs DomainRecordsResult, DomainRecordResult
@docs Droplet, Networks, Network, DropletsResult
@docs AccountWriteableResult

# Functions
@docs getAccount
@docs getDomains, getDomain, deleteDomain, createDomain
@docs getDomainRecords, getDomainRecord, createDomainRecord
@docs updateDomainRecord, deleteDomainRecord
@docs getDroplets
@docs testAccountWriteable

-}

import Json.Decode as JD exposing (field, Decoder)
import Json.Encode as JE exposing (Value)
import Http exposing (Error)
import Debug exposing (log)

baseUrl : String
baseUrl =
    "https://api.digitalocean.com/v2/"

accountUrl : String
accountUrl =
    baseUrl ++ "account"

domainsUrl : String
domainsUrl =
    baseUrl ++ "domains"

domainUrl : String -> String
domainUrl domain =
    domainsUrl ++ "/" ++ domain

domainRecordsUrl : String -> String
domainRecordsUrl domain =
    (domainUrl domain) ++ "/" ++ "records"

domainRecordUrl : String -> Int -> String
domainRecordUrl domain recordId =
    (domainRecordsUrl domain) ++ "/" ++ (toString recordId)

dropletsUrl : String
dropletsUrl =
    baseUrl ++ "droplets"

tagsUrl : String
tagsUrl =
    baseUrl ++ "tags"

tagUrl : String -> String
tagUrl tag =
    tagsUrl ++ "/" ++ tag

-- Append this to GET requests that may return more than 25 records.
-- The code really should do more requests with ?page=N appended,
-- If {"meta":{"total":<cnt>}} has <cnt> greater than the number of
-- results returned, but I didn't bother for now.
perPage : String
perPage =
    "?per_page=200"

---
--- Generic HTTP GET
---

authHeader : String -> Http.Header
authHeader token =
    Http.header "Authorization" ("Bearer " ++ token)

jsonHeader : Http.Header
jsonHeader =
    Http.header "Content-Type" "application/json"

getRequestHeaders : String -> List Http.Header
getRequestHeaders token =
    [ jsonHeader
    , authHeader token
    ]

makeGetRequest : String -> String -> Decoder a -> Http.Request a
makeGetRequest token url decoder =
    Http.request
        { method = "GET"
        , headers = getRequestHeaders token
        , url = url ++ perPage
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }

sendGetRequest : (Result Error a -> msg) -> String -> String -> Decoder a -> Cmd msg
sendGetRequest resultToMsg token url decoder =
    Http.send resultToMsg <| makeGetRequest token url decoder

---
--- Generic HTTP POST
---

makePostRequest : String -> String -> Value -> Decoder a -> Http.Request a
makePostRequest token url body decoder =
    Http.request
        { method = "POST"
        , headers = getRequestHeaders token
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }

makePutRequest : String -> String -> Value -> Decoder a -> Http.Request a
makePutRequest token url body decoder =
    Http.request
        { method = "PUT"
        , headers = getRequestHeaders token
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }

sendPostRequest : (Result Error a -> msg) -> String -> String -> Value -> Decoder a -> Cmd msg
sendPostRequest resultToMsg token url value decoder =
    Http.send resultToMsg <| makePostRequest token url value decoder

sendPutRequest : (Result Error a -> msg) -> String -> String -> Value -> Decoder a -> Cmd msg
sendPutRequest resultToMsg token url value decoder =
    Http.send resultToMsg <| makePutRequest token url value decoder

---
--- Generic HTTP DELETE
---

makeDeleteRequest : String -> String -> Http.Request String
makeDeleteRequest token url =
    Http.request
        { method = "DELETE"
        , headers = getRequestHeaders token
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

sendDeleteRequest : (Result Error String -> msg) -> String -> String -> Cmd msg
sendDeleteRequest resultToMsg token url =
    Http.send resultToMsg <| makeDeleteRequest token url

---
--- Accounts
---

{-| Information returned by the ["account" query](https://developers.digitalocean.com/documentation/v2/#get-user-information).
-}
type alias AccountInfo =
    { dropletLimit : Int
    , floatingIpLimit : Int
    , email : String
    , uuid : String
    , emailVerified : Bool
    , status : String
    , statusMessage : String
    }

accountInfoDecoder : Decoder AccountInfo
accountInfoDecoder =
    JD.map7
        AccountInfo
        (field "droplet_limit" JD.int)
        (field "floating_ip_limit" JD.int)
        (field "email" JD.string)
        (field "uuid" JD.string)
        (field "email_verified" JD.bool)
        (field "status" JD.string)
        (field "status_message" JD.string)

type alias AccountRes =
    { account : AccountInfo }

accountResDecoder : Decoder AccountRes
accountResDecoder =
    JD.map
        AccountRes
        (field "account" accountInfoDecoder)

{-| `Result` of the `getAccount` function
-}
type alias AccountInfoResult =
    Result Error AccountInfo

accountResToInfo : Result Error AccountRes -> AccountInfoResult
accountResToInfo res =
    case res of
        Err error -> Err error
        Ok accountRes ->
            Ok accountRes.account

{-| Get information about a Digital Ocean account.

    getAccount token resultToMsg
    
The resulting command will call your resultToMsg function with an `AccountInfoResult` record.

Invokes the [Get User Information API](https://developers.digitalocean.com/documentation/v2/#get-user-information).
-}
getAccount : String -> (AccountInfoResult -> msg) -> Cmd msg
getAccount token resultToMsg =
    sendGetRequest
        (\res -> resultToMsg <| accountResToInfo res)
        token accountUrl accountResDecoder 

---
--- Domains
---

{-| Information about a domain.

Returned by `getDomains`, `getDomain`, and `createDomain`.
-}
type alias Domain =
    { name : String
    , ttl : Maybe Int
    , zoneFile : Maybe String
    }

type alias DomainsRes =
    { domains : List Domain
    }

domainDecoder : Decoder Domain
domainDecoder =
    JD.map3
        Domain
        (field "name" JD.string)
        (field "ttl" <| JD.nullable JD.int)
        (field "zone_file" <| JD.nullable JD.string)

domainsResDecoder : Decoder DomainsRes
domainsResDecoder =
    JD.map
        DomainsRes
        (field "domains" (JD.list domainDecoder))

{-| `Result` of the `getDomains` function.
-}
type alias DomainsResult =
    Result Error (List Domain)

domainsResToDomains : Result Error DomainsRes -> DomainsResult
domainsResToDomains res =
    case res of
        Err error -> Err error
        Ok domainsRes ->
            Ok domainsRes.domains

{-| Get a list of DNS domains for a Digital Ocean account.

    getDomains token resultToMsg
    
The resulting command will call your resultToMsg function with a `DomainsResult` record.

Invokes the [List All Domains API](https://developers.digitalocean.com/documentation/v2/#list-all-domains).
-}
getDomains : String -> (DomainsResult -> msg) -> Cmd msg
getDomains token resultToMsg =
    sendGetRequest
        (\res -> resultToMsg <| domainsResToDomains res)
        token domainsUrl domainsResDecoder 

{-| `Result` of the `getDomain` function.
-}
type alias DomainResult =
    Result Error Domain

{-| Get information for a single DNS domain in a Digital Ocean account.

    getDomain token domain resultToMsg
    
The resulting command will call your resultToMsg function with a `DomainResult` record.

Invokes the [Retreive an existing Domain API](https://developers.digitalocean.com/documentation/v2/#retrieve-an-existing-domain).
-}
getDomain : String -> String -> (DomainResult -> msg) -> Cmd msg
getDomain token domain resultToMsg =
    let url = domainUrl domain
    in
        sendGetRequest resultToMsg token url domainDecoder

{-| `Result` of the `deleteDomain` function.
-}
type alias DeleteResult =
    Result Error String

{-| Removes a domain from a Digital Ocean account.

    removeDomain token domain resultToMsg
    
The resulting command will call your resultToMsg function with a `DeleteResult` record.

Invokes the [Delete a Domain API](https://developers.digitalocean.com/documentation/v2/#delete-a-domain).
-}
deleteDomain : String -> String -> (DeleteResult -> msg) -> Cmd msg
deleteDomain token domain resultToMsg =
    let url = domainUrl domain
    in
        sendDeleteRequest resultToMsg token url

{-| Input to the `newDomain` function.
-}
type alias NewDomain =
    { name : String
    , ip : String
    }

newDomainEncoder : NewDomain -> Value
newDomainEncoder domain =
    JE.object
        [ ("name", JE.string domain.name)
        , ("ip_address", JE.string domain.ip)
        ]

domainResToDomain : Result Error DomainRes -> DomainResult
domainResToDomain res =
    case res of
        Err error -> Err error
        Ok domainRes ->
            Ok domainRes.domain

type alias DomainRes =
    { domain : Domain
    }

domainResDecoder : Decoder DomainRes
domainResDecoder =
    JD.map
        DomainRes
        (field "domain" domainDecoder)

{-| Create a new DNS domain for a Digital Ocean account.

    createDomain token domain resultToMsg
    
The resulting command will call your resultToMsg function with a `DomainResult` record.

The new domain will have the standard Digital Ocean NS records and an "A" record for the `ip` in the passed-in `domain` record.

Invokes the [Retreive an existing Domain API](https://developers.digitalocean.com/documentation/v2/#retrieve-an-existing-domain).
-}
createDomain : String -> NewDomain -> (DomainResult -> msg) -> Cmd msg
createDomain token domain resultToMsg =
    sendPostRequest
        (\res -> resultToMsg <| domainResToDomain res)
        token domainsUrl (newDomainEncoder domain) domainResDecoder

---
--- Domain Records
---

{-| Information about a single domain record.

Retured by the `getDomainRecords`, `getDomainRecord`, and `createDomainRecord` functions.

Input to the `createDomainRecord` function.
-}
type alias DomainRecord =
    { id : Int
    , recordType : String
    , name : String
    , data : String
    , priority : Maybe Int      -- for SRV and MX records
    , srvPort : Maybe Int       -- for SRV records
    , srvWeight : Maybe Int     -- for SRV records
    }

{-| Properties of a domain record to update.

Input to the `updateDomainRecord` function.
-}
type alias DomainRecordUpdate =
    { recordType : String
    , name : Maybe String
    , data : Maybe String
    , priority : Maybe Int      -- for SRV and MX records
    , srvPort : Maybe Int       -- for SRV records
    , srvWeight : Maybe Int     -- for SRV records
    }

type alias DomainRecordRes =
    { domainRecord : DomainRecord }

type alias DomainRecordsRes =
    { domainRecords : List DomainRecord
    }

domainRecordDecoder : Decoder DomainRecord
domainRecordDecoder =
    JD.map7
        DomainRecord
        (field "id" JD.int)
        (field "type" JD.string)
        (field "name" JD.string)
        (field "data" JD.string)
        (field "priority" <| JD.nullable JD.int)
        (field "port" <| JD.nullable JD.int)
        (field "weight" <| JD.nullable JD.int)

domainRecordResDecoder : Decoder DomainRecordRes
domainRecordResDecoder =
    JD.map
        DomainRecordRes
        (field "domain_record" domainRecordDecoder)

domainRecordsResDecoder : Decoder DomainRecordsRes
domainRecordsResDecoder =
    JD.map
        DomainRecordsRes
        (field "domain_records" <| JD.list domainRecordDecoder)

{-| `Result` of the `getDomainRecords` function.
-}
type alias DomainRecordsResult =
    Result Error (List DomainRecord)

domainRecordsResToDomainRecords : Result Error DomainRecordsRes -> DomainRecordsResult
domainRecordsResToDomainRecords res =
    case res of
        Err error -> Err error
        Ok domainRecordsRes ->
            Ok domainRecordsRes.domainRecords

{-| Get a list of domain records for a Digital Ocean domain.

    getDomainRecords token domain resultToMsg
    
The resulting command will call your resultToMsg function with a `DomainRecordsResult` record.

Invokes the [List all Domain Records API](https://developers.digitalocean.com/documentation/v2/#list-all-domain-records).
-}
getDomainRecords : String -> String -> (DomainRecordsResult -> msg) -> Cmd msg
getDomainRecords token domain resultToMsg =
    sendGetRequest
        (\res -> resultToMsg <| domainRecordsResToDomainRecords res)
        token (domainRecordsUrl domain) domainRecordsResDecoder

{-| `Result` of the `getDomainRecord` function.
-}
type alias DomainRecordResult =
    Result Error DomainRecord

domainRecordResToDomainRecord : Result Error DomainRecordRes -> DomainRecordResult
domainRecordResToDomainRecord res =
    case res of
        Err error -> Err error
        Ok domainRecordRes ->
            Ok domainRecordRes.domainRecord

{-| Get a single domain record for a Digital Ocean domain.

    getDomainRecord token domain id resultToMsg
    
The resulting command will call your resultToMsg function with a `DomainRecordResult` record.

You have to know the record's `id`, which is returned by `getDomainRecords`.

Invokes the [Retrieve an existing Domain Record API](https://developers.digitalocean.com/documentation/v2/#retrieve-an-existing-domain-record).
-}
getDomainRecord : String -> String -> Int -> (DomainRecordResult -> msg) -> Cmd msg
getDomainRecord token domain id resultToMsg =
    sendGetRequest
        (\res -> resultToMsg <| domainRecordResToDomainRecord res)
        token (domainRecordUrl domain id) domainRecordResDecoder

maybeIntEncoder : Maybe Int -> Value
maybeIntEncoder maybeInt =
    case maybeInt of
        Nothing -> JE.null
        Just int -> JE.int int

newDomainRecordEncoder : DomainRecord -> Value
newDomainRecordEncoder record =
    JE.object
        [ ("type", JE.string record.recordType)
        , ("name", JE.string record.name)
        , ("data", JE.string record.data)
        , ("priority", maybeIntEncoder record.priority)
        , ("port", maybeIntEncoder record.srvPort)
        , ("weight", maybeIntEncoder record.srvWeight)
        ]

maybeTupleVector : String -> Maybe value -> (value -> Value ) -> List (String, Value)
maybeTupleVector name value encoder =
    case value of
        Nothing -> []
        Just v -> [ ( name, encoder v ) ]

updatedDomainRecordEncoder : DomainRecordUpdate -> Value
updatedDomainRecordEncoder record =
    JE.object
        <| List.concat
            [ maybeTupleVector "name" record.name JE.string
            , maybeTupleVector "data" record.data JE.string
            , maybeTupleVector "priority" record.priority JE.int
            , maybeTupleVector "port" record.srvPort JE.int
            , maybeTupleVector "weight" record.srvWeight JE.int
            ]

dotifyData : String -> String
dotifyData data =
    if (data /= "@") && ((String.right 1 data) /= ".") then
        data ++ "."
    else
        data

dotifiedDomainRecordTypes : List String
dotifiedDomainRecordTypes =
    [ "CNAME"
    , "MX"
    ]

dotifyDomainRecord : DomainRecord -> DomainRecord
dotifyDomainRecord record =
    if not <| List.member record.recordType dotifiedDomainRecordTypes then
        record
    else
        { record | data = dotifyData record.data }

dotifyDomainRecordUpdate : DomainRecordUpdate -> DomainRecordUpdate
dotifyDomainRecordUpdate record =
    if not <| List.member record.recordType dotifiedDomainRecordTypes then
        record
    else
        case record.data of
            Nothing -> record
            Just d ->
                { record | data = Just <| dotifyData d }

{-| Create a new domain record for a Digital Ocean domain.

    createDomainRecord token domain record resultToMsg
    
The resulting command will call your resultToMsg function with a `DomainRecordResult` record.

The `id` field in the passed-in `domain` record is ignored.

Will add a dot (".") to the end of the `data` field for "CNAME" and "MX" records, if their value is not "@", and the dot isn't there already.

Invokes the [Create a new Domain Record API](https://developers.digitalocean.com/documentation/v2/#create-a-new-domain-record).
-}
createDomainRecord : String -> String -> DomainRecord -> (DomainRecordResult -> msg) -> Cmd msg
createDomainRecord token domain record resultToMsg =
    sendPostRequest
        (\res -> resultToMsg <| domainRecordResToDomainRecord res)
        token (domainRecordsUrl domain)
        (newDomainRecordEncoder <| dotifyDomainRecord record)
        domainRecordResDecoder

{-| Update an existing domain record for a Digital Ocean domain.

    updateDomainRecord token domain id record resultToMsg
    
The resulting command will call your resultToMsg function with a `DomainRecordResult` record.

Updates only the fields in the passed-in `record` whose values are not `Nothing`.

Will add a dot (".") to the end of the `data` field for "CNAME" and "MX" records, if their value is not "@", and the dot isn't there already.

Invokes the [Update an existing Domain Record API](https://developers.digitalocean.com/documentation/v2/#update-a-domain-record).
-}
updateDomainRecord : String -> String -> Int -> DomainRecordUpdate -> (DomainRecordResult -> msg) -> Cmd msg
updateDomainRecord token domain id record resultToMsg =
    sendPutRequest
        (\res -> resultToMsg <| domainRecordResToDomainRecord res)
        token (domainRecordUrl domain id)
        (updatedDomainRecordEncoder <| dotifyDomainRecordUpdate record)
        domainRecordResDecoder

{-| Delete an existing domain record from a Digital Ocean domain.

    deleteDomainRecord token domain id resultToMsg
    
The resulting command will call your resultToMsg function with a `DeleteResult` record.

Invokes the [Delete a Domain Record API](https://developers.digitalocean.com/documentation/v2/#delete-a-domain-record).
-}
deleteDomainRecord : String -> String -> Int -> (DeleteResult -> msg) -> Cmd msg
deleteDomainRecord token domain id resultToMsg =
    let url = domainRecordUrl domain id
    in
        sendDeleteRequest resultToMsg token url

---
--- Droplets - Just enough to get their IP addresses
---

{-| Information about a single network interface for a droplet.

Part of the `Networks` record.
-}
type alias Network =
    { ip : String
    , networkType : String
    }

networkDecoder : Decoder Network
networkDecoder =
    JD.map2
        Network
        (field "ip_address" JD.string)
        (field "type" JD.string)

{-| The lists of v4 ("A" record) and v6 ("AAAA" record) networks for a droplet.

Part of the `Droplet` record.
-}
type alias Networks =
    { v4 : List Network
    , v6 : List Network
    }

networksDecoder : Decoder Networks
networksDecoder =
    JD.map2
        Networks
        (field "v4" (JD.list networkDecoder))
        (field "v6" (JD.list networkDecoder))

{-| The network-related information about a Droplet.

Returned by the `getDroplets` function.
-}
type alias Droplet =
    { id : Int
    , name : String
    , networks : Networks
    }

dropletDecoder : Decoder Droplet
dropletDecoder =
    JD.map3
        Droplet
        (field "id" JD.int)
        (field "name" JD.string)
        (field "networks" networksDecoder)

type alias DropletsRes =
    { droplets : List Droplet
    }

dropletsResDecoder : Decoder DropletsRes
dropletsResDecoder =
    JD.map
        DropletsRes
        (field "droplets" <| JD.list dropletDecoder)

{-| The `Result` of the `getDroplets` function.
-}
type alias DropletsResult =
    Result Error (List Droplet)

dropletsResToDroplets : Result Error DropletsRes -> DropletsResult
dropletsResToDroplets res =
    case res of
        Err error -> Err error
        Ok dropletsRes ->
            Ok dropletsRes.droplets

{-| Returns information about all the droplets in a Digital Ocean account.

    getDroplets token resultToMsg
    
The resulting command will call your resultToMsg function with a `DropletsResult` record.

Invokes the [List all Droplets API](https://developers.digitalocean.com/documentation/v2/#list-all-droplets).
-}
getDroplets : String -> (DropletsResult -> msg) -> Cmd msg
getDroplets token resultToMsg =
    sendGetRequest
        (\res -> resultToMsg <| dropletsResToDroplets res)
        token dropletsUrl dropletsResDecoder

{-| The `Result` of the `testAccountWriteable` function.
-}
type alias AccountWriteableResult =
    Result Error Bool

nonexistentTag : String
nonexistentTag =
    "this tag couldnt possibly exist"

testWriteableResultToBool : Result Error String -> AccountWriteableResult
testWriteableResultToBool res =
    case res of
        Ok _ ->
            -- If this happens, the tag DID exist, and we just deleted it.
            Ok True
        Err error ->
            case error of
                Http.BadStatus response ->
                    case response.status.code of
                        403 -> Ok False -- forbidden, account not writeable
                        404 -> Ok True -- not found, as expected
                        _ -> Err error
                _ ->
                    Err error

{-| Tests whether the `token` for a Digital Ocean account gives write access.

    testAccountWriteable token resultToMsg
    
The resulting command will call your resultToMsg function with an `AccountWriteableResult` record.

It works by attempting to delete a sure-to-not-exist resource tag, and inspecting the result. If it gets a 403 ("forbidden"), then the account is not writeable. If it gets a 404 ("not found"), then it IS writeable.

Invokes the [Delete a Tag API](https://developers.digitalocean.com/documentation/v2/#delete-a-tag).
-}
testAccountWriteable : String -> (AccountWriteableResult -> msg) -> Cmd msg
testAccountWriteable token resultToMsg =
    let url = tagUrl nonexistentTag
    in
        sendDeleteRequest
          (\res ->  resultToMsg <| testWriteableResultToBool res)
          token url
