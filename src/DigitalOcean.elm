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
                             , DomainRecord, DomainRecordsResult, DomainRecordResult
                             , getDomainRecords, getDomainRecord, createDomainRecord
                             , updateDomainRecord, deleteDomainRecord
                             , Droplet, Networks, Network, DropletsResult
                             , getDroplets
                             )

import Json.Decode as JD exposing (field, Decoder)
import Json.Encode as JE exposing (Value)
import Http exposing (Error)

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
        , url = url
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

sendPostRequest : (Result Error a -> msg) -> String -> String -> Value -> Decoder a -> Cmd msg
sendPostRequest resultToMsg token url value decoder =
    Http.send resultToMsg <| makePostRequest token url value decoder

---
--- Generic HTTP DELETE
---

makeDeleteRequest : String -> String -> Decoder a -> Http.Request a
makeDeleteRequest token url decoder =
    Http.request
        { method = "DELETE"
        , headers = getRequestHeaders token
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }

sendDeleteRequest : (Result Error a -> msg) -> String -> String -> Decoder a -> Cmd msg
sendDeleteRequest resultToMsg token url decoder =
    Http.send resultToMsg <| makeDeleteRequest token url decoder

---
--- Accounts
---

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

decodeAccountRes : String -> Result String AccountInfo
decodeAccountRes json =
    case JD.decodeString accountResDecoder json of
        Err msg -> Err msg
        Ok accountRes ->
            Ok accountRes.account

type alias AccountInfoResult =
    Result Error AccountInfo

accountResToInfo : Result Error AccountRes -> AccountInfoResult
accountResToInfo res =
    case res of
        Err error -> Err error
        Ok accountRes ->
            Ok accountRes.account

getAccount : String -> (AccountInfoResult -> msg) -> Cmd msg
getAccount token resultToMsg =
    sendGetRequest
        (\res -> resultToMsg <| accountResToInfo res)
        token accountUrl accountResDecoder 

---
--- Domains
---

type alias Domain =
    { name : String
    , ttl : Int
    , zoneFile : String
    }

type alias DomainsRes =
    { domains : List Domain
    }

domainDecoder : Decoder Domain
domainDecoder =
    JD.map3
        Domain
        (field "name" JD.string)
        (field "ttl" JD.int)
        (field "zone_file" JD.string)

domainsResDecoder : Decoder DomainsRes
domainsResDecoder =
    JD.map
        DomainsRes
        (field "domains" (JD.list domainDecoder))

type alias DomainsResult =
    Result Error (List Domain)

domainsResToDomains : Result Error DomainsRes -> DomainsResult
domainsResToDomains res =
    case res of
        Err error -> Err error
        Ok domainsRes ->
            Ok domainsRes.domains

getDomains : String -> (DomainsResult -> msg) -> Cmd msg
getDomains token resultToMsg =
    sendGetRequest
        (\res -> resultToMsg <| domainsResToDomains res)
        token domainsUrl domainsResDecoder 

type alias DomainResult =
    Result Error Domain

getDomain : String -> String -> (DomainResult -> msg) -> Cmd msg
getDomain token domain resultToMsg =
    let url = domainUrl domain
    in
        sendGetRequest resultToMsg token url domainDecoder

emptyDecoder : Decoder ()
emptyDecoder =
    JD.succeed ()

type alias DeleteResult =
    Result Error ()

deleteDomain : String -> String -> (DeleteResult -> msg) -> Cmd msg
deleteDomain token domain resultToMsg =
    let url = domainUrl domain
    in
        sendDeleteRequest resultToMsg token url emptyDecoder

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

createDomain : String -> NewDomain -> (DomainResult -> msg) -> Cmd msg
createDomain token domain resultToMsg =
    sendPostRequest
        (\res -> resultToMsg <| domainResToDomain res)
        token domainsUrl (newDomainEncoder domain) domainResDecoder

---
--- Domain Records
---

type alias DomainRecord =
    { id : Int
    , recordType : String
    , name : String
    , data : String
    , priority : Maybe Int      -- for SRV and MX records
    , srvPort : Maybe Int       -- for SRV records
    , srvWeight : Maybe Int     -- for SRV records
    }

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

domainRecordsResDecoder : Decoder DomainRecordsRes
domainRecordsResDecoder =
    JD.map
        DomainRecordsRes
        (field "domain_records" <| JD.list domainRecordDecoder)

type alias DomainRecordsResult =
    Result Error (List DomainRecord)

domainRecordsResToDomainRecords : Result Error DomainRecordsRes -> DomainRecordsResult
domainRecordsResToDomainRecords res =
    case res of
        Err error -> Err error
        Ok domainRecordsRes ->
            Ok domainRecordsRes.domainRecords

getDomainRecords : String -> String -> (DomainRecordsResult -> msg) -> Cmd msg
getDomainRecords token domain resultToMsg =
    sendGetRequest
        (\res -> resultToMsg <| domainRecordsResToDomainRecords res)
        token (domainRecordsUrl domain) domainRecordsResDecoder

type alias DomainRecordResult =
    Result Error DomainRecord

getDomainRecord : String -> String -> Int -> (DomainRecordResult -> msg) -> Cmd msg
getDomainRecord token domain id resultToMsg =
    sendGetRequest
        resultToMsg token (domainRecordUrl domain id) domainRecordDecoder

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

-- The domain.id field is ignored
createDomainRecord : String -> String -> DomainRecord -> (DomainRecordResult -> msg) -> Cmd msg
createDomainRecord token domain record resultToMsg =
    sendPostRequest
        resultToMsg
        token (domainRecordsUrl domain)
        (newDomainRecordEncoder record) domainRecordDecoder

updateDomainRecord : String -> String -> DomainRecord -> (DomainRecordResult -> msg) -> Cmd msg
updateDomainRecord token domain record resultToMsg =
    sendPostRequest
        resultToMsg
        token (domainRecordUrl domain record.id)
        (newDomainRecordEncoder record) domainRecordDecoder

deleteDomainRecord : String -> String -> Int -> (DeleteResult -> msg) -> Cmd msg
deleteDomainRecord token domain id resultToMsg =
    let url = domainRecordUrl domain id
    in
        sendDeleteRequest resultToMsg token url emptyDecoder

---
--- Droplets - Just enough to get their IP addresses
---

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

type alias DropletsResult =
    Result Error (List Droplet)

dropletsResToDroplets : Result Error DropletsRes -> DropletsResult
dropletsResToDroplets res =
    case res of
        Err error -> Err error
        Ok dropletsRes ->
            Ok dropletsRes.droplets

getDroplets : String -> (DropletsResult -> msg) -> Cmd msg
getDroplets token resultToMsg =
    sendGetRequest
        (\res -> resultToMsg <| dropletsResToDroplets res)
        token dropletsUrl dropletsResDecoder
