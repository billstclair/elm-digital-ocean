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

module DigitalOcean exposing ( AccountInfo, getAccount
                             , Domain, getDomains, getDomain
                             , DomainRecord, getDomainRecords, getDomainRecord
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
    Http.send resultToMsg (makeGetRequest token url decoder)

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

accountResToInfo : Result Error AccountRes -> Result Error AccountInfo
accountResToInfo res =
    case res of
        Err error -> Err error
        Ok accountRes ->
            Ok accountRes.account

getAccount : String -> (Result Error AccountInfo -> msg) -> Cmd msg
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

domainsResToDomains : Result Error DomainsRes -> Result Error (List Domain)
domainsResToDomains res =
    case res of
        Err error -> Err error
        Ok domainsRes ->
            Ok domainsRes.domains

getDomains : String -> (Result Error (List Domain) -> msg) -> Cmd msg
getDomains token resultToMsg =
    sendGetRequest
        (\res -> resultToMsg <| domainsResToDomains res)
        token domainsUrl domainsResDecoder 

getDomain : String -> String -> (Result Error Domain -> msg) -> Cmd msg
getDomain token domain resultToMsg =
    let url = domainUrl domain
    in
        sendGetRequest resultToMsg token url domainDecoder

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

domainRecordsResToDomainRecords : Result Error DomainRecordsRes -> Result Error (List DomainRecord)
domainRecordsResToDomainRecords res =
    case res of
        Err error -> Err error
        Ok domainRecordsRes ->
            Ok domainRecordsRes.domainRecords

getDomainRecords : String -> String -> (Result Error (List DomainRecord) -> msg) -> Cmd msg
getDomainRecords token domain resultToMsg =
    sendGetRequest
        (\res -> resultToMsg <| domainRecordsResToDomainRecords res)
        token (domainRecordsUrl domain) domainRecordsResDecoder

getDomainRecord : String -> String -> Int -> (Result Error DomainRecord -> msg) -> Cmd msg
getDomainRecord token domain id resultToMsg =
    sendGetRequest
        resultToMsg token (domainRecordUrl domain id) domainRecordDecoder
