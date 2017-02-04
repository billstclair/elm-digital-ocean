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

module DigitalOcean exposing (AccountInfo, getAccount)

import Json.Decode as JD exposing (field, Decoder)
import Json.Encode as JE exposing (Value)
import Http exposing (Error)

baseUrl : String
baseUrl =
    "https://api.digitalocean.com/v2/"

accountUrl : String
accountUrl =
    baseUrl ++ "account"

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

-- This decodes the return from the accountUrl request
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
        (\res -> resultToMsg (accountResToInfo res))
        token accountUrl accountResDecoder 

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
