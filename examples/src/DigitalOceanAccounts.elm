----------------------------------------------------------------------
--
-- DigitalOceanAccounts.elm
-- Test accounts.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module DigitalOceanAccounts exposing ( Account, testAccounts
                                     , accountDecoder, decodeAccount
                                     , accountsDecoder, decodeAccounts
                                     , accountEncoder, encodeAccount
                                     , accountsEncoder, encodeAccounts
                                     )

import DigitalOcean exposing (AccountInfoResult)

import Json.Decode as JD exposing (field, Decoder)
import Json.Encode as JE exposing (Value)

accountDecoder : Decoder Account
accountDecoder =
    JD.map3
        Account
            (field "name" JD.string)
            (field "token" JD.string)
            (JD.succeed Nothing)

decodeAccount : String -> Result String Account
decodeAccount json =
    JD.decodeString accountDecoder json

accountsDecoder : Decoder (List Account)
accountsDecoder =
    JD.list accountDecoder

decodeAccounts : String -> Result String (List Account)
decodeAccounts json =
    JD.decodeString accountsDecoder json

accountEncoder : Account -> Value
accountEncoder account =
    JE.object
        [ ("name", JE.string account.name)
        , ("token", JE.string account.token)
        ]

encodeAccount : Account -> String
encodeAccount account =
    JE.encode 0 <| accountEncoder account

accountsEncoder : (List Account) -> Value
accountsEncoder accounts =
    JE.list <| List.map accountEncoder accounts

encodeAccounts : (List Account) -> String
encodeAccounts accounts =
    JE.encode 0 <| accountsEncoder accounts

type alias Account =
    { name : String
    , token : String
    , info : Maybe AccountInfoResult
    }

testAccounts : List Account
testAccounts =
    [ 
     {-
     Account
         "some-name"
         "123456789X123456789XX23456789XXX3456789XXXX456789V123456789VI234"
         Nothing
     -}
    ]
