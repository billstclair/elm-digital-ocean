----------------------------------------------------------------------
--
-- port-webapp.elm
-- Top level code for Digital Ocean webapp with persistent accounts.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
-- Use reactor-webapp.elm for debugging in elm-reactor.
--
----------------------------------------------------------------------

port module DigitalOceanWebapp exposing (..)

import SharedUI exposing ( Model, Msg, AccountSetter
                         , view, update, subscriptions
                         )
import DigitalOceanAccounts exposing (Account)

import Html
import List.Extra as LE
import Json.Decode as JD exposing (Decoder)
import Debug exposing (log)

-- ports, defined in site/index.html
port setProperty : (String, Maybe String) -> Cmd a

acctPrefix : String
acctPrefix =
    "acct."

acctPrefixLength : Int
acctPrefixLength =
    String.length acctPrefix

accountProperty : String -> String
accountProperty name =
    acctPrefix ++ name

isAccountProperty : String -> Bool
isAccountProperty property =
    (String.left acctPrefixLength property) == acctPrefix

accountName : String -> Maybe String
accountName property =
    if isAccountProperty property then
        Just <| String.dropLeft acctPrefixLength property
    else
        Nothing

accountSetter : String -> Maybe Account -> Cmd Msg
accountSetter name account =
    let json = case account of
                   Nothing -> Nothing
                   Just acct ->
                       Just <| DigitalOceanAccounts.encodeAccount acct
    in
        setProperty (accountProperty name, json)

type alias Property =
    (String, String)

main : Program (List Property) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : List Property -> ( Model, Cmd Msg )
init properties =
    let pairs = List.filter (\a -> isAccountProperty <| Tuple.first a) properties
        jsons = List.map Tuple.second pairs
        results = List.map DigitalOceanAccounts.decodeAccount jsons
        accounts = List.foldl (\x res ->
                                   case x of
                                       Err _ -> res
                                       Ok account ->
                                           account :: res
                              )
                              [] results
    in
        SharedUI.init accounts accountSetter
