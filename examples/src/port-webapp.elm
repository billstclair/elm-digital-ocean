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

-- This is a port in port-webapp.elm
port setProperty : (String, Maybe String) -> Cmd a

accountSetter : String -> Maybe Account -> Cmd Msg
accountSetter name account =
    Cmd.none                    --TODO

main : Program (List (String, String)) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : List (String, String) -> ( Model, Cmd Msg )
init properties =
    SharedUI.init DigitalOceanAccounts.testAccounts accountSetter
