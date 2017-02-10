----------------------------------------------------------------------
--
-- reactor-webapp.elm
-- Top level code for debugging version of Digital Ocean webapp.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
-- This will work in elm-reactor.
-- Use port-webapp.elm for the live page with persistent accounts.
--
----------------------------------------------------------------------

module DigitalOceanReactor exposing (..)

import SharedUI exposing ( Model, Msg, AccountSetter
                         , view, update, subscriptions
                         )
import DigitalOceanAccounts

import Html

-- This is a port in port-webapp.elm
accountSetter : AccountSetter
accountSetter key value =
    Cmd.none

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : ( Model, Cmd Msg )
init =
    SharedUI.init DigitalOceanAccounts.testAccounts accountSetter
