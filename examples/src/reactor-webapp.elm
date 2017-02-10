----------------------------------------------------------------------
--
-- reactor-webapp.elm
-- Top level code for debugging version of Digital Ocean webapp.
-- This one will work in elm-reactor.
-- port-webapp.elm contains ports to make the accounts persistent.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module DigitalOceanWebapp exposing (..)

import SharedUI exposing ( Model, Msg, AccountSetter
                         , view, update, subscriptions
                         )
import DigitalOceanAccounts

import Html

-- This is a port in port-webapp.elm
accountSetter : AccountSetter
accountSetter (key, value) =
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
