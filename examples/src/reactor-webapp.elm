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

import SharedUI exposing ( Model, Msg, Property
                         , view, update, subscriptions
                         )
import DigitalOceanAccounts

import Html

-- This is a port in port-webapp.elm
setProperty : (String, Maybe String) -> Cmd Msg
setProperty (key, value) =
    Cmd.none

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

initialProperties : List Property
initialProperties =
    let json = DigitalOceanAccounts.encodeAccounts DigitalOceanAccounts.testAccounts
    in
        [ ( SharedUI.accountsProperty, json ) ]

init : ( Model, Cmd Msg )
init =
    SharedUI.init initialProperties setProperty
