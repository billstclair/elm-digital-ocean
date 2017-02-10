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

import SharedUI exposing ( Property, Model, Msg
                         , view, update, subscriptions
                         )
import DigitalOceanAccounts exposing (Account)

import Html
import List.Extra as LE
import Json.Decode as JD exposing (Decoder)
import Debug exposing (log)

-- port defined in site/index.html
port setProperty : (String, Maybe String) -> Cmd a

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
    SharedUI.init properties setProperty
