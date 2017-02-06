----------------------------------------------------------------------
--
-- Entities.elm
-- HTML entities, for fun and profit.
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Entities exposing (..)

import String
import Char

stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ (Char.fromCode code) ]

nbsp : String
nbsp =
    stringFromCode 160  -- \u00A0

copyright : String
copyright =
    stringFromCode 169  -- \u00A9

checkmark : String
checkmark =
    stringFromCode 10004 -- \u2714

ellipsis : String
ellipsis =
    stringFromCode 8230 -- \u2026
