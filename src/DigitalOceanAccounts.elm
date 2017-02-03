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

module DigitalOceanAccounts exposing ( Account, testAccounts )

import DigitalOcean exposing (AccountInfo)

type alias Account =
    { name : String
    , token : String
    , info : Maybe AccountInfo
    }

testAccounts : List Account
testAccounts =
    [ -- Put your testing account here
    ]
