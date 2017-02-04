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

import DigitalOcean exposing (AccountInfoResult)

type alias Account =
    { name : String
    , token : String
    , info : Maybe AccountInfoResult
    }

testAccounts : List Account
testAccounts =
    [ -- Add your private accounts here during development.
    ]
