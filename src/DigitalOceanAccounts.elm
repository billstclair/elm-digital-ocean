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

type alias Account =
    { name : String
    , token : String
    }

testAccounts : List Account
testAccounts =
    [ Account
          "wws-read"
          ""
    ]
