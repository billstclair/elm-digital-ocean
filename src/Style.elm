----------------------------------------------------------------------
--
-- Style.elm
-- The CSS Stylesheet for the elm-digital-ocean
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Style exposing ( style, SClass(..), SId(..), id, class )

import Css exposing (Sel(..))
import Html.Attributes

type SClass
    = ErrorClass
    | PrettyTable
    | AutoMargins
    | Centered

type SId
    = OuterDiv
    | TopInputId
    | FooterId

imports : List String
imports =
    []

rule : a -> b -> { selectors : a, descriptor : b }
rule selectors descriptor =
    { selectors = selectors
    , descriptor = descriptor
    }

rules =
    [ rule
        [ Id OuterDiv ]
        [ ( "width", "40em" )
        ]
    , rule
        [ Id TopInputId ]
        [ ( "margin-bottom", "0.5em" )
        ]
    , rule
        [ Id FooterId ]
        [ ( "margin-top", "1em" )
        ]
    , rule
        [ Class ErrorClass ]
        [ ( "color", "red" )
        ]
    , rule
        [ Class AutoMargins ]
        [ ( "margin-left", "auto" )
        , ( "margin-right", "auto" )
        ]
    , rule
        [ Class Centered ]
        [ ( "text-align", "center" )
        ]
    , rule
        [ Class PrettyTable ]
        [ ( "background", "white" )
        , ( "border-collapse", "collapse" )
        ]
    , rule
        [ Descendant (Type "th") (Class PrettyTable) ]
        [ ( "border", "1px silver solid" )
        , ( "padding", "0.2em" )
        , ( "background", "gainsboro" )
        , ( "text-align", "center" )
        ]
    , rule
        [ Descendant (Type "td") (Class PrettyTable) ]
        [ ( "border", "1px silver solid" )
        , ( "padding", "0.2em" )
        , ( "text-align", "left" )
        ]
    , rule
        [ Descendant (Type "tr:nth-child(odd)") (Class PrettyTable) ]
        [ ( "background-color", "#f2f2f2" )
        ]
    ]

stylesheet =
    Css.stylesheet imports rules


-- This is for inclusion at the beginning of the outermost div

style =
    Css.style [ Html.Attributes.scoped True ] stylesheet


-- For use in the attributes of Html elements
-- E.g. id Board

id =
    stylesheet.id

class =
    stylesheet.class
