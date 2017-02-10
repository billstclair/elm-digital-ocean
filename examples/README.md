This directory contains a webapp that illustrates use of the [../src/DigitalOcean.elm](DigitalOcean) module.

First, you need to generate token(s) for your account(s). You can do this on the "[API/Tokens](https://cloud.digitalocean.com/settings/api/tokens)" page. This webapp persistently stores the tokens in your browser's database, but they never leave your machine (except when sent over the encrypted wire to the API server).

The persistent account store isn't yet implemented. During development, you can use Elm reactor:

    cd .../elm-digital-ocean/examples
    elm reactor

Then aim your browser at [localhost:8000/src/elm-digital-ocean.elm](http://localhost:8000/src/elm-digital-ocean.elm).

Bill St. Clair &lt;<billstclair@gmail.com>&gt;<br/>
10 February 2017
