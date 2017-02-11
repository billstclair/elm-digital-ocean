This directory contains a webapp that illustrates use of the [DigitalOcean](../src/DigitalOcean.elm) module.

First, you need to generate token(s) for your account(s). You can do this on the "[API/Tokens](https://cloud.digitalocean.com/settings/api/tokens)" page. This webapp persistently stores the tokens in your browser's database, but they never leave your machine (except when sent over the encrypted wire to the API server).

During development, you can use Elm reactor:

    cd .../elm-digital-ocean/examples
    elm reactor

Then aim your browser at [localhost:8000/src/reactor-webapp.elm](http://localhost:8000/src/reactor-webapp.elm).

You may want to edit [DigitalOceanAccounts.elm](src/DigitalOceanAccounts.elm), to add your accounts, so you don't have to recreate those account entries every time you rebuild the code.

To build the live app:

    cd .../elm-digital-ocean/examples
    bin/build-site
    
To upload the live app to my web site (won't work for anybody but me):

    bin/update-site
    
The app is live at [lisplog.org/elm-do](https://lisplog.org/elm-do/).

`bin/rsyncit` is a synchronization script I've been using for years. It is documented [here](https://steemit.com/hacking/@billstclair/rsyncit).

Bill St. Clair &lt;<billstclair@gmail.com>&gt;<br/>
10 February 2017
