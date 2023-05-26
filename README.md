# Domain Modelling Workshop

This is a Haskell implementation of the server one has to implement for the https://domainmodelling.dev workshop by Michel Grootjans and Thomas Coopman.

## Necessary tools

To get the Haskell Compiler, use [ghcup](https://www.haskell.org/ghcup/). Version 9.4.5 is used in this project. You will also need the `cabal` CLI tool that usually comes with it.

## How to use

There is a Makefile that is documented. Just type `make` to see the available commands and what they do.

To run the server (this will fetch dependencies and build, if this is the first run), type:

```bash
$ make start
# Starting server on http://localhost:8081
```

To run the tests, type:

```bash
$ make tests
```

To run the code linters and formatters, run:

```bash
$ make lint
# and
$ make style
```
You will need to run following command line:

```bash
$ cabal install cabal-fmt fourmolu hlint apply-refact
```

## How to read

* The web server and request handling happens in `./app/Main.hs`
* Business logic like validation, price computation, and types happen in `./src`
* The tests happen in `./test`

Enjoy!
