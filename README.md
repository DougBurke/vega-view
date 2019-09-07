# vega-view

I am not 100% convinced this is a worthwhile project, but let's see how
it goes.

The aim is to make it easy to view
[Vega](https://vega.github.io/vega/)
and
[Vega-Lite](https://vega.github.io/vega-lite/)
specifications - i.e. the JSON representing a visualization - as
a visualization. It relies on
[Vega Embed](https://github.com/vega/vega-embed) to do all
the hard work, and just provides a basic web server that will list the
files in a given directory and, when selected, create the
call to Vega Embed.

## License

This is released under a BSD3 license.

## Usage

The server - called `vega-view` - should be run from the directory
containing the specifications to view. It then provides a web server
on port 8082 that can be used to view them at the URL

    http://localhost:8082/display/

## GHC support

This is currently a **very basic** application, so will hopefully build
against a wide variety of GHC installations. There has been /no/ testing
on Windows.

## Bugs and Issues

Please use the [issues list](https://github.com/DougBurke/vega-view/issues)
to report any problems.
