# vega-view

I am not 100% convinced this is a worthwhile project, but let's see how
it goes. You can think of it as "I want
[vega desktop](https://github.com/vega/vega-desktop) but don't want
to install an electron application, so decided to write an inferior
product" project.

The aim is to make it easy to view
[Vega](https://vega.github.io/vega/)
and
[Vega-Lite](https://vega.github.io/vega-lite/)
specifications - i.e. the JSON representing a visualization - as
a visualization. It relies on
[Vega Embed](https://github.com/vega/vega-embed) to do all
the hard work, and just provides a basic web server that:

 - you can drag-and-drop files onto to view them;
 
 - and will list the files in a given directory and, when selected,
   view them inline or on a separate page.

![A Vega-Lite specification being dragged from a file browser and dropped onto the index page of the vega-view web server](https://raw.githubusercontent.com/DougBurke/vega-view/master/images/vega-view-drag-n-drop.gif)

## License

This is released under a BSD3 license.

## Usage

The server - called `vega-view` - should be run from the directory
containing the specifications to view. It then provides a web server
on port 8082 that can be used to view them at the URLs

    http://localhost:8082/
    http://localhost:8082/display/

The first page lets you drag-and-drop files onto the page to view
them. Thse second lets you browse the visualizations that are present
in the diectory where you started the application. Thse can either be
viewed as their own "page", or inline, which may be more useful when
you have multiple plots to view.

The aim is to be run in a a directory structure where most, if not
all, the files are Vega or Vega-Lite specifications. This means that
the web server tries to parse each file as JSON, which could cause
memory- or time- issues if there are large non-JSON files in the
directory tree.

## GHC support

This is currently a **very basic** application, so will hopefully build
against a wide variety of GHC installations. There has been /no/ testing
on Windows.

## Bugs and Issues

Please use the [issues list](https://github.com/DougBurke/vega-view/issues)
to report any problems.
