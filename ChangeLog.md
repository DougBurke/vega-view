# Changelog for vega-view

## 0.3.0.5

Improved error handling when displaying Vega or Vega-Lite specifications,
in that the error message from vega-embed is now shown to the viewer rather
than hidden away in the console log. It is not clear to me how useful these
error messages are, other than just to say "hey, something went wrong".

The filenames for the export-to-svg/png options now uses the specification
file name rather than the default "visualization". There is no attempt to
remove any suffixes from the file name, so "foo.vg.json" will end up as
"foo.vg.json.svg" (or .png).

Removed paths from file names in the directory listings, and also stopped
dropping their suffixes.

## 0.3.0.4

Fixed directory browsing: previously things didn't work out so well if
there were no Vega or Vega-Lite visualizations in the working directory,
but now you can properly recurse into sub-directories.

## 0.3.0.3

Oops: chaning from a bytestring to a text during development caused
"problems". There really should be a swooshy icon on the home page now,
and the a text rendering of the SVG...

## 0.3.0.2

Add a link back to the "home page" in the page header.

The home page has a swooshy icon on it, to try and visually indicate
that you can drag onto the page.

## 0.3.0.1

Tweak the styling on some of the pages to use the "new look"™.

## 0.3.0.0

Add drag-and-drop support to the main page. You can now drag in a file
(or multiple files) and have their contents be displayed. There is a mode
for selecting how new visualizations are added to the page: at the top
(default), at the bottom, or clear-out any previous plots.

## 0.2.0.2

Bump base minimum version to supporg ghc 8 or later, to save
hackage trying to build with older versions. There is no user-visible
change to version 0.2.

## 0.2.0.1

Internal change to get it to build with ghc 8.2 and 8.0.

## 0.2

The directory view now displays all the visualizations in a directory,
and hides those files that are not JSON. Selecting a visualization can
now open it in a new page (0.1 behavior) or inline (new).

## 0.1

Initial release
