{-# LANGUAGE OverloadedStrings #-}

{-

Usage:

   ./vega-view

will create web pages at

  http://localhost:n/
  http://localhost:n/display/

where n is 8082 unless the PORT environment variable is set to an
integer, in which case that will be used.

The top-level page can be used to drag-and-drop specifications and
view them, and supports several modes:

  - add to start
  - add to end
  - only show the current visualization

whereas the display/ directory lets you view any Vega and Vega-Lite
specfications in the working directory (or sub-directories), either
"in line" (i.e. in the page) or as a separate page.

The code could be refactored to be a SPA, but does it need to be?

-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(String, Object), Object
                  , (.=)
                  , eitherDecode', encode, object)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Version (showVersion)
import Network.HTTP.Types (status404)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), splitFileName, takeFileName)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty (ScottyM, ActionM
                  , get, html, json
                  , notFound, param
                  , redirect, regex
                  , status, scotty
                  , text)

import Paths_vega_view (version)

pageTitle :: H.Html
pageTitle = (H.span ! A.id "title") "Vega and Vega-Lite viewer"

homeLink :: H.Html
homeLink = (H.a ! A.id "homeLink" ! A.href "/") "Home"


toCSS, toJS :: [H.Html] -> H.Html
toCSS = (H.style ! A.type_ "text/css") . mconcat
toJS = (H.script ! A.type_ "text/javascript") . mconcat


-- Represent a Vega or Vega-Lite sepcification, which has
-- to be a Javascript object. Other than checking that we
-- have an object, there is no other validation of the
-- JSON.
--
data Spec = Spec {
  specVis :: Object
  , specPath :: FilePath
    -- ^ the path to the file
  , specFileName :: String
    -- ^ the file name with no path
  }


-- Create HTML for the given specification; try to match embedSpec
-- JS routines.
--
createView ::
  Spec
  -- ^ This is assumed to be Vega or Vega-Lite specification, but
  --   no check is made.
  --
  --   The description field is used if present.
  -> String
  -- ^ The id for the Vega-Embed visualization div.
  -> H.Html
  -- ^ The Html code needed to display this visualization
  --   (assumes vega-embed is already available).
createView spec specId =
  let vis = specVis spec

      mDesc = case HM.lookup "description" vis of
                Just (String d) -> Just d
                _ -> Nothing

      jsCts = mconcat [ "const vdiv = document.getElementById('"
                      , H.toHtml specId
                      , "'); "
                      , "const vopts = { downloadFileName: '"
                      , H.toHtml (specFileName spec)
                      , "' }; "
                      , "vegaEmbed(vdiv, "
                      , H.toHtml (LB8.unpack (encode vis))
                      , ", vopts).then((result) => { "
                        -- it's almost like I'm making this up as I go along
                      , "resetLocationWidth(vdiv.parentElement.parentElement); "
                      , "}).catch((err) => { "
                      , "vdiv.appendChild(document.createTextNode(err)); "
                      , "vdiv.setAttribute('class', 'vega-error'); "
                      , "});"
                      ]

  in (H.div ! A.class_ "vizview") $ do
    -- unlike embedSpec JS routines, do not add close or hide buttons
    (H.p ! A.class_ "location") (H.toHtml ("File: " ++ specFileName spec))

    (H.div ! A.class_ "contents") $ do
      case mDesc of
        Just desc -> (H.p ! A.class_ "description") (H.toHtml desc)
        Nothing -> pure ()

      (H.div ! A.id (H.toValue specId)) ""

    (H.script ! A.type_ "text/javascript") jsCts


readJSON ::
  FilePath
  -- ^ The path to the file. This *must* be relative to the
  --   current working directory.
  -> IO (Either String Value)
readJSON infile = do
  ans <- try (LB8.readFile infile)
  pure $ case ans of
           Left e -> Left (showIOException e)
           Right v -> eitherDecode' v


showIOException :: IOException -> String
showIOException = show

  
readSpec ::
  FilePath
  -- ^ The path to the file. This *must* be relative to the
  --   current working directory.
  -> IO (Either String Spec)
readSpec infile = do
  cts <- either (Left . show) Right <$> readJSON infile
  pure $ case cts of
           Right (Object o) ->
             let (path, filename) = splitFileName infile
             in Right (Spec o path filename)
           Right _ -> Left "JSON was not an object"
           Left e -> Left e


-- | Return a HTML block (a div) that will dislay the visualization,
--   if the file is a JSON object (but not guaranteed to be a Vega or
--   Vega-Lite spec). The id of the visualization is based on the
--   file name, so it is assumed to be unique for the page.
--
makeSpec :: FilePath -> IO (Maybe H.Html)
makeSpec infile = do
  espec <- readSpec infile
  case espec of
    Left _ -> pure Nothing
    Right s -> pure (Just (createView s infile))


addTextJS, addTitleJS, addDescriptionJS :: [H.Html]

addTextJS = [ "function addText(parent, text) { "
            , "parent.appendChild(document.createTextNode(text)); "
            , "} "
            ]


addTitleJS = [ "function addTitle(div, contents, infile) { "
             , "const el = document.createElement('p'); "
             , "el.setAttribute('class', 'location'); "
             , "div.appendChild(el); "
             , "const close = document.createElement('span'); "
             , "close.setAttribute('class', 'close'); "
             , "el.appendChild(close); "
             , "close.addEventListener('click', (ev) => { "
             , "div.style.display = 'none'; "
             , "while (div.firstChild) { "
             , "div.removeChild(div.firstChild); "
             , "} "
             , "}); "
             , "const hide = document.createElement('span'); "
             , "hide.setAttribute('class', 'hide'); "
             , "el.appendChild(hide); "
             , "hide.addEventListener('click', (ev) => { "
             , "if (contents.style.display !== 'none') { "
             , "contents.style.display = 'none'; "
             , "hide.setAttribute('class', 'show'); } else { "
             , "contents.style.display = 'block'; "
             , "hide.setAttribute('class', 'hide'); } "
             , "}); "
             , "addText(el, 'File: ' + infile); "
             , "} "
             ]

addDescriptionJS =
  [ "function addDescription(div, spec) { "
  , "if (!spec.description || spec.description === '') { return; } "
  , "const el = document.createElement('p'); "
  , "el.setAttribute('class', 'description'); "
  , "addText(el, spec.description); "
  , "div.appendChild(el); "
  , "} "
  ]


-- Do we want to hide the "swoosh" icon when a visualization is shown?
--
dragJS :: H.Html
dragJS =
  let cts = [ "function preventDefault(event) { event.preventDefault(); } "
            , "window.addEventListener('dragenter', preventDefault, false); "
            , "window.addEventListener('dragover', preventDefault); "
            , "window.addEventListener('drop', handleDrop); "
            , "function handleDrop(ev) { "
            , "ev.preventDefault(); "
            , "if (ev.dataTransfer.items) { "
            , "for (var i = 0; i < ev.dataTransfer.items.length; i++) { "
            , "if (ev.dataTransfer.items[i].kind === 'file') { "
            , "readFromDrop(ev.dataTransfer.items[i].getAsFile()); "
            , "} } } else { "
            , "for (var i = 0; i < ev.dataTransfer.files.length; i++) { "
            , "readFromDrop(ev.dataTransfer.files[i]); "
            , "} } "
            , "} "
            , "function readFromDrop(file) { "
            , "if (file.type !== 'application/json') { return; } "
            , "const reader = new FileReader(); "
            , "reader.onload = (event) => { embedSpec(file.name, event.target.result); } "
            , "reader.onerror = (event) => { alert('Unable to read from ' + file.name); } "
            , "reader.readAsText(file); "
            , "}"
            , "function embedSpec(filename, filects) { "
            , "let spec;"
            , "try { "
            , "spec = JSON.parse(filects); "
            , "} catch (error) { "
            , "reportParseError(filename); "
            , "return; "
            , "} "
            , "const parent = document.getElementById('vizlist'); "
            , "if (addMode === 'single') { "
            , "while (parent.firstChild) { "
            , "parent.removeChild(parent.firstChild);"
            , "} }"
            , "const div = document.createElement('div'); "
            , "div.setAttribute('class', 'vizview'); "
            , "if (addMode === 'top') { "
            , "parent.insertBefore(div, parent.firstChild); "
            , "} else { parent.appendChild(div); } "
            , "const contents = document.createElement('div'); "
            , "contents.setAttribute('class', 'contents'); "
            , "addTitle(div, contents, filename); "
            , "div.appendChild(contents); "
            , "addDescription(contents, spec); "
            , "const vdiv = document.createElement('div'); "
            , "contents.appendChild(vdiv); "
            , "const vopts = { downloadFileName: filename }; "
            , "vegaEmbed(vdiv, spec, vopts).catch((err) => { "
            , "vdiv.appendChild(document.createTextNode(err)); "
            , "vdiv.setAttribute('class', 'vega-error'); "
            , "}); "
            , "div.style.display = 'block';"
            , "} "
            , "var addMode = 'top'; " -- should read from HTML or set HTML
            , "document.getElementById('mode-select')."
            , "addEventListener('change', (ev) => { "
            , "const sel = ev.target; "
            , "for (var i = 0; i < sel.length; i++) { "
            , "if (sel[i].selected) { addMode = sel[i].value; break; } "
            , "} "
            , "}); "
            -- do we want to report the details of the error?
            -- be lazy and use an alert for now
            , "function reportParseError(filename) { "
            , "alert('Unable to parse ' + filename + ' as JSON'); "
            , "} "
            ] ++ addTextJS ++ addTitleJS ++ addDescriptionJS

      
      -- add newlines for debugging, although I've done something
      -- stupid to require this -- TODO track down
      ncts = concatMap (\n -> [n, "\n"]) cts
      jsCts = mconcat ncts
      
      -- jsCts = mconcat cts

  in (H.script ! A.type_ "text/javascript") jsCts


closeCSS, hideCSS, descriptionCSS, locationCSS :: [H.Html]
closeCSS = [ ".close { "
           , "background: rgba(230, 20, 20, 0.6); "
           , "border-radius: 50%; "
           , "cursor: pointer; "
           , "display: inline-block; "
           -- , "float: left; "
           , "height: 1em; "
           , "margin-right: 0.5em; "
           , "width: 1em; "
           , "} "
           , ".close:hover { "
           , "background: rgba(230, 20, 20, 1); "
           , "} "
           ]

hideCSS = [ ".hide { "
          , "border-left: 0.5em solid transparent; "
          , "border-right: 0.5em solid transparent; "
          , "border-top: 1em solid rgba(255, 165, 0, 0.6); "
          , "cursor: pointer; "
          , "display: inline-block; "
          -- , "float: left; "
          , "height: 0; "
          , "margin-right: 0.5em; "
          , "width: 0; "
          , "} "
          , ".show { "
          , "border-left: 0.5em solid transparent; "
          , "border-right: 0.5em solid transparent; "
          , "border-bottom: 1em solid rgba(255, 165, 0, 0.6); "
          , "cursor: pointer; "
          , "display: inline-block; "
          -- , "float: left; "
          , "height: 0; "
          , "margin-right: 0.5em; "
          , "width: 0; "
          , "} "
          , ".hide:hover { "
          , "border-top: 1em solid rgba(255, 165, 0, 1); "
          , "} "
          , ".show:hover { "
          , "border-bottom: 1em solid rgba(255, 165, 0, 1); "
          , "} "
          ]

descriptionCSS = [ "p.description { "
                 , "text-align: center; "
                 , "}"
                 ]

-- combine location and contents here
--
locationCSS = [ "p.location { "
              , "background: rgba(0, 0, 0, 0.2);"
              , "font-weight: bold; "
              , "margin: -1em; "
              , "margin-bottom: 0; "
              , "padding: 0.5em; "
                -- add a little horizontal space before the end of the "window"
                -- for when the visualization is minimised
              , "padding-right: 1.5em; "
              , "} "
              , "div.contents { "
              , "margin: 0; "
              , "margin-top: 1em; "
              , "} "
              ]


pageSetupCSS :: [H.Html]
pageSetupCSS = [ "body { margin: 0; } "
               , "#infobar { "
               , "background: rgb(120, 120, 200); "
               , "color: white; "
               , "font-family: sans-serif; "
               , "padding: 0.5em; "
               , "} "
               , "#infobar #title { "
               , "font-size: 150%; "
               , "font-variant-caps: small-caps; "
               , "margin-right: 2em; "
               , "} "
               , "#infobar #homeLink { "
               , "color: white; "
               , "text-decoration: none; "
               , "} "
               , "#homeLink:hover { "
               , "border-bottom: 2px solid white; "
               , "} "
               , "#mainbar { "
               , "padding: 1em; "
               , "} "
               ]


-- not convinced using the header color is a good thing to indicate
-- an error; should be visually distinct
--
vegaErrorCSS :: [H.Html]
vegaErrorCSS = [ ".vega-error { "
               , "background: rgba(120, 120, 200); "
               , "color: white; "
               , "font-family: monospace; "
               , "font-size: 150%; "
               , "font-weight: bold; "
               , "padding: 0.5em; "
               , "} "
               ]


vizCSS :: [H.Html]
vizCSS = [ ".vizview { "
         , "background-color: white; "
         , "border: 2px solid rgba(0, 0, 0, 0.4); "
         , "border-radius: 0.5em; "
         , "padding: 1em; "
         , "} "
         , ".vizview:hover { "
         , "border-color: rgba(0, 0, 0, 0.8); "
         , "box-shadow: 4px 4px 8px rgba(0, 0, 0, 0.2); "
         , "} "
         ]


-- Handle header / main areas of the page
sectionsCSS :: [H.Html]
sectionsCSS = [ "#infobar label { "
              , "margin-right: 0.5em; "
              , "}"
              , "#mainbar { "
              , "padding: 1em; "
              , "} "
              , "#mainbar #swoosh svg { "
              , "fill: rgba(120, 120, 200, 0.2); "
              , "height: 200px; "
              , "width: 200px; "
              , "} "
              ]


dragCSS :: H.Html
dragCSS =
  let cts = pageSetupCSS ++
            [ ".vizview { "
            , "float: left; "
            , "margin: 0.5em; "
            , "} "
            ] ++ closeCSS ++ hideCSS ++ descriptionCSS ++
            locationCSS ++ vegaErrorCSS ++ sectionsCSS ++ vizCSS

  in toCSS cts


indexPage :: H.Html
indexPage =
  H.docTypeHtml ! A.lang "en-US" $ do
    H.head $ do
      H.title "View a Vega or Vega-Lite specification"
      vegaEmbed
      dragCSS

    H.body $ do
      (H.div ! A.id "infobar") $ do
        pageTitle
        (H.label ! A.for "mode-select") "Drop mode:"
        (H.select ! A.id "mode-select") $ do
          (H.option ! A.value "single") "Single"
          -- TODO: can get selected="" with this, but not selected as a
          --       stand-alone attribute
          (H.option ! A.value "top" ! A.selected "") "Add at start"
          (H.option ! A.value "bottom") "Add to end"

      let elink url = H.a ! A.href url ! A.target "_blank"

      (H.div ! A.id "mainbar") $ do
        H.p (mconcat [ "This is version "
                     , H.toHtml (showVersion version)
                     , " of "
                     , elink "https://github.com/DougBurke/vega-view#readme"
                       "vega-view"
                     , ". Go to "
                     , (H.a ! A.href "/display/") "/display/"
                     , " to see the available visualizations, or "
                     , "drag files containing "
                     , elink "https://vega.github.io/vega-lite/" "Vega"
                     , " or "
                     , elink "https://vega.github.io/vega-lite/" "Vega-Lite"
                     , " visualizations onto this page to view them."
                     ])

        (H.div ! A.id "vizlist") ""

        -- embed the SVG directly so we can style it
        (H.div ! A.id "swoosh")
          (B.preEscapedText swooshSVG)

      -- since too lazy to set up an onload handler, stick all the JS
      -- here
      dragJS
      

-- Return the directories in ths directory, and the JSON files we
-- can try displaying. All other files are dropped.
--
getFileContents ::
  FilePath
  -> IO ([FilePath], [(FilePath, H.Html)])
  -- ^ First we list the directories in ths directory, and then the
  --   displayable contents. Either list can be empty.
  --
getFileContents indir = do

  infiles <- map (indir </>) . sort <$> listDirectory indir
  dirFlags <- mapM doesDirectoryExist infiles

  let files = zip dirFlags infiles

      -- these are not expected to be large lists so any duplicated effort
      -- is not large; also, rely on the power of the compiler to fuse
      -- everything
      --
      dirNames = map snd (filter fst files)
      otherNames = map snd (filter (not . fst) files)

      go f = do
        mspec <- makeSpec f
        case mspec of
          Just h -> pure (Just (f, h))
          _ -> pure Nothing

  mspecs <- mapM go otherNames

  let specs = catMaybes mspecs

  pure (dirNames, specs)


pageLink :: FilePath -> H.Html
pageLink infile =
  let toHref = H.toValue ("/display" </> infile)
      toText = H.toHtml (takeFileName infile)
  in (H.a ! A.href toHref ! A.class_ "pagelink") toText

makeLi :: FilePath -> H.Html
makeLi infile = H.li (pageLink infile)


embedLink :: FilePath -> H.Html
embedLink infile =
  let toHref = H.toValue ("/embed" </> infile)
      hdlr = mconcat [ "embed('", toHref, "');" ]

      -- label = ">>"  -- H.toHtml (takeFileName infile)
      label = B.preEscapedText arrowSVG

  in (H.a ! A.href "#" ! A.onclick hdlr ! A.class_ "embedlink") label


-- Nothing to see here; slightly different if base directory or not
emptyDir :: FilePath -> ActionM ()
emptyDir indir =
  let page = (H.docTypeHtml ! A.lang "en-US") $ do
        H.head $ do
          H.title (H.toHtml ("Files to view: " ++ indir))
          toCSS pageSetupCSS

        H.body $ do
          (H.div ! A.id "infobar") $ do
            pageTitle
            homeLink

          (H.div ! A.id "mainbar") $
            if indir == "."
              then H.p "There is nothing to see in the base directory!"
              else do
                labelDirectory True indir
                H.p "There is nothing to see here!"

  in html (renderHtml page)


-- Code to display a specification inline
--
-- Would be a lot nicer to embed the JS code from a file at build time
-- or to load at run time.
--
-- TODO: set max width/height of the visualization window so that
--       overflow works? Not obvious best way to do this.
--
-- TODO: allow the user to drag the window around
--
embedJS :: H.Html
embedJS =
  let cts = [ "function embed(path) { "
            , "var req = new XMLHttpRequest(); "
            , "req.addEventListener('load', embedSpec); "
            , "req.addEventListener('error', embedSpec); "
            , "req.responseType = 'json'; "
            , "req.open('GET', path); "
            , "req.send(); "
            , "} "
            , "function embedSpec(e) { "
            , "const div = document.getElementById('vizview'); "
            , "while (div.firstChild) { "
            , "div.removeChild(div.firstChild);"
            , "} "
            , "const tgt = e.target; "
            , "if (tgt.status == 200) { "
            , "const vopts = { downloadFileName: tgt.response.infile }; "
            , "const contents = document.createElement('div'); "
            , "contents.setAttribute('class', 'contents'); "
            , "addTitle(div, contents, tgt.response.infile); "
            , "div.appendChild(contents); "
            , "addDescription(contents, tgt.response.spec); "
            , "const vdiv = document.createElement('div'); "
            , "contents.appendChild(vdiv); "
            , "vegaEmbed(vdiv, tgt.response.spec, vopts).then((result) => { "
            , "resetLocationWidth(div); "
            , "}).catch((err) => { "
            , "vdiv.appendChild(document.createTextNode(err)); "
            , "vdiv.setAttribute('class', 'vega-error'); "
            , "}); "
            , "} else { "
            , "addText(div, 'There was an error when loading the specification. '); "
            , "addText(div, 'Is the vega-view web server still running?'); "
            , "} "
            , "div.style.display = 'block';"
            , "} "
            ] ++ addTextJS ++ addTitleJS ++ addDescriptionJS ++
            resetLocationJS

  in (H.script ! A.type_ "text/javascript") (mconcat cts)


embedCSS :: H.Html
embedCSS =
  let cts = [ ".vizview { "
            , "display: none; "
            , "overflow: auto; "
            , "} "
            , ".vizlist { "
            , "display: flex; "
            , "justify-content: space-around; "
            , "}"
            , "#visualizations { "
            , "float: left; "
            , "margin-right: 1em; "
            , "} "
            , "#visualizations h2 { "
            , "margin-bottom: 0; "
            , "margin-top: 0; "
            , "} "
            , "#visualizations .pagelist { "
            , "display: grid; "
            , "grid-column-gap: 0.4em; "
            , "grid-row-gap: 0.4em; "
            , "grid-template-columns: 1fr 2.5em; "
            , "margin-top: 1em; "
            , "} "
            , "#visualizations .pagelist a { "
            , "background: rgba(120, 120, 200, 0.8); "
            , "color: white; "
            , "font-family: sans-serif; "
            , "padding: 0.5em; "
            , "text-decoration: none; "
            , "} "
            , "#visualizations .pagelist a:hover { "
            , "background: rgba(120, 120, 200, 1); "
            , "box-shadow: 0.1em 0.1em 0.2em rgba(0, 0, 0, 0.4); "
            , "} "
            , ".pagelink { "
            , "} "
            , ".embedlink { "
            , "} "
            , ".embedlink svg { "
            , "fill: white; "
            , "height: 1.2em; "
            , "width: 1.2em; "
            , "} "
            ] ++ closeCSS ++ hideCSS ++ descriptionCSS ++
            locationCSS ++ pageSetupCSS ++ vegaErrorCSS ++ vizCSS

  in toCSS cts


labelDirectory ::
  Bool
  -- ^ If True then the parent directory is ../ above the input directory
  --   and the link label includes the term "parent".
  -> FilePath
  -- ^ The directory name for display to the user
  -> H.Html
labelDirectory goUp dirName = do
  H.p (H.toHtml ("Directory: " ++ dirName))
  let baseURL = "/display" </> dirName
      url = if goUp then baseURL </> ".." else baseURL
      label = mconcat [ if goUp then "parent " else "", "directory" ]

  H.p (mconcat [ "Go to "
               , (H.a ! A.href (H.toValue url)) label
               , "."
               ])


showDir ::
  FilePath
  -> ([FilePath], [(FilePath, H.Html)])
  -> ActionM ()
showDir indir (subdirs, files) =
  let atTop = indir == "."
  
      page = (H.docTypeHtml ! A.lang "en-US") $ do
        H.head $ do
          H.title (H.toHtml ("Files to view: " ++ indir))
          unless (null files) $ do
            vegaEmbed
            embedJS
          embedCSS

        H.body $ do
          (H.div ! A.id "infobar") $ do
            pageTitle
            homeLink

          (H.div ! A.id "mainbar") $ do
            unless atTop (labelDirectory True indir)

            unless (null subdirs) $ do
              H.h2 "Sub-directories"
              H.ul (forM_ subdirs makeLi)

            -- let's see how this basic setup works
            --
            -- TODO: might be nice to let users easily skip to next or
            --       previous visualization when viewing one.
            --
            unless (null files) $ do
              (H.div ! A.id "visualizations") $ do
                H.h2 "Visualizations"
                (H.div ! A.class_ "pagelist") $
                  forM_ files $ \(f, _) -> do
                    pageLink f
                    embedLink f

              (H.div ! A.class_ "vizlist") $
                (H.div ! A.class_ "vizview" ! A.id "vizview") ""

  in html (renderHtml page)


dirPage :: FilePath -> ActionM ()
dirPage indir = do

  files <- liftIO (getFileContents indir)
  case files of
    ([], []) -> emptyDir indir
    _ -> showDir indir files


-- load up vega embed
vegaEmbed :: H.Html
vegaEmbed =
  let load n = H.script ! A.src (mconcat [ "https://cdn.jsdelivr.net/npm/"
                                         , n])

  in do
    load "vega@5" ""
    load "vega-lite@3" ""
    load "vega-embed@4" ""


pageCSS :: H.Html
pageCSS =
  let cts = pageSetupCSS ++
            [ ".vizview { "
            , "overflow: auto; "
            , "} "
            , ".vizlist { "
            , "display: flex; "
            , "justify-content: space-around; "  -- not convinced about this
            , "} "
            ] ++ descriptionCSS ++
            locationCSS ++ vegaErrorCSS ++ sectionsCSS ++ vizCSS

  in toCSS cts


-- change the "title" bar, containing the loction, but not any description,
-- as want that to stay bouded by the starting bounding box, I think
-- (so that it doesn't appear off-screen initially for a short-enough
--  description, if centered).
--
--  I had originally thought I would have to call resetLocationWidth on
--  a page resize, but it doesn't need to be, since the title is never
--  going to need to be larger than the value the scrollWidth of the
--  visualization.
--
pageJS :: H.Html
pageJS = toJS resetLocationJS


resetLocationJS :: [H.Html]
resetLocationJS =
  [ "function resetLocationWidth(div) { "
  , "const locs = div.getElementsByClassName('location'); "
  , "if (locs.length === 0) { console.log('DBG: no location'); console.log({div}); return; } "
  , "const loc = locs[0]; "
  , "loc.style.width = div.scrollWidth + 'px'; "
  , "} "
  ]


showPage :: FilePath -> ActionM ()
showPage infile = do
  espec <- liftIO (readSpec infile)
  case espec of
    Left emsg -> do
      -- This is not very informative, but at least provides the user
      -- with some information.  The assumption is that this is running
      -- "locally" so we do not have to worry about any possible
      -- information leak from this.
      --
      text (LT.pack emsg)
      errorStatus

    Right spec ->
      let contents = createView spec "vega-vis"
          page = (H.docTypeHtml ! A.lang "en-US") $ do
            H.head $ do
              H.title "View a spec"
              vegaEmbed
              pageCSS
              pageJS

            H.body $ do
              (H.div ! A.id "infobar") $ do
                pageTitle
                homeLink

              (H.div ! A.id "mainbar") $ do
                labelDirectory False (specPath spec)
                (H.div ! A.class_ "vizlist") contents

      in html (renderHtml page)
    

displayPage :: FilePath -> ActionM ()
displayPage infile = do
  isDir <- liftIO (doesDirectoryExist infile)
  if isDir
    then dirPage infile
    else showPage infile
    

-- Return data needed to display this file.
--
embedPage :: FilePath -> ActionM ()
embedPage infile = do
  espec <- liftIO (readSpec infile)
  case espec of
    Right (Spec o path filename) -> json (object [ "spec" .= Object o
                                                 , "path" .= path
                                                 , "infile" .= filename
                                                 ])
    _ -> errorStatus


-- embed https://commons.wikimedia.org/wiki/File:Curved_Arrow.svg
-- which is licensed under the Creative Commons CC0 1.0 Universal
-- Public Domain Dedication
--
swooshSVG :: T.Text
swooshSVG =
  mconcat
        [ "<svg"
        , "   xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
        , "   xmlns:cc=\"http://creativecommons.org/ns#\""
        , "   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""
        , "   xmlns:svg=\"http://www.w3.org/2000/svg\""
        , "   xmlns=\"http://www.w3.org/2000/svg\""
        , "   xml:space=\"preserve\""
        , "   enable-background=\"new 0 0 595.28 841.89\""
        , "   viewBox=\"0 0 776.09175 693.66538\""
        , "   height=\"693.66541\""
        , "   width=\"776.0918\""
        , "   y=\"0px\""
        , "   x=\"0px\""
        , "   id=\"Layer_1\""
        , "   version=\"1.1\"><metadata"
        , "     id=\"metadata11\"><rdf:RDF><cc:Work"
        , "         rdf:about=\"\"><dc:format>image/svg+xml</dc:format><dc:type"
        , "           rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" /><dc:title></dc:title></cc:Work></rdf:RDF></metadata><defs"
        , "     id=\"defs9\" /><g"
        , "     transform=\"matrix(2.7190747,0,0,3.1037754,-326.9763,-1172.9045)\""
        , "     id=\"g3\"><path"
        , "       style=\"clip-rule:evenodd;fill-rule:evenodd\""
        , "       id=\"path5\""
        , "       d=\"m 130.838,381.118 c 1.125,28.749 5.277,54.82 12.695,78.018 7.205,22.53 18.847,40.222 36.812,53.747 52.018,39.16 153.369,16.572 153.369,16.572 l -4.632,-32.843 72.918,42.778 -58.597,58.775 -3.85,-27.303 c 0,0 -100.347,18.529 -163.905,-34.881 -37.659,-31.646 -53.293,-84.021 -51.593,-153.962 0.266,-0.247 4.728,-0.908 6.783,-0.901 z\" /></g></svg>"
        ]


-- This is from
-- https://fontawesome.com/icons/arrow-alt-circle-right?style=solid
-- version 5.10.2, free version
--
arrowSVG :: T.Text
arrowSVG = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 512 512\"><path d=\"M256 8c137 0 248 111 248 248S393 504 256 504 8 393 8 256 119 8 256 8zM140 300h116v70.9c0 10.7 13 16.1 20.5 8.5l114.3-114.9c4.7-4.7 4.7-12.2 0-16.9l-114.3-115c-7.6-7.6-20.5-2.2-20.5 8.5V212H140c-6.6 0-12 5.4-12 12v64c0 6.6 5.4 12 12 12z\"/></svg>"


errorStatus :: ActionM ()
errorStatus = status status404


webapp :: ScottyM ()
webapp = do

  get "/" (redirect "/index.html")
  get "/index.html" (html (renderHtml indexPage))

  -- TODO: catch errors
  get "/display/" (dirPage ".")

  get (regex "^/display/(.+)$") $ do
    infile <- param "1"
    displayPage infile

  get (regex "^/embed/(.+)$") $ do
    infile <- param "1"
    embedPage infile

  notFound errorStatus


-- for now assume current directory  
main :: IO ()
main = do
  mPortStr <- lookupEnv "PORT"
  let port = case read <$> mPortStr of
               Just n -> n
               _ -> 8082

  scotty port webapp
