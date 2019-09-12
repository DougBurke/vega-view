{-# LANGUAGE OverloadedStrings #-}

{-
A very-basic viewer for Vega and Vega-Lite specfications. You
can browse from the current working directory, and view any
individual specification via Vega Embed (or an error if it isn't
a JSON file or some other issue).

A drag-and-drop page could be added, as could and endpoint to which
you post a specification.

The code could be refactored to be a SPA.

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
import System.FilePath ((</>), takeBaseName, takeDirectory)
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


-- Represent a Vega or Vega-Lite sepcification, which has
-- to be a Javascript object. Other than checking that we
-- have an object, there is no other validation of the
-- JSON.
--
data Spec = Spec {
  specVis :: Object
  , specPath :: FilePath
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

      jsCts = mconcat [ "vegaEmbed('#"
                      , H.toHtml specId
                      , "', "
                      , H.toHtml (LB8.unpack (encode vis))
                      , ");"]

  in (H.div ! A.class_ "vizview") $ do
    -- unlike embedSpec JS routines, do not add a close button
    (H.p ! A.class_ "location") (H.toHtml ("File: " ++ specPath spec))
      
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
           Right (Object o) -> Right (Spec o infile)
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
    Right s -> pure (Just (createView s (takeBaseName infile)))


addTextJS, addTitleJS, addDescriptionJS :: [H.Html]

addTextJS = [ "function addText(parent, text) { "
            , "parent.appendChild(document.createTextNode(text)); "
            , "} "
            ]

-- TODO: should parent node be emoved from DOM on close? depends on
--       what page it is being used in
addTitleJS = [ "function addTitle(div, infile) { "
             , "const el = document.createElement('p'); "
             , "el.setAttribute('class', 'location'); "
             , "addText(el, 'File: ' + infile); "
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
            , "addTitle(div, filename); "
            , "addDescription(div, spec); "
            , "const vdiv = document.createElement('div'); "
            , "div.appendChild(vdiv); "
            , "vegaEmbed(vdiv, spec); "
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


closeCSS, descriptionCSS, locationCSS :: [H.Html]
closeCSS = [ ".close { "
           , "background: red; "
           , "border-radius: 50%; "
           , "cursor: pointer; "
           , "float: left; "
           , "height: 1em; "
           , "margin-right: 0.5em; "
           , "width: 1em; "
           , "}"
           ]

descriptionCSS = [ "p.description { "
                 , "text-align: center; "
                 , "}"
                 ]

locationCSS = [ "p.location { "
              , "background: rgba(0, 0, 0, 0.2);"
              , "font-weight: bold; "
              , "margin: -1em; "
              , "margin-bottom: 1em; "
              , "padding: 0.5em; "
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


dragCSS :: H.Html
dragCSS =
  let cts = pageSetupCSS ++
            [ ".vizview { "
            , "background: white; "
            , "border: 2px solid rgba(0, 0, 0, 0.6); "
            , "margin: 0.5em; "
            , "float: left; "
            , "padding: 1em; "
            , "} "
            , "#infobar label { "
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
            ] ++ closeCSS ++ descriptionCSS ++ locationCSS

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

  infiles <- sort <$> listDirectory indir
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


pageLink :: FilePath -> FilePath -> H.Html
pageLink indir infile =
  let toHref = H.toValue ("/display" </> indir </> infile)
  in (H.a ! A.href toHref) (H.toHtml infile)

makeLi :: FilePath -> FilePath -> H.Html
makeLi indir infile = H.li (pageLink indir infile)

makeParentLi :: FilePath -> H.Html
makeParentLi indir =
  let toHref = H.toValue ("/display" </> indir </> "..")
  in H.li ((H.a ! A.href toHref) "parent directory")

embedLink :: FilePath -> FilePath -> H.Html
embedLink indir infile =
  let toHref = H.toValue ("/embed" </> indir </> infile)
      hdlr = mconcat [ "embed('", toHref, "');" ]

  in (H.a ! A.href "#" ! A.onclick hdlr) (H.toHtml infile)


toCSS :: [H.Html] -> H.Html
toCSS = (H.style ! A.type_ "text/css") . mconcat


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
                H.p (H.toHtml ("Directory: " ++ indir))
                H.p "There is nothing to see here!"
                H.ul (makeParentLi indir)

  in html (renderHtml page)


-- Code to display a specification inline
--
-- Would be a lot nicer to embed the code from a file at build time
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
            , "addTitle(div, tgt.response.infile); "
            , "addDescription(div, tgt.response.spec); "
            , "const vdiv = document.createElement('div'); "
            , "div.appendChild(vdiv); "
            , "vegaEmbed(vdiv, tgt.response.spec); "
            , "} else { "
            , "addText(div, 'Unable to load specification'); "
            , "} "
            , "div.style.display = 'block';"
            , "} "
            ] ++ addTextJS ++ addTitleJS ++ addDescriptionJS

  in (H.script ! A.type_ "text/javascript") (mconcat cts)


embedCSS :: H.Html
embedCSS =
  let cts = [ "#vizview { "
            , "background: white; "
            , "border: 2px solid rgba(0, 0, 0, 0.6); "
            , "display: none; "
            , "left: 2em; "
            , "overflow: hidden; "
            , "padding: 1em; "
            , "position: fixed; "
            , "top: 2em; "
            , "} "
            ] ++ closeCSS ++ descriptionCSS ++ locationCSS ++ pageSetupCSS

  in toCSS cts


showDir ::
  FilePath
  -> ([FilePath], [(FilePath, H.Html)])
  -> ActionM ()
showDir indir (subdirs, files) =
  let atTop = indir == "."
  
      page = (H.docTypeHtml ! A.lang "en-US") $ do
        H.head $ do
          H.title (H.toHtml ("Files to view: " ++ indir))
          vegaEmbed
          embedJS
          embedCSS

        H.body $ do
          (H.div ! A.id "infobar") $ do
            pageTitle
            homeLink

          (H.div ! A.id "mainbar") $ do
            unless atTop (H.p (H.toHtml ("Directory: " ++ indir)))

            unless (null subdirs) $ do
              H.h2 "Sub-directories"
              H.ul $ do
                unless atTop (makeParentLi indir)
                forM_ subdirs (makeLi indir)

            -- let's see how this basic setup works
            --
            -- TODO: might be nice to let users easily skip to next or
            --       previous visualization when viewing one.
            --
            unless (null files) $ do
              (H.div ! A.id "vizview") ""
              (H.div ! A.id "vizlist") $ do
                H.h2 "Visualizations"
                H.table $ do
                  H.thead $
                    H.tr $ do
                      H.th "View page"
                      H.th "View inline"
                  H.tbody $
                    forM_ files $ \(f, _) ->
                      H.tr $ do
                        H.td (pageLink indir f)
                        H.td (embedLink indir f)

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
              dragCSS  -- don't need closeCSS, but near enough

            H.body $ do
              (H.div ! A.id "infobar") $ do
                pageTitle
                homeLink

              (H.div ! A.id "mainbar") $ do
                H.p $ H.toHtml (mconcat ["Go to ", parentLink])
                contents

          dirName = H.toValue ("/display" </> takeDirectory infile)
          parentLink = (H.a ! A.href dirName) "parent directory"
          
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
    Right (Spec o _) -> json (object [ "spec" .= Object o
                                     , "infile" .= infile
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
main = scotty 8082 webapp
