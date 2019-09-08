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
import qualified Data.Text.Lazy as LT
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

-- Represent a Vega or Vega-Lite sepcification, which has
-- to be a Javascript object. Other than checking that we
-- have an object, there is no other validation of the
-- JSON.
--
data Spec = Spec {
  specVis :: Object
  , specPath :: FilePath
  }


-- Create HTML for the given specification
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

  in (H.div ! A.class_ "spec") $ do
    (H.p ! A.class_ "location") (H.toHtml (specPath spec))
      
    case mDesc of
      Just desc -> (H.p ! A.class_ "description") (H.toHtml desc)
      Nothing -> pure ()

    (H.div ! A.class_ "embed" ! A.id (H.toValue specId)) ""
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

  
indexPage :: H.Html
indexPage =
  H.docTypeHtml ! A.lang "en-US" $ do
    H.head (H.title "View a Vega or Vega-Lite specification")

    H.body $ do
      H.h1 "View a Vega or Vega-Lite specification."

      H.p (mconcat [ "This is version "
                   , H.toHtml (showVersion version)
                   , " of vega-view. Go to "
                   , (H.a ! A.href "/display/") "/display/"
                   , " to see the available visualizations."
                   ])
      

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

embedLink :: FilePath -> FilePath -> H.Html
embedLink indir infile =
  let toHref = H.toValue ("/embed" </> indir </> infile)
      hdlr = "embed('" <> toHref <> "');"

  in (H.a ! A.href "#" ! A.onclick hdlr) (H.toHtml infile)

-- Nothing to see here; slightly different if base directory or not
emptyDir :: FilePath -> ActionM ()
emptyDir indir =
  let page = (H.docTypeHtml ! A.lang "en-US") $ do
        H.head (H.title (H.toHtml ("Files to view: " ++ indir)))
        H.body $ do
          H.h1 "Vega and Vega-Lite viewer"

          if indir == "."
            then H.p "There is nothing to see in the base directory!"
            else do
              H.p (H.toHtml ("Directory: " ++ indir))
              H.p "There is nothing to see here!"
              H.ul (makeLi indir "..")

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
inlineJS :: H.Html
inlineJS =
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
            , "function addText(parent, text) { "
            , "parent.appendChild(document.createTextNode(text)); "
            , "} "
            , "function addTitle(div, infile) { "
            , "const el = document.createElement('p'); "
            , "el.setAttribute('class', 'location'); "
            , "addText(el, 'File: ' + infile); "
            , "div.appendChild(el); "
            , "const close = document.createElement('span'); "
            , "close.setAttribute('class', 'close'); "
            , "addText(close, '[X]'); "
            , "el.appendChild(close); "
            , "close.addEventListener('click', (ev) => { "
            , "div.style.display = 'none'; "
            , "while (div.firstChild) { "
            , "div.removeChild(div.firstChild);"
            , "} "
            , "});"
            , "}"
            , "function addDescription(div, spec) { "
            , "if (!spec.description || spec.description === '') { return; } "
            , "const el = document.createElement('p'); "
            , "el.setAttribute('class', 'description'); "
            , "addText(el, spec.description); "
            , "div.appendChild(el); "
            , "}"
            ]
  in (H.script ! A.type_ "text/javascript") (mconcat cts)


inlineCSS :: H.Html
inlineCSS =
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
            , "p.location { "
            , "background: rgba(0, 0, 0, 0.2);"
            , "font-weight: bold; "
            , "margin: -1em; "
            , "margin-bottom: 1em; "
            , "padding: 0.5em; "
            , "} "
            , ".close { "
            , "float: right; "
            , "}"
            ]
  in (H.style ! A.type_ "text/css") (mconcat cts)


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
          inlineJS
          inlineCSS

        H.body $ do
          H.h1 "Vega and Vega-Lite viewer"
          unless atTop (H.p (H.toHtml ("Directory: " ++ indir)))

          unless (null subdirs) $ do
            H.h2 "Sub-directories"
            H.ul $ do
              unless atTop (makeLi indir "..")
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
            H.body $ do
              H.h1 "View Vega or Vega-Lite with Vega Embed"
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
