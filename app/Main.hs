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
import Data.Aeson (Value(String, Object), Object, eitherDecode', encode)
import Data.List (sort)
import Data.Version (showVersion)
import Network.HTTP.Types (status404)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeDirectory)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty (ScottyM, ActionM
                  , get, html, notFound, param
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
      
  
dirPage :: FilePath -> ActionM ()
dirPage indir = do
  infiles <- liftIO (listDirectory indir)

  let atTop = indir == "."
  
      page = (H.docTypeHtml ! A.lang "en-US") $ do
        H.head (H.title (H.toHtml ("Files to view: " ++ indir)))
        H.body $ do
          H.h1 "Vega and Vega-Lite viewer"
          unless atTop (H.p (H.toHtml ("Directory: " ++ indir)))
          H.ul $ do
            unless atTop (makeLi "..")
            forM_ (sort infiles) makeLi

      toHref infile = H.toValue ("/display" </> indir </> infile)
      
      makeLi infile = H.li $ (H.a ! A.href (toHref infile)) (H.toHtml infile)

  html (renderHtml page)


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
      status status404

    Right spec ->
      let contents = createView spec "vega-vis"
          page = (H.docTypeHtml ! A.lang "en-US") $ do
            H.head $ do
              H.title "View a spec"
              load "vega@5" ""
              load "vega-lite@3" ""
              load "vega-embed@4" ""
            H.body $ do
              H.h1 "View Vega or Vega-Lite with Vega Embed"
              H.p $ H.toHtml (mconcat ["Go to ", parentLink])
              contents

          load n = H.script ! A.src (mconcat [ "https://cdn.jsdelivr.net/npm/"
                                             , n])

          dirName = H.toValue ("/display" </> takeDirectory infile)
          parentLink = (H.a ! A.href dirName) "parent directory"
          
      in html (renderHtml page)
    

displayPage :: FilePath -> ActionM ()
displayPage infile = do
  isDir <- liftIO (doesDirectoryExist infile)
  if isDir
    then dirPage infile
    else showPage infile
    

webapp :: ScottyM ()
webapp = do

  get "/" (redirect "/index.html")
  get "/index.html" (html (renderHtml indexPage))

  -- TODO: catch errors
  get "/display/" (dirPage ".")

  get (regex "^/display/(.+)$") $ do
    infile <- param "1"
    displayPage infile

  notFound $ do
    status status404
    pure ()


-- for now assume current directory  
main :: IO ()
main = scotty 8082 webapp
