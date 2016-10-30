module Haskellnautas.Html
  ( useCleanUrls
  ) where

import           Hakyll
import           System.FilePath

useCleanUrls :: Item String -> Compiler (Item String)
useCleanUrls = return . fmap (withUrls cleanUrl)


cleanUrl :: String -> String
cleanUrl url =
  let (path, ext) = splitExtensions url
  in
    if ext == ".html"
    then
      if takeBaseName path == "index"
      then take (length url - 10) url
      else take (length url - 5) url </> ""
    else url
