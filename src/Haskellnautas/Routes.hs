module Haskellnautas.Routes
    ( stripRoute
    , cleanRoute
    )
    where

import           Hakyll
import           System.FilePath


stripRoute :: String -> Routes
stripRoute = (`gsubRoute` const "")


cleanRoute :: Routes
cleanRoute = customRoute filenameToIndex

filenameToIndex :: Identifier -> FilePath
filenameToIndex identifier =
  let (path, ext) = splitExtensions $ toFilePath identifier
  in
    path </> "index" <.> ext
