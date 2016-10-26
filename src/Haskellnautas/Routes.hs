module Haskellnautas.Routes
    ( stripRoute
    )
    where

import           Hakyll


stripRoute :: String -> Routes
stripRoute = (`gsubRoute` const "")
