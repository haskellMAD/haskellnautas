{-# LANGUAGE OverloadedStrings #-}
import Clay

styles :: Css
styles =
    body ? do
      return ()

main :: IO ()
main = putCss styles
