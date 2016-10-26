module Config (config) where

import Hakyll

config :: Configuration
config = defaultConfiguration { providerDirectory = "static/" }
