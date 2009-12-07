module Analyse.Language (german, english) where

import Analyse.Types
import qualified Data.ByteString.Lazy.Char8 as C

german :: Language
german  = Language { articleTags     = bs ["ART"]
                   , prepositionTags = bs ["APPR", "APPO", "APZR"] }

english :: Language
english = Language { articleTags     = bs ["AT","AT1"]
                   , prepositionTags = bs ["IF","II","IO","IW"] }

bs :: [String] -> [Tag]
bs = map (Tag . C.pack)
