module Analyse.Language (german, english) where

import Analyse.Types
import qualified Data.ByteString.Lazy.Char8 as C

german :: Language
german  = Language { articleTags     = bs ["ART"]
                   , prepositionTags = bs ["APPR", "APPO", "APZR"] 
                   , name = "German" }

english :: Language
english = Language { articleTags     = bs ["AT","AT1"]
                   , prepositionTags = bs ["IF","II","IO","IW"] 
                   , name = "English" }

bs :: [String] -> [Tag]
bs = map (Tag . C.pack)
