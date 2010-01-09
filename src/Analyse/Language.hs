module Analyse.Language (german, english) where

import Analyse.Types
import qualified Data.ByteString.Lazy.Char8 as C

german :: (Token a) => Language a
german  = Language { isArticle     = \t -> (getTag t) `elem` germanArticleTags
                   , isPreposition = \t -> (getTag t) `elem` germanPrepositionTags
                   , isDefiniteArticle   = undefined
                   , isIndefiniteArticle = undefined
                   , name = "German" }

germanArticleTags     = bs ["ART"]
germanPrepositionTags = bs ["APPR", "APPO", "APZR"] 


english :: (Token a) => Language a
english = Language { isArticle     = \t -> (getTag t) `elem` englishArticleTags
                   , isPreposition = \t -> (getTag t) `elem` englishPrepositionTags
                   , isDefiniteArticle   = undefined
                   , isIndefiniteArticle = undefined
                   , name = "English" }

englishArticleTags     = bs ["AT","AT1"]
englishPrepositionTags = bs ["IF","II","IO","IW"] 

bs :: [String] -> [Tag]
bs = map (Tag . C.pack)
