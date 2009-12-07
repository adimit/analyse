module Analyse.Tools (filterForTags,freqMap,top,makeCorpus,totalBaseline) where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as M
import Data.Foldable (foldl')
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Char (toLower)
import Analyse.Types

-- | Given a list of tags, filter a corpus for all words containing these tags.
filterForTags :: [Tag] -> Corpus Token -> Corpus Token
filterForTags i (Corpus c) = Corpus $ filter interesting c
    where t = (getTag t) `elem` i

-- | Calculates the frequency of tokens (exact matches over the Corpus' Token!)
freqMap :: (Token a) => Corpus a -> FreqMap
freqMap (Corpus c) = foldl' increment M.empty c
    where increment m w = M.insertWith' (+) w 1 m

-- | Sorts a frequency map by number of tokens. Also filters the map by the list of
-- tags it is given.
top :: [Tag] -> FreqMap -> [(a,Int64)]
top tags freqs = sortBy sndPair (filter (\((Token _ t),_) -> t `elem` tags) $ M.toList freqs)
    where sndPair (_,a) (_,b) = compare b a

-- | Given a function to handle each line, make a corpus out of the given ByteString.
makeCorpus :: (Token a) => ([C.ByteString] -> a) -> C.ByteString -> Corpus a
makeCorpus f = (Corpus . (map (f . reverse . C.words)) . C.lines)

-- | Make a simple token from a line (list of ByteStrings).
makeSimpleToken :: [C.ByteString] -> StdSimpleToken
makeSimpleToken [] = SimpleToken [] C.empty
makeSimpleToken s = SimpleToken (map (C.pack . (map toLower) . C.unpack) (tail s)) (head s)

-- | Make a token with morphology.
makeMorphToken :: [C.ByteString] -> StdMorphToken
makeMorphToken [] = MorphToken [] C.empty Nothing
makeMorphToken (m:s) = MorphToken (map (C.pack . (map toLower) . C.unpack) (tail s)) (head s) m

totalBaseline :: (forall a) => [(a,Int64)] -> Double
totalBaseline ts = ((fromIntegral . snd . head $ ts)/(fromIntegral (foldl' (\n (_,i) -> n+i) 0 ts)))
