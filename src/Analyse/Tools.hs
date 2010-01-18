module Analyse.Tools
    ( tokenDataEquality
    , calculateTotalMajorityBaseline
    , calculateMajorityBaseline
    , filterForTags
    , freqMap
    , top
    , makeCorpus
    , makeSimpleToken
    , makeMorphToken ) where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as M
import Data.Foldable (foldl')
import Data.Int (Int64)
import Data.List (sortBy,maximumBy,partition)
import Data.Char (toLower)
import Analyse.Types

-- | Given a list of tags, filter a corpus for all words containing these tags.
filterForTags :: (Token a ) => [Tag] -> Corpus a -> Corpus a
filterForTags i (Corpus c) = Corpus $ filter f c
    where f t = getTag t `elem` i

-- | Calculates the frequency of tokens (exact matches over the Corpus' Token!)
freqMap :: (Token a, Ord a) => [a] -> FreqMap a
freqMap c = foldl' increment M.empty c
    where increment m w = M.insertWith' (+) w 1 m

top :: (Token a) => FreqMap a -> [FrequencyItem a]
top = sortBy frequencyComparision . M.toList

frequencyComparision :: FrequencyItem a -> FrequencyItem a -> Ordering
frequencyComparision (_,a) (_,b) = compare b a

frequencyComparision' (_,a) (_,b) = compare a b

-- | Given a function to handle each line, make a corpus out of the given ByteString.
makeCorpus :: (Token a) => ([C.ByteString] -> a) -> C.ByteString -> Corpus a
makeCorpus f = Corpus . map (f . reverse . C.words) . C.lines

-- | Make a normalised simple token from a line (list of ByteStrings).
makeSimpleToken :: [C.ByteString] -> SimpleToken
makeSimpleToken [] = SimpleToken (TokenData []) (Tag C.empty)
makeSimpleToken s = SimpleToken (TokenData $  map (C.pack . map toLower . C.unpack) (tail s)) (Tag $ head s)

-- | Make a normalised token with morphology.
makeMorphToken :: [C.ByteString] -> MorphToken
makeMorphToken [] = MorphToken (TokenData []) (Tag C.empty) (Morphology C.empty)
makeMorphToken (m:s) = MorphToken (TokenData $ map (C.pack . map toLower . C.unpack) (tail s)) (Tag $ head s) (Morphology m)

calculateTotalMajorityBaseline :: (Token a) => (a -> a -> Bool) -> [FrequencyItem a] -> Double
calculateTotalMajorityBaseline f m = calculateMajorityBaseline (f . fst . head $ m) m

calculateMajorityBaseline :: (Token a) => (a -> Bool) -> [FrequencyItem a] -> Double
calculateMajorityBaseline f m = fromIntegral (sumup $ filter (f . fst) m) / fromIntegral (sumup m)
    where sumup = foldl' (flip $ (+) . snd) 0

tokenDataEquality :: (Token a) => a -> a -> Bool
tokenDataEquality a b = getToken a == getToken b
