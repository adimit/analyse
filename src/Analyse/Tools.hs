module Analyse.Tools (filterForTags,freqMap,top,makeCorpus,makeSimpleToken,makeMorphToken) where

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
freqMap :: (Token a, Ord a) => Corpus a -> FreqMap a
freqMap (Corpus c) = foldl' increment M.empty c
    where increment m w = M.insertWith' (+) w 1 m

top :: (Token a) => [Tag] -> [FrequencyItem a] -> [FrequencyItem a]
top tags = sortBy frequencyComparision . filter (tagMatch tags . fst)

tagMatch :: (Token a) => [Tag] -> a -> Bool
tagMatch tags t = getTag t `elem` tags

frequencyComparision :: FrequencyItem a -> FrequencyItem a -> Ordering
frequencyComparision (_,a) (_,b) = compare b a

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

calculateMajorityBaseline :: (Token a) => (a -> a -> Bool) -> [FrequencyItem a] -> a -> Double
calculateMajorityBaseline f m r = fromIntegral (sumup matches) / fromIntegral (sumup mismatches)
    where sumup :: ([FrequencyItem a]) -> Int64
          sumup  = foldl' (flip $ (+) . snd) 0
          (matches,mismatches) = partition (f r . fst) m

calculateTotalMajorityBaseline :: (Token a) => (a -> a -> Bool) -> [FrequencyItem a] -> Double
calculateTotalMajorityBaseline f m = calculateMajorityBaseline f m (fst $ maximumBy frequencyComparision m)
