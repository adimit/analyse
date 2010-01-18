module Analyse.Types 
    (SimpleToken(..)
    , MorphToken(..)
    , TokenData(..)
    , Morphology(..)
    , Token(..)
    , Corpus(..)
    , Tag(..)
    , FreqMap
    , FrequencyItem
    , Language(..)
    , Analysis(..)
    , lowercase) where

import Data.Int (Int64)
import Data.Char (toLower)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as C

class Token t where
    getToken :: t -> TokenData
    getTag :: t -> Tag

data (Token a) => Analysis a = Analysis { resultArticleTotalBaseline        :: Double
                                        , resultPrepositionTotalBaseline    :: Double
                                        , resultTopPrepositions             :: [FrequencyItem a]
                                        , resultTopArticles                 :: [FrequencyItem a]
                                        , resultCorpusSize                  :: Int
                                        , resultSpecificArticleBaseline     :: Maybe Double
                                        , resultSpecificPrepositionBaseline :: Maybe Double
                                        }

instance (Token a, Show a) => Show (Analysis a) where
    show a  = "Analysis results:"
           ++ "\nCorpus size: " ++ (show $ resultCorpusSize a) 
           ++ "\nTop Articles: " ++ (show $ resultTopArticles a)
           ++ "\nTop Prepositions: " ++ (show $ resultTopPrepositions a)
           ++ "\nTotal majority baseline for articles: " ++ (show $ resultArticleTotalBaseline a)
           ++ "\nTotal majority baseline for prepositions: " ++ (show $ resultPrepositionTotalBaseline a)

data SimpleToken = SimpleToken { simpleToken :: TokenData
                               , simpleTag   :: Tag } deriving (Eq,Ord)

data MorphToken = MorphToken { morphToken :: TokenData
                             , morphTag   :: Tag
                             , morphology :: Morphology } deriving (Eq,Ord)

type FrequencyItem a =  (a,Int64)

instance Token SimpleToken where
    getToken = simpleToken
    getTag   = simpleTag

instance Token MorphToken where
    getToken = morphToken
    getTag   = morphTag

instance Show SimpleToken where
    show (SimpleToken td t) = unwords [show td,show t]

instance Show MorphToken where
    show (MorphToken td t m) = unwords [show td,show t,show m]

data Corpus a = Corpus { content :: [a] }

-- | PoS tags are currently implemented as ByteStrings. Bad, I know.
newtype Tag = Tag C.ByteString deriving (Eq,Ord)
instance Show Tag where
    show (Tag t) = wrap (C.unpack t) "(" ")"

-- | Tokens are currently implemented as ByteStrings. Only slightly better, I know.
newtype TokenData = TokenData [C.ByteString] deriving (Eq,Ord)
instance Show TokenData where
    show (TokenData t) = wrap (unwords $ map C.unpack t) "\"" "\""

-- | Very inefficient lowercase method for tokendata.
lowercase :: TokenData -> TokenData
lowercase (TokenData ts) = TokenData $ map (C.pack . map toLower . C.unpack) ts

-- | Morphology data is currently implemented as ByteStrings. Even worse, I know.
newtype Morphology = Morphology C.ByteString deriving (Eq,Ord)
instance Show Morphology where
    show (Morphology t) = wrap (C.unpack t) "(" ")"

-- Shorthands
type FreqMap a = M.Map a Int64

data (Token a) => Language a =
   Language { isArticle  :: a -> Bool -- ^ Is the token an article in the language.
   , isPreposition       :: a -> Bool -- ^ Is the token a preposition  in the language.
   , isDefiniteArticle   :: a -> Bool -- ^ Is the token a definite article in the language.
   , isIndefiniteArticle :: a -> Bool -- ^ Is the token an indefinite article in the language.
   , name :: String -- ^ The language's name in plain text.
   }

wrap :: [a] -> [a] -> [a] -> [a]
wrap s a b = a ++ s ++ b
