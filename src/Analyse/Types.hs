module Analyse.Types (SimpleToken(..), MorphToken(..), Token(..), Corpus(..), Tag(..), FreqMap, Language(..)) where

import Data.Int (Int64)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as C

class Token t where
    getToken :: t -> TokenData
    getTag :: t -> Tag

data SimpleToken = SimpleToken { simpleToken :: TokenData
                               , simpleTag   :: Tag }
data MorphToken = MorphToken { morphToken :: TokenData
                             , morphTag   :: Tag
                             , morphology :: Morphology } deriving (Eq,Ord)

instance Token SimpleToken where
    getToken = simpleToken
    getTag   = simpleTag

instance Token MorphToken where
    getToken = morphToken
    getTag   = morphTag

instance Show SimpleToken where
    show (SimpleToken td t) = unwords $ [show td,show t]

instance Show MorphToken where
    show (MorphToken td t m) = unwords $ [show td,show t,show m]

data Corpus a = Corpus { content :: [a] }

-- | PoS tags are currently implemented as ByteStrings. Bad, I know.
newtype Tag = Tag C.ByteString deriving (Eq,Ord)
instance Show Tag where
    show (Tag t) = wrap (C.unpack t) "(" ")"

-- | Tokens are currently implemented as ByteStrings. Only slightly better, I know.
newtype TokenData = TokenData [C.ByteString] deriving (Eq,Ord)
instance Show TokenData where
    show (TokenData t) = wrap (unwords $ map C.unpack t) "\"" "\""

-- | Morphology data is currently implemented as ByteStrings. Even worse, I know.
newtype Morphology = Morphology C.ByteString deriving (Eq,Ord)
instance Show Morphology where
    show (Morphology t) = wrap (C.unpack t) "(" ")"

-- Shorthands
type FreqMap a = M.Map a Int64

data Language = Language { articleTags     :: [Tag]
                         , prepositionTags :: [Tag] }

wrap :: [a] -> [a] -> [a] -> [a]
wrap s a b = a ++ s ++ b
