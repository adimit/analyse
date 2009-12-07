module Analyse.Types (Token(..), Corpus(..), Tag(..), FreqMap, Language(..)) where

import Data.Int (Int64)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as C

data Token = SimpleToken { simpleToken :: TokenData
                         , simpleTag   :: Tag }
           | MorphToken  { morphToken :: TokenData
                         , morphTag   :: Tag
                         , morphology :: Morphology } deriving (Eq,Ord)

token :: Token -> TokenData
token (MorphToken t _ _) = t
token (SimpleToken t _)  = t

tag :: Token -> Tag
tag (MorphToken _ t _) = t
tag (SimpleToken _ t) = t

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

instance  Show Token where
    show (SimpleToken w t)  = unwords [show w, show t]
    show (MorphToken w t m) = unwords [show w, show t, show m]

data Language = Language { articleTags     :: [Tag]
                         , prepositionTags :: [Tag] }

wrap :: [a] -> [a] -> [a] -> [a]
wrap s a b = a ++ s ++ b
