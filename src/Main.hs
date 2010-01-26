module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as C
import Analyse.Language (german,english)
import Analyse.Types
import Analyse.Tools
import System.Console.GetOpt

main :: IO ()
main = do
    (flags,_) <- analyserOptions =<< getArgs
    f <- C.readFile (optCorpus flags)
    if optMorphology flags
                    then putStrLn . show $ analyse (optLanguage flags) (makeCorpus makeMorphToken  f)
                    else putStrLn . show $ analyse (optLanguage flags) (makeCorpus makeSimpleToken f)

analyse :: (Token a,Ord a) => Language a -> Corpus a -> Analysis a
analyse l (Corpus ts) = Analysis
   { resultArticleTotalBaseline        = calculateTotalMajorityBaseline articles
   , resultPrepositionTotalBaseline    = calculateTotalMajorityBaseline prepositions
   , resultTopPrepositions             = take 10 prepositions
   , resultTopArticles                 = take 10 articles
   , resultCorpusSize                  = length ts
   , resultSpecificArticleBaseline     = Nothing
   , resultSpecificPrepositionBaseline = Nothing
   } where prepositions = top $ freqMap $ filter (isPreposition l) ts
           articles     = top $ freqMap $ filter (isArticle     l) ts

analyserOptions :: [String] -> IO (Options, [String])
analyserOptions argv = case getOpt Permute options argv of
                            (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
                            (_,_,es) -> ioError $ userError (concat es)

data Options = Options { optVerbose    :: Bool
                       , optCorpus     :: FilePath
                       , optLanguage   :: (Token a) => Language a
                       , optMorphology :: Bool
                       , optWordList   :: [String]
                       , optReplacer   :: String }

defaultOptions :: Options
defaultOptions = Options { optVerbose    = False
                         , optCorpus     = error "No corpus file specified."
                         , optLanguage   = english
                         , optMorphology = False
                         , optWordList   = []
                         , optReplacer   = [] }

options :: [OptDescr (Options -> Options)]
options = [ Option "v" ["verbose"]     (NoArg  (\o -> o {optVerbose = True}))
                "Print detailed info."
          , Option "m" ["use-morphology"] (NoArg (\o -> o {optMorphology = True}))
                "Assume the corpus contains morphology information"
          , Option "c" ["corpus-file"] (ReqArg (\d o -> o {optCorpus = d}) "CORPUS")
                "Corpus input file."
          , Option "l" ["language"]    (ReqArg (\d o -> o {optLanguage = language d}) "LANGUAGE")
                "Corpus language to use."
          , Option "w" ["use-words"] (ReqArg (\d o -> o { optReplacer = head (words d), optWordList = tail (words d) }) "WORDS")
                "Space-seperated list of words to build a specific majority baseline by. The head of the list is the master token, the tail is going to be tested against."
          ]

language :: (Token a) => String -> Language a
language s = case s of "german"  -> german
                       "english" -> english
                       _         -> error $ "Unknown language " ++ s
