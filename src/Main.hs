module Main where

import System.Environment (getArgs)
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as M
import Analyse.Language (german,english)
import Analyse.Types
import Analyse.Tools
import System.Console.GetOpt

main :: IO ()
main = do
    (flags,s) <- analyserOptions =<< getArgs
    corpus <- liftM makeCorpus $ C.readFile (optCorpus flags)
    let lang  = optLanguage flags -- for convenience and shorter lines :-)
        freqs = (freqMap $ filterForTags ((articleTags lang) ++ (prepositionTags lang)) corpus)
        articles     = top (articleTags lang) freqs
        prepositions = top (prepositionTags lang) freqs
    if (optVerbose flags)
        then putStrLn $ show $ M.toList freqs 
        else return ()
    putStrLn $ "Corpus size: " ++ (show $ length (content corpus)) ++ " tokens"
    putStrLn $ "\nTop articles: "     ++ (show $ take 10 $ articles)
    putStrLn $ "\nTop prepositions: " ++ (show $ take 10 $ prepositions)
    putStrLn $ "\nTotal majority basline for articles: "     ++ (show $ totalBaseline articles)
    putStrLn $ "\nTotal majority basline for prepositions: " ++ (show $ totalBaseline prepositions)

analyserOptions :: [String] -> IO (Options, [String])
analyserOptions argv = case getOpt Permute options argv of
                            (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
                            (_,_,es) -> ioError $ userError (concat es)

data Options = Options { optVerbose  :: Bool
                       , optCorpus   :: FilePath
                       , optLanguage :: Language }

defaultOptions :: Options
defaultOptions = Options { optVerbose  = False
                         , optCorpus   = error "No corpus file specified."
                         , optLanguage = english }

options :: [OptDescr (Options -> Options)]
options = [ Option ['v'] ["verbose"]     (NoArg  (\o -> o {optVerbose = True}))
                "Print detailed info."
          , Option ['c'] ["corpus-file"] (ReqArg (\d o -> o {optCorpus = d}) "CORPUS")
                "Corpus input file."
          , Option ['l'] ["language"]    (ReqArg (\d o -> o {optLanguage = language d}) "LANGUAGE")
                "Corpus language to use." ]

language :: String -> Language
language s = case s of "german"  -> german
                       "english" -> english
                       _         -> error $ "unknown language " ++ s
