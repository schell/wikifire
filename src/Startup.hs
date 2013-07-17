{-#LANGUAGE RecordWildCards #-}
module Startup where

import System.Environment
import System.Console.GetOpt
import System.FilePath
import Data.List
import Control.Monad
import Prelude                   hiding ( FilePath )

start :: (Options -> IO ()) -> IO ()
start f = do
    args <- getArgs
    case getOpt Permute options args of
        ( opts,    _,   []) -> runWithOpts f $ foldl (flip id) defaultOptions opts
        (    _,    _, msgs) -> error $ concat msgs ++ usageInfo header options

data Options = Options { optShowVersion :: Bool
                       , optDataDir     :: FilePath
                       } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optDataDir = "."
                         }

options :: [OptDescr (Options -> Options)]
options = [ Option "v" ["version"]
              (NoArg (\opts -> opts { optShowVersion = True }))
              "show version info"
          , Option "d" ["directory"]
              (ReqArg (\d opts -> opts { optDataDir = d }) "DIRECTORY")
              "directory to read routes from (default is ./)"
          ]

header :: String
header = "Usage: wikifire [v] -d datadir"

version :: String
version = intercalate "\n" [ "\nWikifire 0.0.0.1"
                           , "    by Schell Scivally"
                           , ""
                           ]

runWithOpts :: (Options -> IO ()) -> Options -> IO ()
runWithOpts f opts = do
    let Options{..} = opts
    when optShowVersion $ putStrLn version
    f opts

