module Options where

import Data.Monoid
import System.Console.GetOpt
import Snap.Core
import Snap.Http.Server

data Options = Options { optShowVersion :: Bool
                       , optDataDir     :: FilePath
                       } deriving (Show, Eq)

instance Monoid Options where
    mempty = Options False "./"
    mappend a b = a { optShowVersion = optShowVersion b
                    , optDataDir     = optDataDir b
                    }

type SnapConfig = Config Snap Options

getConfig :: IO (SnapConfig, Options)
getConfig = do
    scfg <- snapConfig emptyConfig
    let mcfg = getOther scfg
    return $ maybe (scfg,mempty) ((,) scfg) mcfg

snapConfig :: SnapConfig -> IO SnapConfig
snapConfig defaults = extendedCommandLineConfig (userOptions ++ optDescrs defaults) mappend defaults


userOptions :: [OptDescr (Maybe SnapConfig)]
userOptions = map (fmapOpt $ fmap (`setOther` mempty))
    [ Option "v" ["version"]
        (NoArg (Just $ mempty { optShowVersion = True }))
        "show version info"
    , Option "d" ["directory"]
        (ReqArg (\d -> Just $ mempty { optDataDir = d }) "DIRECTORY")
        "directory to read routes from (default is ./)"
    ]

