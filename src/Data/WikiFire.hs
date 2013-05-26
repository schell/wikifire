{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TypeFamilies, OverloadedStrings,
    DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances #-}
module Data.WikiFire where

import Data.Acid
import Data.Aeson
import Paths_wikifire
import System.Directory
import Data.Typeable          ( Typeable )
import Data.Vector            ( fromList )
import Data.Maybe             ( fromMaybe )
import Control.Monad          ( foldM, mzero )
import Control.Monad.State    ( get, put )
import Control.Monad.Reader   ( ask )
import Control.Applicative    ( (<$>), (<*>) )
import System.FilePath        ( (</>) )
import Data.SafeCopy          ( base, deriveSafeCopy )

import qualified Data.Map                   as M
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T

-- | The main acid store.
type WFTemplateSourceMap = M.Map String WFTemplate

data WFTemplate = WFTemplate { templateType  :: B.ByteString
                             , templateSource:: B.ByteString
                             } deriving (Eq, Typeable)

instance Show WFTemplate where
    show (WFTemplate ct src) = "WFTemplate { templateType='"++ B.unpack ct ++ "' templateSrc='" ++ suf (trunc src) ++ "'}"
        where suf xs = if length xs >= chars
                       then xs ++ "..."
                       else xs
              trunc  = take chars . B.unpack
              chars  = 10

$(deriveSafeCopy 0 'base ''WFTemplate)

-- | Represents a config file.
data Config = Config { routeCfgs :: [RouteCfg] }

instance FromJSON Config where
   parseJSON (Object v) = Config <$>
                          v .: "configRoutes"
   parseJSON _          = mzero

-- | Each route is a route name that points to a file from
-- which to read the template for that route. The `routeFilePath` is an
-- array of nodes that will be concatenated with the OS's directory
-- separator.
data RouteCfg  = RouteCfg { routeName         :: String       -- ^ Path of the template on the server.
                          , routeContentType  :: Maybe String -- ^ Content type of the route (ie, "text/plain")
                          , routeFilePath     :: [String]     -- ^ Path of the template on disk, each directory being one entry in the array.
                          } deriving (Show, Eq)


instance FromJSON RouteCfg where
    parseJSON (Object v) = RouteCfg                    <$>
                              v .:  "routeName"        <*>
                              v .:? "routeContentType" <*>
                              v .:  "routeFilePath"
    parseJSON _          = mzero

initialTemplateSourceMap :: IO WFTemplateSourceMap
initialTemplateSourceMap = do
    datadir <- getDataDir
    let configFile = datadir </> "routes.json"
    routesExist <- doesFileExist configFile
    routeMap    <- if not routesExist
                   then return []
                   else do config <- readFile $ datadir </> "routes.json"
                           case decode (B.pack config) :: Maybe [RouteCfg] of
                               Just routes -> return routes
                               Nothing     -> do
                                   putStrLn $ "Could not decode config file " ++ configFile
                                   return []
    let routesInDataDir = map (\r -> r { routeFilePath = datadir:routeFilePath r }) routeMap
    addRoutes M.empty routesInDataDir

addRoutes :: WFTemplateSourceMap -> [RouteCfg] -> IO WFTemplateSourceMap
addRoutes = foldM (\m r -> do
    let name = routeName r
    template <- toWFTemplate r
    putStrLn $ "  Routing   " ++ name ++ "  ->  " ++ show template
    return $ M.insert name template m)

toWFTemplate :: RouteCfg -> IO WFTemplate
toWFTemplate (RouteCfg _ mT p) = do
    let filePath = foldl (</>) "" p
        t        = B.pack $ fromMaybe "text/html" mT
    src <- readFile filePath
    return $ WFTemplate t $ B.pack src

postTemplate :: String -> WFTemplate -> Update WFTemplateSourceMap B.ByteString
postTemplate name t = do
    sourceMap      <- get
    put $ M.insert name t sourceMap
    return $ replyJsonMsg True $ object [ T.pack "name"   .= name
                                        , T.pack "bytes"  .= B.length (templateSource t)
                                        ]

getTemplate :: String -> Query WFTemplateSourceMap (Maybe WFTemplate)
getTemplate name = M.lookup name <$> ask

getTemplateNames :: Query WFTemplateSourceMap B.ByteString
getTemplateNames = do
    keys <- M.keys <$> ask
    return $ encode $ object [ T.pack "ok"   .= True
                             , T.pack "data" .= fromList (map (String . T.pack) keys) ]

replyJsonMsg :: Bool -> Value -> B.ByteString
replyJsonMsg ok reply = encode $ object [ T.pack "ok"   .= ok
                                        , T.pack "data" .= reply ]

$(makeAcidic ''WFTemplateSourceMap [ 'postTemplate
                                 , 'getTemplate
                                 , 'getTemplateNames ])

