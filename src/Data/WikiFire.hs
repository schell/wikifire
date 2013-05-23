{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TypeFamilies, OverloadedStrings,
    DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances #-}
module Data.WikiFire where

import Data.Acid
import Data.Aeson
import Paths_wikifire
import System.Directory
import Data.Vector            ( fromList )
import Control.Monad          ( foldM, mzero )
import Control.Monad.State    ( get, put )
import Control.Monad.Reader   ( ask )
import Control.Applicative    ( (<$>), (<*>) )
import System.FilePath        ( (</>) )

import qualified Data.Map                   as M
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T

-- | The main acid store.
type TemplateSourceMap = M.Map String String

-- | Represents a config file.
data Config = Config { configRoutes :: [Route] }

instance FromJSON Config where
   parseJSON (Object v) = Config <$>
                          v .: "configRoutes"
   parseJSON _          = mzero

-- | Each route is a route name that points to a file from
-- which to read the template for that route. The `routeFilePath` is an
-- array of nodes that will be concatenated with the OS's directory
-- separator.
data Route  = Route { routeName      :: String
                    , routeFilePath  :: [String] } deriving (Show, Eq)

instance FromJSON Route where
    parseJSON (Object v) = Route            <$>
                           v .: "routeName" <*>
                           v .: "routeFilePath"
    parseJSON _          = mzero

-- | A type to reply with JSON data.
data JSONReply = JSONReply { jsonReplyOk   :: Bool
                           , jsonReplyData :: Value }

initialTemplateSourceMap :: IO TemplateSourceMap
initialTemplateSourceMap = do
    datadir <- getDataDir
    let configFile = datadir </> "routes.json"
    routesExist <- doesFileExist configFile
    routeMap    <- if not routesExist
                   then return []
                   else do config <- readFile $ datadir </> "routes.json"
                           case decode (B.pack config) :: Maybe [Route] of
                               Just routes -> return routes
                               Nothing     -> do
                                   putStrLn $ "Could not decode config file " ++ configFile
                                   return []
    let routesInDataDir = map (\r -> r { routeFilePath = datadir:routeFilePath r }) routeMap
    addRoutes M.empty routesInDataDir

addRoutes :: TemplateSourceMap -> [Route] -> IO TemplateSourceMap
addRoutes = foldM (\m r -> do
    let filePath = path r
        name     = routeName r
        path     = foldl (</>) "" . routeFilePath
    file <- readFile filePath
    putStrLn $ "  Routing   " ++ name ++ "  ->  " ++ filePath
    return $ M.insert name file m)

postTemplate :: String -> String -> Update TemplateSourceMap B.ByteString
postTemplate name src = do
    sourceMap      <- get
    put $ M.insert name src sourceMap
    return $ replyJsonMsg True $ object [ T.pack "name"   .= name
                                        , T.pack "source" .= src
                                        , T.pack "bytes"  .= length src ]

getTemplate :: String -> Query TemplateSourceMap (Maybe String)
getTemplate name = M.lookup name <$> ask

getTemplateNames :: Query TemplateSourceMap B.ByteString
getTemplateNames = do
    keys <- M.keys <$> ask
    return $ encode $ object [ T.pack "ok"   .= True
                             , T.pack "data" .= fromList (map (String . T.pack) keys) ]

replyJsonMsg :: Bool -> Value -> B.ByteString
replyJsonMsg ok reply = encode $ object [ T.pack "ok"   .= ok
                                        , T.pack "data" .= reply ]

$(makeAcidic ''TemplateSourceMap [ 'postTemplate
                                 , 'getTemplate
                                 , 'getTemplateNames ])

