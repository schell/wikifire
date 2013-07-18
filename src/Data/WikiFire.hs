{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies,
  OverloadedStrings, DeriveDataTypeable, TypeSynonymInstances,
  TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.WikiFire where

import Types
import Data.Parser

import Data.Acid
import Data.Aeson
import System.Directory
import Control.Monad
import Data.Text.Encoding
import Data.Vector            ( fromList )
import Control.Monad.State    ( get, put )
import Control.Monad.Reader   ( ask )
import Control.Applicative    ( (<$>) )
import System.FilePath        ( (</>) )

import qualified Data.Map                   as M
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString            as B
import qualified Data.Text                  as T


initialTemplateSourceMap :: FilePath -> IO WFTemplateSourceMap
initialTemplateSourceMap datadir = do
    let configFile = datadir </> "routes.json"
    routesExist <- doesFileExist configFile
    unless routesExist $ putStrLn $ "Could not find routes.json at " ++ show configFile
    routeMap    <- if not routesExist
                   then return []
                   else do config <- readFile $ datadir </> "routes.json"
                           case decode (L.pack config) :: Maybe [RouteCfg] of
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
        t        = maybe WFTTextPlain readWFTemplateType mT
    src <- B.readFile filePath
    return $ makeWFTemplate t src

makeWFTemplate :: WFTemplateType -> B.ByteString -> WFTemplate
makeWFTemplate wft b = WFTemplate wft payload
    where payload = if wfTypeIsBinary wft
                      then WFTSrcBin b
                      else WFTSrcText $ decodeUtf8 b

postTemplate :: String -> WFTemplate -> Update WFTemplateSourceMap B.ByteString
postTemplate name t@(WFTemplate _ (WFTSrcText s)) =
    -- Parse the new template first.
    case parseWFTemplate t of
        Left err       -> return $ replyJsonMsg False $ object [ T.pack "name" .= name
                                                               , T.pack "error".= err
                                                               ]
        Right template -> do
            sourceMap      <- get
            put $ M.insert name t sourceMap
            return $ replyJsonMsg True $ object [ T.pack "name"  .= name
                                                , T.pack "bytes" .= T.length s
                                                , T.pack "parse" .= show template
                                                ]

postTemplate name t@(WFTemplate _ (WFTSrcBin b)) = do
    sourceMap <- get
    put $ M.insert name t sourceMap
    return $ replyJsonMsg True $ object [ T.pack "name"  .= name
                                        , T.pack "bytes" .= B.length b
                                        ]

getTemplate :: String -> Query WFTemplateSourceMap (Maybe WFTemplate)
getTemplate name = M.lookup name <$> ask

allTemplateNames :: Query WFTemplateSourceMap [String]
allTemplateNames = M.keys <$> ask

getTemplateNames :: Query WFTemplateSourceMap B.ByteString
getTemplateNames = do
    keys <- M.keys <$> ask
    return $ L.toStrict $ encode $ object [ T.pack "ok"   .= True
                                          , T.pack "data" .= fromList (map (String . T.pack) keys) ]

replyJsonMsg :: Bool -> Value -> B.ByteString
replyJsonMsg ok reply = L.toStrict $ encode $ object [ T.pack "ok"   .= ok
                                                     , T.pack "data" .= reply ]

$(makeAcidic ''WFTemplateSourceMap [ 'postTemplate
                                   , 'getTemplate
                                   , 'getTemplateNames
                                   , 'allTemplateNames
                                   ])

