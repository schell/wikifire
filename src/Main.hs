{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Main where

import Data.WikiFire
import Data.Parser
import Types
import Template
import Options

import Snap.Core
import Snap.Http.Server
import Control.Concurrent.MVar
import Control.Monad
import Data.Maybe
import Data.Text.Encoding
import Data.List

import Control.Monad.IO.Class   ( liftIO )
import Control.Exception        ( bracket )
import Data.Acid                ( AcidState, openLocalState )
import Data.Acid.Local          ( createCheckpointAndClose )
import Data.Acid.Advanced       ( query', update' )

import qualified Data.Text                  as T
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as L
import qualified Data.Map                   as M

main :: IO ()
main = do
    (cfg, opts) <- getConfig
    let datadir = optDataDir opts
    putStrLn $ "Reading routes from " ++ show datadir
    sourceMap <- initialTemplateSourceMap datadir
    putStrLn "Creating template web..."
    tMap      <- newEmptyMVar
    putStrLn "Running the wikifire server..."
    bracket (openLocalState sourceMap)
            createCheckpointAndClose
            (httpServe cfg . flip site tMap)

site :: AcidState WFTemplateSourceMap -> TemplateMapState -> Snap ()
site acid tMapVar = do
    liftIO $ loadTemplatesIfNeeded acid tMapVar
    msum [ method POST $ routePOST acid tMapVar
         , method GET $ routeGET  acid tMapVar
         ]

routeGET :: AcidState WFTemplateSourceMap -> TemplateMapState -> Snap ()
routeGET acid tMap =
    route [ ("_templateNames", handleGetTemplateNames acid)
          , ("_", fmap C.unpack (getsRequest rqPathInfo) >>= handleGetTemplate acid)
          , ("", fmap C.unpack (getsRequest rqPathInfo) >>= handleRenderTemplate acid tMap)
          , ("favicon.ico", getResponse >>= finishWith)
          ]

routePOST :: AcidState WFTemplateSourceMap -> TemplateMapState -> Snap ()
routePOST = handlePostTemplate

handleGetTemplate :: AcidState WFTemplateSourceMap -> String -> Snap ()
handleGetTemplate acid name = do
    liftIO $ print name
    mTemplate <- query' acid $ GetTemplate name
    maybe (return ()) okGetTemplate mTemplate

okGetTemplate :: WFTemplate -> Snap ()
okGetTemplate (WFTemplate typ (WFTSrcBin b)) = do
    res <- getResponse
    putResponse $ setContentType (C.pack $ showWFTemplateType typ) res
    writeBS b

okGetTemplate (WFTemplate _ (WFTSrcText t)) = do
    res <- getResponse
    putResponse $ setContentType "text/plain" res
    writeBS $ encodeUtf8 t

handlePostTemplate :: AcidState WFTemplateSourceMap -> TemplateMapState -> Snap ()
handlePostTemplate acid tMapVar = do
    mName <- getsRequest $ rqPostParam "name"
    mSrc  <- getsRequest $ rqPostParam "source"
    mCt   <- getsRequest $ rqPostParam "contentType"
    tMap  <- liftIO $ getTemplateMap tMapVar
    when (all isJust [mName,mSrc]) $ do
        let ct:_   = fromMaybe ["text/plain"] mCt
            ct'    = readWFTemplateType $ C.unpack ct
            name:_ = fromJust mName
            name'  = C.unpack name
            src:_  = fromJust mSrc
            eTmp = parseWFTemplate wft
            wft  = WFTemplate ct' $ if wfTypeIsBinary ct'
                                     then WFTSrcBin src
                                     else WFTSrcText $ decodeUtf8 src
        void $ liftIO $ putStrLn $ "Posting template to " ++ show name
        case eTmp of
            Left err -> writeBS $ encodeUtf8 err
            Right t  -> do
                liftIO $ void $ tryTakeMVar tMapVar
                success <- liftIO $ tryPutMVar tMapVar $ M.insert name' t tMap
                if success
                  then do msg <- update' acid $ PostTemplate name' wft
                          writeBS msg
                  else writeBS "Could not store parsed template."

handleGetTemplateNames :: AcidState WFTemplateSourceMap -> Snap ()
handleGetTemplateNames acid = do
    namesJSON <- query' acid GetTemplateNames
    writeBS namesJSON

handleRenderTemplate :: AcidState WFTemplateSourceMap -> TemplateMapState -> String -> Snap ()
handleRenderTemplate acid tMapVar name = do
    let name' = if "/" `isSuffixOf` name
                  then init name
                  else name
    tMap  <- liftIO $ getTemplateMap tMapVar
    case M.lookup name' tMap of
        Just t  -> renderTemplate t tMap
        Nothing -> do eitherT <- renderTemplateFromAcid acid name tMapVar
                      case eitherT of
                          Left msg -> writeBS $ encodeUtf8 msg
                          Right t  -> renderTemplate t tMap

renderTemplateFromAcid :: AcidState WFTemplateSourceMap -> String -> TemplateMapState -> Snap (Either T.Text Template)
renderTemplateFromAcid acid name tMapVar = do
    liftIO $ putStrLn $ "Rendering template " ++ show name ++ " from acid."
    mwft  <- query' acid $ GetTemplate name
    case mwft of
        Nothing  -> return $ Left $ T.pack $ "Could not find template " ++ show name
        Just wft -> do
            tMap <- liftIO $ getTemplateMap tMapVar
            case parseWFTemplate wft of
                Left err       -> return $ Left err
                Right template -> do
                    let tMap' = M.insert name template tMap
                    liftIO $ putStrLn $ "Storing template " ++ show name ++ " into map " ++ show tMap'
                    success <- liftIO $ tryPutMVar tMapVar tMap'
                    unless success $ liftIO $ putStrLn "Could not store template in map!"
                    return $ Right template

renderTemplate :: Template -> TemplateMap -> Snap ()
renderTemplate t m = do
    res <- getResponse
    putResponse $ setContentType (C.pack $ showWFTemplateType typ) res
    writeBS payload
        where Binary _ b         = t
              typ                = getTemplateType t
              (Template _ frags) = t
              payload = if templateTypeIsBinary t
                          then b
                          else encodeUtf8 $ resolveFragmentsWithMap frags m

convertText :: T.Text -> L.ByteString
convertText = L.fromStrict . encodeUtf8

getTemplateMap :: TemplateMapState -> IO TemplateMap
getTemplateMap = readMVar

loadTemplatesIfNeeded :: AcidState WFTemplateSourceMap -> TemplateMapState -> IO ()
loadTemplatesIfNeeded acid tMapVar = do
    mtMap <- tryTakeMVar tMapVar
    tMap <- if isNothing mtMap
              then do tMap <- loadTemplates acid
                      putStrLn "Loaded parsed templates for the first time."
                      return tMap
              else if M.null $ fromJust mtMap
                     then do tMap <- loadTemplates acid
                             putStrLn "Loaded parsed templates again."
                             return tMap
                     else return $ fromJust mtMap
    putMVar tMapVar tMap

loadTemplates :: AcidState WFTemplateSourceMap -> IO TemplateMap
loadTemplates acid = do
    putStrLn "Loading templates..."
    ks <- query' acid AllTemplateNames
    foldM accumulateTemplate M.empty ks
        where accumulateTemplate = accumulateTemplateFromAcid acid

accumulateTemplateFromAcid :: AcidState WFTemplateSourceMap -> TemplateMap -> String -> IO TemplateMap
accumulateTemplateFromAcid a m k = do
    putStrLn $ "Loading " ++ show k
    mTmp <- query' a $ GetTemplate k
    if isNothing mTmp
      then return m
      else case parseWFTemplate (fromJust mTmp) of
               Right t  -> return $ M.insert k t m
               Left err -> do putStrLn $ T.unpack err
                              return m

