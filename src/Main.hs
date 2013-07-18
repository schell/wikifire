{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Main where

import Data.WikiFire
import Data.Parser
import Types
import Template
import Startup

import Happstack.Server
import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text.Encoding

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
main = start run

run :: Options -> IO ()
run opts = do
    let datadir = optDataDir opts
    putStrLn $ "Reading routes from " ++ show datadir
    sourceMap <- initialTemplateSourceMap datadir
    putStrLn "Creating template web..."
    tMap      <- newEmptyMVar
    putStrLn "Running the wikifire server..."
    bracket (openLocalState sourceMap)
            createCheckpointAndClose
            (simpleHTTP nullConf . flip route tMap)

route :: AcidState WFTemplateSourceMap -> TemplateMapState -> ServerPart Response
route acid tMapVar = do
    mtMap <- liftIO $ tryTakeMVar tMapVar
    tMap <- if isNothing mtMap
              then do tMap <- liftIO $ loadTemplates acid
                      liftIO $ putStrLn "Loaded parsed templates for the first time."
                      return tMap
              else if M.null $ fromJust mtMap
                     then do tMap <- liftIO $ loadTemplates acid
                             liftIO $ putStrLn "Loaded parsed templates again."
                             return tMap
                     else return $ fromJust mtMap
    liftIO $ putMVar tMapVar tMap
    decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
    msum [ do method POST
              routePOST acid tMapVar
         , do method GET
              routeGET  acid tMapVar
         ]

routeGET :: AcidState WFTemplateSourceMap -> TemplateMapState -> ServerPart Response
routeGET acid tMap =
    msum [ dir "favicon.ico"    $ notFound (toResponse ())
         , dir "_"              $ uriRest $ \s -> handleGetTemplate acid s
         , dir "_templateNames" $ handleGetTemplateNames acid
         , uriRest              $ \s -> handleRenderTemplate acid tMap s]

routePOST :: AcidState WFTemplateSourceMap -> TemplateMapState -> ServerPart Response
routePOST acid tMapVar =
    msum [ do nullDir
              handlePostTemplate acid tMapVar]

handleGetTemplate :: AcidState WFTemplateSourceMap -> String -> ServerPart Response
handleGetTemplate acid name = do
    mTemplate <- query' acid $ GetTemplate name
    case mTemplate of
        Nothing -> notFound $ toResponse ()
        Just t  -> okGetTemplate t

okGetTemplate :: WFTemplate -> ServerPart Response
okGetTemplate (WFTemplate typ (WFTSrcBin b)) =
    ok $ contentLength $ toResponseBS (C.pack $ showWFTemplateType typ) $ L.fromStrict b
okGetTemplate (WFTemplate _ (WFTSrcText t)) =
    ok $ contentLength $ toResponseBS (C.pack "text/plain") $ L.fromStrict $ encodeUtf8 t

handlePostTemplate :: AcidState WFTemplateSourceMap -> TemplateMapState -> ServerPart Response
handlePostTemplate acid tMapVar = do
    name <- look "name"
    src  <- look "source"
    mCt  <- optional $ look "contentType"
    tMap <- liftIO $ getTemplateMap tMapVar
    void $ liftIO $ putStrLn $ "Posting template to " ++ show name
    let ct   = maybe WFTTextPlain readWFTemplateType mCt
        eTmp = parseWFTemplate wft
        wft  = WFTemplate ct $ if wfTypeIsBinary ct
                                 then WFTSrcBin $ C.pack src
                                 else WFTSrcText $ T.pack src
    case eTmp of
        Left err -> ok $ contentLength $ toResponse err
        Right t  -> do
            liftIO $ void $ tryTakeMVar tMapVar
            success <- liftIO $ tryPutMVar tMapVar $ M.insert name t tMap
            if success
              then do msg <- update' acid $ PostTemplate name wft
                      ok $ contentLength $ toResponse msg
              else ok $ contentLength $ toResponse ("Could not store parsed template." :: L.ByteString)

handleGetTemplateNames :: AcidState WFTemplateSourceMap -> ServerPart Response
handleGetTemplateNames acid = do
    namesJSON <- query' acid GetTemplateNames
    ok $ contentLength $ toResponse namesJSON

handleRenderTemplate :: AcidState WFTemplateSourceMap -> TemplateMapState -> String -> ServerPart Response
handleRenderTemplate acid tMapVar name = do
    tMap  <- liftIO $ getTemplateMap tMapVar
    case M.lookup name tMap of
        Just t  -> renderTemplate t tMap
        Nothing -> do eitherT <- renderTemplateFromAcid acid name tMapVar
                      case eitherT of
                          Left msg -> ok $ toResponse msg
                          Right t  -> renderTemplate t tMap

renderTemplateFromAcid :: AcidState WFTemplateSourceMap -> String -> TemplateMapState -> ServerPart (Either T.Text Template)
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

renderTemplate :: Template -> TemplateMap -> ServerPart Response
renderTemplate t m =
    ok $ contentLength $ toResponseBS (C.pack $ showWFTemplateType typ) $ L.fromStrict payload
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

