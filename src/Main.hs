{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.WikiFire
import Types

import Happstack.Server
import Control.Concurrent.MVar

import Debug.Trace              ( trace )
import Data.Maybe               ( fromMaybe )
import Control.Applicative      ( optional )
import Control.Monad            ( msum )
import Control.Monad.IO.Class   ( liftIO )
import Control.Exception        ( bracket )
import Data.Acid                ( AcidState, openLocalState )
import Data.Acid.Local          ( createCheckpointAndClose )
import Data.Acid.Advanced       ( query', update' )

import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as M

type TemplateMapState = MVar (M.Map String Template)

main :: IO ()
main = do
    putStrLn "Reading default templates..."
    sourceMap <- initialTemplateSourceMap
    putStrLn "Creating template web..."
    tMap      <- newEmptyMVar
    putStrLn "Running the wikifire server..."
    bracket (openLocalState sourceMap)
            createCheckpointAndClose
            (simpleHTTP nullConf . flip route tMap)

route :: AcidState WFTemplateSourceMap -> TemplateMapState -> ServerPart Response
route acid tMap =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
       msum [ do method POST
                 routePOST acid tMap,
              do method GET
                 routeGET  acid tMap ]

routeGET :: AcidState WFTemplateSourceMap -> TemplateMapState -> ServerPart Response
routeGET acid tMap =
    msum [ dir "favicon.ico"    $ notFound (toResponse ())
         , dir "_"              $ uriRest $ \s -> handleGetTemplate acid s
         , dir "_templateNames" $ handleGetTemplateNames acid
         , uriRest              $ \s -> handleRenderTemplate acid tMap s]

routePOST :: AcidState WFTemplateSourceMap -> TemplateMapState -> ServerPart Response
routePOST acid tMap =
    msum [ do nullDir
              handlePostTemplate acid tMap]

handleGetTemplate :: AcidState WFTemplateSourceMap -> String -> ServerPart Response
handleGetTemplate acid name = do
    mTemplate <- query' acid $ GetTemplate name
    case mTemplate of
        Nothing -> notFound (toResponse ())
        Just t  -> ok $ contentLength $ toResponse $ templateSource t

handlePostTemplate :: AcidState WFTemplateSourceMap -> TemplateMapState -> ServerPart Response
handlePostTemplate acid tMap = do
    name <- look "name"
    src  <- look "source"
    mCt  <- optional $ look "contentType"
    let ct = fromMaybe "text/plain" mCt
    let t  = WFTemplate (L.pack ct) (L.pack src)
    msg <- update' acid $ PostTemplate name t
    ok $ contentLength $ toResponse msg

handleGetTemplateNames :: AcidState WFTemplateSourceMap -> ServerPart Response
handleGetTemplateNames acid = do
    namesJSON <- query' acid GetTemplateNames
    ok $ contentLength $ toResponse namesJSON

handleRenderTemplate :: AcidState WFTemplateSourceMap -> TemplateMapState -> String -> ServerPart Response
handleRenderTemplate acid tMapVar name = do
    mMap  <- liftIO $ tryTakeMVar tMapVar
    tMap  <- case mMap of
                 Nothing  -> return M.empty
                 Just map -> return map
    bytes <- case M.lookup name tMap of
                 Nothing -> renderTemplateFromAcid acid name
                 Just t  -> renderTemplate t
    ok $ toResponse bytes

renderTemplateFromAcid :: AcidState WFTemplateSourceMap -> String -> ServerPart L.ByteString
renderTemplateFromAcid acid name = undefined

renderTemplate :: Template -> ServerPart L.ByteString
renderTemplate t = undefined
