{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.WikiFire
import Types

import Happstack.Server
import Debug.Trace              ( trace )
import Data.Maybe               ( fromMaybe )
import Control.Applicative      ( optional )
import Control.Monad            ( msum )
import Control.Exception        ( bracket )
import Data.Acid                ( AcidState, openLocalState )
import Data.Acid.Local          ( createCheckpointAndClose )
import Data.Acid.Advanced       ( query', update' )

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    putStrLn "Reading default templates..."
    sourceMap <- initialTemplateSourceMap
    putStrLn "Running the wikifire server..."
    bracket (openLocalState sourceMap)
            createCheckpointAndClose
            (simpleHTTP nullConf . route)

route :: AcidState WFTemplateSourceMap -> ServerPart Response
route acid =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
       msum [ do method POST
                 routePOST acid,
              do method GET
                 routeGET  acid ]

routeGET :: AcidState WFTemplateSourceMap -> ServerPart Response
routeGET acid =
    msum [ dir "favicon.ico"    $ notFound (toResponse ())
         , dir "_"              $ uriRest $ \s -> handleGetTemplate acid s
         , dir "_templateNames" $ handleGetTemplateNames acid
         , uriRest              $ \s -> handleRenderTemplate acid s]

routePOST :: AcidState WFTemplateSourceMap -> ServerPart Response
routePOST acid =
    msum [ do nullDir
              handlePostTemplate acid ]

handleGetTemplate :: AcidState WFTemplateSourceMap -> String -> ServerPart Response
handleGetTemplate acid name = do
    mTemplate <- query' acid $ GetTemplate name
    case mTemplate of
        Nothing -> notFound (toResponse ())
        Just t  -> ok $ contentLength $ toResponse $ templateSource t

handlePostTemplate :: AcidState WFTemplateSourceMap -> ServerPart Response
handlePostTemplate acid = do
    name <- look "name"
    src  <- look "source"
    mCt  <- optional $ look "contentType"
    let ct = fromMaybe "text/plain" mCt
    let t = WFTemplate (L.pack ct) (L.pack src) Nothing
    msg <- update' acid $ PostTemplate name t
    ok $ contentLength $ toResponse msg

handleGetTemplateNames :: AcidState WFTemplateSourceMap -> ServerPart Response
handleGetTemplateNames acid = do
    namesJSON <- query' acid GetTemplateNames
    ok $ contentLength $ toResponse namesJSON

handleRenderTemplate :: AcidState WFTemplateSourceMap -> String -> ServerPart Response
handleRenderTemplate acid name = do
    mT <- query' acid $ GetTemplate name
    case mT of
        Nothing                               -> notFound $ toResponse ()
        Just   (WFTemplate ct _ (Just cache)) -> ok $ toResponseBS (C.pack $ L.unpack ct) cache
        Just t@(WFTemplate ct _ Nothing)      -> do
            trace ("Cacheing " ++ name ++ "...") $ return ()
            rendering <- update' acid $ CacheTemplate name t
            ok $ toResponseBS (C.pack $ L.unpack ct) rendering

