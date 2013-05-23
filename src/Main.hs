{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.WikiFire

import Happstack.Server     
import Control.Monad            ( msum, void )
import Control.Monad.IO.Class   ( liftIO )
import Control.Exception        ( bracket )
import Data.Acid                ( AcidState, Query, Update, makeAcidic, openLocalState )
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

route :: AcidState TemplateSourceMap -> ServerPart Response
route acid = 
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
       msum [ do method POST
                 routePOST acid,
              do method GET 
                 routeGET  acid ]

routeGET :: AcidState TemplateSourceMap -> ServerPart Response
routeGET acid =
    msum [ dir "favicon.ico"    $ notFound (toResponse ())
         , dir "_"              $ uriRest $ \s -> handleGetTemplate acid s
         , dir "_templateNames" $ handleGetTemplateNames acid 
         , uriRest              $ \s -> handleRenderTemplate acid s]

routePOST :: AcidState TemplateSourceMap -> ServerPart Response
routePOST acid = 
    msum [ do nullDir 
              handlePostTemplate acid ] 

handleGetTemplate :: AcidState TemplateSourceMap -> String -> ServerPart Response 
handleGetTemplate acid name = do
    mTemplate <- query' acid $ GetTemplate name
    case mTemplate of
        Nothing -> notFound (toResponse ())
        Just t  -> ok $ contentLength $ toResponse t 

handlePostTemplate :: AcidState TemplateSourceMap -> ServerPart Response 
handlePostTemplate acid = do
    name <- look "name"
    src  <- look "source"
    msg <- update' acid (PostTemplate name src)
    ok $ contentLength $ toResponse msg

handleGetTemplateNames :: AcidState TemplateSourceMap -> ServerPart Response
handleGetTemplateNames acid = do 
    namesJSON <- query' acid GetTemplateNames
    ok $ contentLength $ toResponse namesJSON

handleRenderTemplate :: AcidState TemplateSourceMap -> String -> ServerPart Response
handleRenderTemplate acid name = do
    mSrc <- query' acid $ GetTemplate name
    case mSrc of
        Nothing -> notFound $ toResponse ()
        Just t  -> ok $ toResponseBS (C.pack "text/html") (L.pack t)
