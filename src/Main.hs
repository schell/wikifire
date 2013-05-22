{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.WikiFire
import RequestHelpers

import Happstack.Server     
import Control.Monad            ( msum, void )
import Control.Monad.IO.Class   ( liftIO )
import Control.Exception        ( bracket )
import Data.Acid                ( AcidState, Query, Update, makeAcidic, openLocalState )
import Data.Acid.Local          ( createCheckpointAndClose )
import Data.Acid.Advanced       ( query', update' )

import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do 
    putStrLn "Running the wikifire server..."
    bracket (openLocalState initialTemplateSourceMap)
            createCheckpointAndClose
            (\acid -> simpleHTTP nullConf $ route acid)

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
         , dir "_templateNames" $ handleGetTemplateNames acid ]

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

