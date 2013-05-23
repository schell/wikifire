{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TypeFamilies, OverloadedStrings,
    DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances #-}
module Data.WikiFire where

import Data.SafeCopy        
import Data.Acid
import Data.Aeson
import Data.Aeson.Types
import Paths_wikifire
import System.Directory        
import Data.List              ( intercalate )
import Data.Maybe             ( fromJust )
import Control.Monad          ( foldM )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.State    ( get, put )
import Control.Monad.Reader   ( ask )
import Control.Applicative    ( (<$>) )
import System.FilePath        ( (</>) )

import qualified Data.Map             as M
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T

-- Acid
type TemplateSourceMap = M.Map String String

initialTemplateSourceMap :: IO TemplateSourceMap
initialTemplateSourceMap = do
    datadir <- getDataDir
    let templatesDir = datadir </> "templates"
    templates <- allTemplates templatesDir
    foldM (\m t -> do
        contents <- readFile t
        let name = drop (length templatesDir) t
        putStrLn $ "  " ++ name ++ " -> " ++ t
        return $ M.insert name contents m
        ) M.empty templates


allTemplates :: FilePath -> IO [FilePath]
allTemplates dir = do
    putStrLn $ "Reading contents of " ++ dir
    paths    <- getDirectoryContents dir
    (dirs,ts)<- separate dir $ clean dir paths
    tree     <- mapM allTemplates $ clean dir dirs
    return $ ts ++ concat tree
        where clean root    = foldl (\acc fp -> if head fp == '.' then acc else (root </> fp):acc) []
              separate root = foldM (\(ds,fs) fp -> do let f = root </> fp
                                                       fe <- doesFileExist f 
                                                       return $ if fe 
                                                                then (ds,f:fs) 
                                                                else (f:ds,fs)) ([],[])

postTemplate :: String -> String -> Update TemplateSourceMap B.ByteString
postTemplate name src = do
    sourceMap      <- get
    put $ M.insert name src sourceMap
    return $ jsonMsg True [ T.pack "name"   .= name
                          , T.pack "source" .= src
                          , T.pack "bytes"  .= length src ] 

getTemplate :: String -> Query TemplateSourceMap (Maybe String)
getTemplate name = M.lookup name <$> ask 

getTemplateNames :: Query TemplateSourceMap B.ByteString
getTemplateNames = encode . M.keys <$> ask

jsonMsg :: Bool -> [Pair] -> B.ByteString
jsonMsg ok pairs = let okay = "ok" .= ok in
    encode $ object $ okay:pairs

$(makeAcidic ''TemplateSourceMap [ 'postTemplate
                                 , 'getTemplate
                                 , 'getTemplateNames ])


