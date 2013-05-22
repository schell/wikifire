{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TypeFamilies, OverloadedStrings,
    DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances #-}
module Data.WikiFire where

import RequestHelpers

import Data.SafeCopy        
import Data.Acid
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe           ( fromJust )
import Control.Monad.State  ( get, put )
import Control.Monad.Reader ( ask )
import Control.Applicative  ( (<$>) )

import qualified Data.Map             as M
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T

-- Acid
type TemplateSourceMap = M.Map String String

initialTemplateSourceMap :: TemplateSourceMap
initialTemplateSourceMap = M.empty

postTemplate :: String -> String -> Update TemplateSourceMap B.ByteString
postTemplate name src = do
    sourceMap <- get
    let newMap = M.insert name src sourceMap
    put newMap 
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

