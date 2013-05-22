{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module RequestHelpers (getBody) where

import Happstack.Server
import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString.Lazy.Char8 as B

getBody :: ServerPart B.ByteString
getBody = do
    req <- askRq
    b   <- liftIO $ takeRequestBody req
    case b of
        Just rqbody -> return . unBody $ rqbody
        Nothing     -> return ""
