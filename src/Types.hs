{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies,
  OverloadedStrings, DeriveDataTypeable, TypeSynonymInstances,
  TemplateHaskell #-}
module Types where

import Data.Aeson
import Data.Typeable          ( Typeable )
import Control.Applicative    ( (<$>), (<*>) )
import Control.Monad          ( mzero )
import Data.SafeCopy          ( base, deriveSafeCopy )

import qualified Data.Map                   as M
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString            as B

-- | The main acid store.
type WFTemplateSourceMap = M.Map String WFTemplate

data WFTemplate = WFTemplate { templateType  :: L.ByteString
                             , templateSource:: L.ByteString
                             , templateCache :: Maybe L.ByteString
                             } deriving (Eq, Typeable)

instance Show WFTemplate where
    show (WFTemplate ct src _) = "WFTemplate { templateType='"++ L.unpack ct ++ "' templateSrc='" ++ suf (trunc src) ++ "'}"
        where suf xs = if length xs >= chars
                       then xs ++ "..."
                       else xs
              trunc  = take chars . L.unpack
              chars  = 10

-- | Represents a tree of template renders.
-- data WFRenderTree = WFRenderTree { rtBranches    :: [WFRenderTree]
--                                  , rtTreeParents :: [WFRenderTree]
--                                  , rtIsDirty     :: Bool
--                                  , rtName        :: String
--                                  , rtCache       :: B.ByteString
--                                  }


-- | Represents a config file.
-- data Config = Config { routeCfgs :: [RouteCfg] }


-- | Each route is a route name that points to a file from
-- which to read the template for that route. The `routeFilePath` is an
-- array of nodes that will be concatenated with the OS's directory
-- separator.
data RouteCfg  = RouteCfg { routeName         :: String       -- ^ Path of the template on the server.
                          , routeContentType  :: Maybe String -- ^ Content type of the route (ie, "text/plain")
                          , routeFilePath     :: [String]     -- ^ Path of the template on disk, each directory being one entry in the array.
                          } deriving (Show, Eq)


$(deriveSafeCopy 0 'base ''WFTemplate)

-- instance FromJSON Config where
--    parseJSON (Object v) = Config <$>
--                           v .: "configRoutes"
--    parseJSON _          = mzero

instance FromJSON RouteCfg where
    parseJSON (Object v) = RouteCfg                    <$>
                              v .:  "routeName"        <*>
                              v .:? "routeContentType" <*>
                              v .:  "routeFilePath"
    parseJSON _          = mzero

{- Parser Types -}

-- | The top level parsed template is a list of fragments.
type Template = [TemplateFragment]

-- | Each fragment can be a sublist of fragments or a command.
data TemplateFragment = FragmentText    B.ByteString    |
                        FragmentCommand TemplateCommand |
                        FragmentOutput  OutputVariable  deriving (Show, Eq)

-- | A command with a name, variable number of arguments and a map of fragment variables.
data TemplateCommand = RenderTemplateCommand RenderTemplateArgs deriving (Show, Eq)

-- | The render template command arguments.
type RenderTemplateArgs = (B.ByteString, InputVariables)

-- | A map for all a template's variables used to fragment to a render.
type InputVariables = M.Map B.ByteString B.ByteString

-- | An variable is a name and a value.
data FragmentVariable     = FragmentVariable FragmentVariableName FragmentValue deriving (Show, Eq)
type FragmentVariableName = B.ByteString
type FragmentValue        = B.ByteString

-- | An output variable is its name. It represents a placeholder for some rendered text.
type OutputVariable = B.ByteString

