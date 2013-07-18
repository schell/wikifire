{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies,
  OverloadedStrings, DeriveDataTypeable, TypeSynonymInstances,
  TemplateHaskell #-}
module Types where

import Data.Aeson
import Control.Concurrent.MVar ( MVar )
import Data.Typeable           ( Typeable )
import Control.Applicative     ( (<$>), (<*>) )
import Control.Monad           ( mzero )
import Data.SafeCopy           ( base, deriveSafeCopy )

import qualified Data.Text       as T
import qualified Data.ByteString as B
import qualified Data.Map        as M

-- | The main acid store.
type WFTemplateSourceMap = M.Map String WFTemplate

data WFTemplateType = WFTTextPlain
                    | WFTTextHtml
                    | WFTTextJavascript
                    | WFTImagePng
                    | WFTApplicationOctetStream
                    deriving (Show, Eq, Typeable)

data WFTemplateSrc = WFTSrcText T.Text
                   | WFTSrcBin B.ByteString
                   deriving (Show,Eq,Typeable)

wfTypeMap :: [(String,WFTemplateType)]
wfTypeMap = [ ("image/png", WFTImagePng)
            , ("text/plain", WFTTextPlain)
            , ("text/html", WFTTextHtml)
            , ("text/javascript", WFTTextJavascript)
            , ("application/octet-stream", WFTApplicationOctetStream)
            ]

wfTypeIsBinary :: WFTemplateType -> Bool
wfTypeIsBinary = (`elem` bins)
    where bins = [WFTImagePng, WFTApplicationOctetStream]

readWFTemplateType :: String -> WFTemplateType
readWFTemplateType s = foldl checkType WFTTextPlain wfTypeMap
    where checkType t (s', t') = if s' == s then t' else t

showWFTemplateType :: WFTemplateType -> String
showWFTemplateType t = foldl checkType "text/plain" wfTypeMap
    where checkType s (s', t') = if t == t' then s' else s

data WFTemplate = WFTemplate { templateType  :: WFTemplateType
                             , templateSource:: WFTemplateSrc
                             } deriving (Eq, Typeable)

instance Show WFTemplate where
    show (WFTemplate ct src) = concat [ "WFTemplate { templateType='"
                                      , show ct
                                      , "' templateSrc='"
                                      , suf src
                                      , "'}"
                                      ]
        where suf (WFTSrcText _) = "TextSource"
              suf (WFTSrcBin _) = "BinarySource"

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

$(deriveSafeCopy 0 'base ''WFTemplateType)
$(deriveSafeCopy 0 'base ''WFTemplateSrc)
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

type TemplateMap = (M.Map String Template)
type TemplateMapState = MVar TemplateMap

-- | The top level parsed template is a type and a list of fragments.
data Template = Template WFTemplateType [TemplateFragment]
              | Binary WFTemplateType B.ByteString
              deriving (Show, Eq)

templateTypeIsBinary :: Template -> Bool
templateTypeIsBinary (Template _ _) = False
templateTypeIsBinary (Binary _ _) = True

getTemplateType :: Template -> WFTemplateType
getTemplateType (Template t _) = t
getTemplateType (Binary t _) = t

-- | Each fragment can be a sublist of fragments or a command.
data TemplateFragment = FragmentText    T.Text          |
                        FragmentCommand TemplateCommand |
                        FragmentOutput  OutputVariable  deriving (Show, Eq)

-- | A command with a name, variable number of arguments and a map of fragment variables.
data TemplateCommand = RenderTemplateCommand RenderTemplateArgs deriving (Show, Eq)

-- | The render template command arguments.
type RenderTemplateArgs = ( T.Text          -- ^ The name of the template to render.
                          , InputVariables  -- ^ The input variables to render as the
                          )                 --   rendered template's output variables.

-- | A map for all a template's variables used to fragment to a render.
type InputVariables = M.Map T.Text T.Text

-- | A variable is a name and a value.
data FragmentVariable     = FragmentVariable FragmentVariableName FragmentValue deriving (Show, Eq)
type FragmentVariableName = T.Text
type FragmentValue        = T.Text

-- | An output variable is its name. It represents a placeholder for some rendered text.
type OutputVariable = T.Text

