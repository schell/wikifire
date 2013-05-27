{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative  ( (<*>), (*>), (<*), (<$>), (<|>), pure )
import Data.Text            ( Text, pack )

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator

import qualified Data.Map                   as M

{- Types -}

-- | The top level parsed template is a list of fragments.
type Template = [TemplateFragment]

-- | Each fragment can be a sublist of fragments or a command.
data TemplateFragment = Text            |
                        TemplateCommand |
                        OutputVariable  deriving (Show, Eq)

-- | A command with a name, variable number of arguments and a map of input variables.
data TemplateCommand = CommandName [Text] InputVariables deriving (Show, Eq)

-- | This is a list of every command name. There should not be much here :\.
data CommandName = RenderTemplateCommand deriving (Show, Eq)

-- | A map for all a template's variables used to input to a render.
type InputVariables = M.Map Text Text

-- | An input variable is a name and a value.
data InputVariable = InputVariable InputVariableName VariableValue deriving (Show, Eq)
type InputVariableName = Text
type VariableValue = Text

-- | An output variable is its name. It represents a placeholder for some rendered text.
type OutputVariable = Text

{- Parsers -}

outputVariable :: Parser OutputVariable
outputVariable = string "[|" *> takeWhile1 (/= '|') <* string "|]"
                 <?> "Parser OutputVariable"

inputVariableName :: Parser InputVariableName
inputVariableName = takeWhile1 (not . isHorizontalSpace)

variableValue :: Parser VariableValue
variableValue = pack <$> (string "[=" *> manyTill anyChar (try (string "=]")))
