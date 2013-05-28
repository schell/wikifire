{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative  ( (<*>), (*>), (<*), (<$>), (<|>), pure )
import Data.Text            ( Text, pack, unpack )
import System.FilePath      ( (</>) )
import Data.Char            ( isSpace )

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator

import qualified Data.Map as M

{- Types -}

-- | The top level parsed template is a list of fragments.
type Template = [TemplateFragment]

-- | Each fragment can be a sublist of fragments or a command.
data TemplateFragment = FragmentText    Text            |
                        FragmentCommand TemplateCommand |
                        FragmentOutput  OutputVariable  deriving (Show, Eq)

-- | A command with a name, variable number of arguments and a map of input variables.
data TemplateCommand = RenderTemplateCommand RenderTemplateArgs deriving (Show, Eq)

-- | The render template command arguments.
type RenderTemplateArgs = (FilePath, InputVariables)

-- | A map for all a template's variables used to input to a render.
type InputVariables = M.Map Text Text

-- | An input variable is a name and a value.
data InputVariable = InputVariable InputVariableName VariableValue deriving (Show, Eq)
type InputVariableName = Text
type VariableValue = Text

-- | An output variable is its name. It represents a placeholder for some rendered text.
type OutputVariable = Text

{- Parsers -}

fragmentText :: Parser TemplateFragment
fragmentText = FragmentText . pack <$> manyTill anyChar fragmentStart
    <?> "FragmentText"
    where fragmentStart = try (string "[{") <|> try (string "[|")

renderTemplateCommand :: Parser TemplateCommand
renderTemplateCommand = RenderTemplateCommand <$> "renderTemplate " .*> renderTemplateArgs
    <?> "RenderTemplateCommand"

renderTemplateArgs :: Parser RenderTemplateArgs
renderTemplateArgs = (,) <$> filepath <*> inputVariables

filepath :: Parser FilePath
filepath = unpack <$> takeTill isSpace
    <?> "FilePath"

inputVariables :: Parser InputVariables
inputVariables = foldVariables <$> manyTill (skipSpace *> inputVariable <* skipSpace) end
    <?> "InputVariables"
    where foldVariables = foldl (\m (InputVariable n v) -> M.insert n v m) M.empty
          end           = try (string "}]")

outputVariable :: Parser OutputVariable
outputVariable = string "[|" *> takeWhile1 (/= '|') <* string "|]"
                 <?> "OutputVariable"

inputVariable :: Parser InputVariable
inputVariable = InputVariable <$> inputVariableName <* skipSpace <*> variableValue
    <?> "InputVariable"

inputVariableName :: Parser InputVariableName
inputVariableName = takeWhile1 (not . isHorizontalSpace)
    <?> "InputVariableName"

variableValue :: Parser VariableValue
variableValue = pack <$> (string "[=" *> manyTill anyChar (try (string "=]")))
    <?> "VariableValue"
