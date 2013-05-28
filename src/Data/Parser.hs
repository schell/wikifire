{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative  ( (<*>), (*>), (<*), (<$>), (<|>), pure )
import System.FilePath      ( (</>) )
import Data.Word            ( Word8 )

import Data.Attoparsec.ByteString
import Data.Attoparsec.Combinator

import qualified Data.Map        as M
import qualified Data.ByteString as B

{- Types -}

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

{- Helpers -}

skipSpace :: Parser ()
skipSpace = skipWhile isSpace

isSpace :: Word8 -> Bool
isSpace = flip B.elem "\n \t\r"

isBracket :: Word8 -> Bool
isBracket = flip B.elem "[]"

{- Parsers -}

template :: Parser Template
template = manyTill templateFragment endOfInput

templateFragment :: Parser TemplateFragment
templateFragment =
    fragmentTemplateCommand <|>
    fragmentOutputVariable  <|>
    fragmentText

fragmentText :: Parser TemplateFragment
fragmentText = FragmentText <$> takeWhile1 (not . isBracket)
    <?> "FragmentText"

fragmentOutputVariable :: Parser TemplateFragment
fragmentOutputVariable = FragmentOutput <$> outputVariable

fragmentTemplateCommand :: Parser TemplateFragment
fragmentTemplateCommand = FragmentCommand <$> (fragmentStart *> renderTemplateCommand)

renderTemplateCommand :: Parser TemplateCommand
renderTemplateCommand = string "renderTemplate " *> (RenderTemplateCommand <$> renderTemplateArgs)
    <?> "RenderTemplateCommand"

renderTemplateArgs :: Parser RenderTemplateArgs
renderTemplateArgs = (,) <$> filepath <*> fragmentVariables

filepath :: Parser B.ByteString
filepath = takeTill isSpace
    <?> "FilePath"

outputVariable :: Parser OutputVariable
outputVariable = fragmentStart *> takeWhile1 (\c -> not (isSpace c) && not (isBracket c)) <* fragmentEnd
    <?> "OutputVariable"

fragmentVariables :: Parser InputVariables
fragmentVariables = option M.empty (foldVariables <$> manyTill (skipSpace *> fragmentVariable <* skipSpace) fragmentEnd)
    <?> "InputVariables"
    where foldVariables = foldl (\m (FragmentVariable n v) -> M.insert n v m) M.empty

fragmentVariable :: Parser FragmentVariable
fragmentVariable = FragmentVariable <$> fragmentVariableName <* skipSpace <*> fragmentValue
    <?> "FragmentVariable"

fragmentVariableName :: Parser FragmentVariableName
fragmentVariableName = takeWhile1 (not . isSpace)
    <?> "FragmentVariableName"

fragmentValue :: Parser FragmentValue
fragmentValue = B.pack <$> (fragmentStart *> manyTill anyWord8 fragmentEnd)
    <?> "FragmentValue"

fragmentStart :: Parser B.ByteString
fragmentStart = string "[["
    <?> "FragmentStart"

fragmentEnd :: Parser B.ByteString
fragmentEnd = string "]]"
    <?> "FragmentEnd"

