{-# LANGUAGE OverloadedStrings #-}
module Data.Parser ( parseWFTemplate ) where

import Types

import Control.Applicative  ( (<*>), (*>), (<*), (<$>), (<|>) )
import Data.Word            ( Word8 )

import Data.Attoparsec.ByteString

import qualified Data.Attoparsec.ByteString.Lazy as LP
import qualified Data.Map                        as M
import qualified Data.ByteString                 as B

{- Big Top -}

parseWFTemplate :: WFTemplate -> Either String Template
parseWFTemplate (WFTemplate ct src _)  =
    case LP.parse template src of
        LP.Fail t ctx err  -> Left $ "Template parse errored" 
        LP.Done _ template -> Right template

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

