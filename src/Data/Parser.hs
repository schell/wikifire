{-# LANGUAGE OverloadedStrings #-}
module Data.Parser ( parseWFTemplate ) where

import Types

import Control.Applicative  ( (<*>), (*>), (<*), (<$>), (<|>) )
import Data.Word            ( Word8 )

import Data.Attoparsec.ByteString

import qualified Data.Attoparsec.ByteString.Lazy as LP
import qualified Data.Map                        as M
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as LB
import qualified Data.ByteString.Lazy.Char8      as LBC

{- Big Top -}

parseWFTemplate :: WFTemplate -> Either LB.ByteString Template
parseWFTemplate (WFTemplate _ src)  =
    case LP.parse template src of
        LP.Done _ t       -> Right t
        LP.Fail t ctx err -> Left $ errMsg t (map LBC.pack ctx) (LBC.pack err)

    where errMsg t c e = LB.intercalate "\n" [ errPosMsg t
                                             , errCtx c
                                             , "error: " `LB.append` e
                                             ]
          errPosMsg t  = "Template parse errored at: " `LB.append` if LB.length t <= 10 
                                                                  then t 
                                                                  else LB.take 10 t `LB.append` "..."
          errCtx ctx   = "\n  context:" `LB.append` LB.intercalate "    \n" ctx
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

