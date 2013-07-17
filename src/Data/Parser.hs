{-# LANGUAGE OverloadedStrings #-}
module Data.Parser (
    parseWFTemplate
) where

import Types

import Control.Applicative  ( (<*>), (*>), (<*), (<$>), (<|>) )

import Data.Attoparsec.Text
import Data.Char
import Data.List

import qualified Data.Map                        as M
import qualified Data.Text                       as T

{- Big Top -}

parseWFTemplate :: WFTemplate -> Either T.Text Template
parseWFTemplate (WFTemplate WFTBinary src) = Right [FragmentText src]
parseWFTemplate (WFTemplate _ src)  = handle $ parse template src
    where handle r = case r of
                         Partial c      -> handle $ c T.empty
                         Done _ t       -> Right t
                         Fail t ctx err -> Left $ T.concat [ "Parsing failed in contexts "
                                                           , T.pack $ intercalate ", " ctx
                                                           , " with error:\n  "
                                                           , T.pack err
                                                           , " at input "
                                                           , T.take 10 t
                                                           , "..."
                                                           ]

{- Helpers -}

isBracket :: Char -> Bool
isBracket = flip elem "[]"

{- Parsers -}

template :: Parser Template
template = manyTill templateFragment endOfInput

templateFragment :: Parser TemplateFragment
templateFragment =
    fragmentTemplateCommand <|>
    fragmentOutputVariable  <|>
    fragmentText            <|>
    fragmentBracketText

fragmentText :: Parser TemplateFragment
fragmentText = FragmentText <$> takeWhile1 (not . isBracket)
    <?> "FragmentText"

fragmentBracketText :: Parser TemplateFragment
fragmentBracketText = FragmentText <$> (matchNextAndThenNot '[' <|> matchNextAndThenNot ']')

matchNextAndThenNot :: Char -> Parser T.Text
matchNextAndThenNot ch = do
    c  <- char ch
    nc <- notChar ch
    return $ T.cons c $ T.singleton nc
    <?> "MatchNextAndThenNot" ++ [' ',ch]

fragmentOutputVariable :: Parser TemplateFragment
fragmentOutputVariable = FragmentOutput <$> outputVariable

fragmentTemplateCommand :: Parser TemplateFragment
fragmentTemplateCommand = FragmentCommand <$> (fragmentStart *> renderTemplateCommand)

renderTemplateCommand :: Parser TemplateCommand
renderTemplateCommand = string "renderTemplate " *> (RenderTemplateCommand <$> renderTemplateArgs)
    <?> "RenderTemplateCommand"

renderTemplateArgs :: Parser RenderTemplateArgs
renderTemplateArgs = (,) <$> filepath <*> fragmentVariables

filepath :: Parser T.Text
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
fragmentValue = T.pack <$> (fragmentStart *> manyTill anyChar fragmentEnd)
    <?> "FragmentValue"

fragmentStart :: Parser T.Text
fragmentStart = string "[["
    <?> "FragmentStart"

fragmentEnd :: Parser T.Text
fragmentEnd = string "]]"
    <?> "FragmentEnd"

