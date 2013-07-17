{-# LANGUAGE OverloadedStrings #-}
module Template (
    resolveTemplateWithMap
) where

import Types
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Map  as M
import qualified Data.Text as T

data Env = Env { tMap     :: TemplateMap
               , source   :: T.Text
               , recurses :: [(String, Int)]
               , inVars   :: InputVariables
               }


resolveTemplateWithMap :: Template -> TemplateMap -> T.Text
resolveTemplateWithMap t m = runReader (resolve t) $ makeEnv m

makeEnv :: TemplateMap -> Env
makeEnv m = Env m T.empty (map (flip (,) 0) $ M.keys m) M.empty

addText :: T.Text -> Env -> Env
addText txt env = env{ source = T.append (source env) txt}

addRecurse :: String -> Env -> Env
addRecurse k e = e{ recurses = map incK $ recurses e }
    where incK (n, i) = if n == k then (n, i+1) else (n, i)

getRecurses :: String -> Env -> Int
getRecurses k = foldl g 0 . recurses
    where g a (n, i) = if a == 0
                         then if n == k
                                then i
                                else 0
                         else a

resolve :: Template -> Reader Env T.Text
resolve (FragmentText txt:frags)    = local (addText txt) $ resolve frags
resolve (FragmentOutput out:frags)  = local (addOutVars out) $ resolve frags
resolve (FragmentCommand cmd:frags) = local (addCommand cmd) $ resolve frags
resolve [] = asks source

addOutVars :: OutputVariable -> Env -> Env
addOutVars out e = addText (fromMaybe (outVarError out) mInVal) e
    where mInVal = M.lookup out $ inVars e

addCommand :: TemplateCommand -> Env -> Env
addCommand (RenderTemplateCommand (n, invrs)) e =
    let n' = T.unpack n
        m  = tMap e
        mT = M.lookup n' m
        e' = (addRecurse n' e){ inVars = invrs }
        r  = getRecurses n' e'
        tx = runReader (resolve $ fromJust mT) e'
    in if isJust mT
         then if r < 10
                then e{ source = tx }
                else addText (recurseError n) e
         else addText (missingTemplateError n) e

recurseError :: T.Text -> T.Text
recurseError = (`T.append` " has been included recursively, which may have formed a loop.")

missingTemplateError :: T.Text -> T.Text
missingTemplateError = (`T.append` " could not be found.")

outVarError :: OutputVariable -> T.Text
outVarError o = T.unwords [ "Variable"
                          , o
                          , "could not be found in input."
                          ]
