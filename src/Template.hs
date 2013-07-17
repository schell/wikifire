module Template (
    resolveTemplateWithMap
) where

import Types
import Control.Monad.Reader
import qualified Data.Text as T



resolveTemplateWithMap :: Template -> TemplateMap -> T.Text
resolveTemplateWithMap t m = runReader (resolve t)  Env { tMap = m, source = T.empty, recurses = []}
    --trace "compiling template" $ L.concat $ Prelude.map (`compileFragment` m) t

-- compileFragment :: TemplateFragment -- ^ The fragment to render.
--                 -> TemplateMap      -- ^ The map of all templates.
--                 -> L.ByteString
-- compileFragment (FragmentText txt)  _ = trace "compiling frag text" $ L.fromStrict txt
-- compileFragment (FragmentCommand cmd) m = trace "compiling frag command" $ compileCommand cmd m
-- compileFragment (FragmentOutput out) _ = trace "compiling frag output" $ L.fromStrict out
--
-- compileCommand :: TemplateCommand -- ^ The command to render.
--                -> TemplateMap     -- ^ The map of all templates.
--                -> L.ByteString
-- compileCommand (RenderTemplateCommand (name, input)) m = trace "compiling command" $
--     let mT    = M.lookup sname m
--         sname = B.unpack name
--         inOut = Prelude.map (tryCompileOutputVars input) $ fromJust mT
--     in case mT of
--            Nothing -> tryCompileOutputVarsace ("Couldn't find template "++show m) L.empty
--            Just _  -> trace "Found template " $ compileTemplateWithMap inOut m
--
-- tryCompileOutputVars :: InputVariables -> TemplateFragment -> TemplateFragment
-- tryCompileOutputVars input (FragmentOutput out)  = trace "compiling output" $
--     case M.lookup out input of
--         Just val -> FragmentText val
--         Nothing  -> FragmentText B.empty -- Maybe later make this a javascript warning or something?
-- tryCompileOutputVars _ t = t

data Env = Env { tMap     :: TemplateMap
               , source   :: T.Text
               , recurses :: [(String, Int)]
               }

addText :: T.Text -> Env -> Env
addText txt env = env{ source = T.append (source env) txt}

resolve :: Template -> Reader Env T.Text
resolve (FragmentText txt:frags)    = local (addText txt) (resolve frags)
resolve (FragmentOutput out:frags)  = local (addText out) (resolve frags)
resolve (FragmentCommand cmd:frags) = local (addText $ T.pack $ show cmd) (resolve frags)
resolve [] = asks source

-- resolveCommand :: TemplateCommand -> Env -> Env
-- resolveCommand (RenderTemplateCommand (name, input)) =

