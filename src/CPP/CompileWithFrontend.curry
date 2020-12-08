------------------------------------------------------------------------------
--- Auxiliary operations to compile additionally imported modules
--- with the front end.

module CPP.CompileWithFrontend
 where

import Control.Monad               ( when )
import Curry.Compiler.Distribution ( curryCompiler )

import System.FrontendExec

------------------------------------------------------------------------------
--- If a module is added as a new import to the transformed program,
--- we have to compile it in order to avoid a compilation error
--- of the front end, since the front end assumes,
-- if the preprocessor is called, that all imported modules are
-- already compiled.
compileImportedModule :: Int -> String -> IO ()
compileImportedModule verb modname = do
  mapM (compileModuleTo verb modname) [ACY, UACY]
  compileSetFunctions2Flat
 where
  compileSetFunctions2Flat
    | curryCompiler == "kics2"
    = compileModuleTo verb modname TFCY
    | curryCompiler == "pakcs"
    = compileModuleTo verb modname FCY
    | otherwise
    = error $ "compileSetFunctions: unknown Curry compiler '" ++
              curryCompiler ++ "'!"

compileModuleTo :: Int -> String -> FrontendTarget -> IO ()
compileModuleTo verb modname target = do
  when (verb > 2) $ putStrLn $
    "Compiling '" ++ modname ++ "' to '" ++ show target ++ "'..."
  callFrontendWithParams target (setQuiet True defaultParams) modname

------------------------------------------------------------------------------
