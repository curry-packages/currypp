------------------------------------------------------------------------------
--- The main module of currypp, the Curry Preprocessor
--- ===================================================
---
--- The Curry Preprocessor transforms the source code of Curry programs.
--- Currently, only the translation of foreign code integrated in Curry code
--- is supported (option `foreigncode`, see module `Translator`).
---
--- @author Michael Hanus
--- @version April 2023
------------------------------------------------------------------------------

import Control.Monad        ( when )
import Data.Char            ( digitToInt )
import Data.List
import System.Directory     ( copyFile, renameFile )
import System.FilePath

import AbstractCurry.Types
import AbstractCurry.Files  ( readCurry, readUntypedCurry )
import AbstractCurry.Pretty ( showCProg )
import System.CurryPath     ( stripCurrySuffix )
import System.CPUTime       ( getCPUTime )
import System.Environment   ( getEnv, getArgs, setEnv )
import System.Process       ( exitWith )

import CPP.DefaultRules     ( translateDefaultRulesAndDetOps )
import CPP.Config           ( packageVersion )
import CPP.Contracts        ( translateContracts )
import CPP.ICode.TransICode ( translateIntCode )

------------------------------------------------------------------------------
cppBanner :: String
cppBanner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText = "Curry Preprocessor (Version " ++
                packageVersion ++ " of 29/01/2024)"
   bannerLine = take (length bannerText) (repeat '=')

--- Preprocessor targets, i.e., kind of entities to be preprocessed:
data PPTarget = ForeignCode | DefaultRules | Contracts
 deriving Eq

parseTarget :: String -> Maybe PPTarget
parseTarget t | t == "foreigncode"  = Just ForeignCode
              | t == "defaultrules" = Just DefaultRules
              | t == "contracts"    = Just Contracts
              | otherwise           = Nothing

--- Preprocessor options:
data PPOpts =
  PPOpts { optHelp      :: Bool
         , optSave      :: Bool       -- save the transformed program?
         , optVerb      :: Int        -- verbosity
         , optTgts      :: [PPTarget] -- targets of the preprocessor
         , optModel     :: String     -- model for the SQL preprocessor
         , optDefRules  :: [String]   -- options for DefaultRules
         , optContracts :: [String]   -- options for Contracts
         }

initOpts :: PPOpts
initOpts = PPOpts { optHelp      = False
                  , optSave      = False
                  , optVerb      = 1
                  , optTgts      = []
                  , optModel     = ""
                  , optDefRules  = []
                  , optContracts = []
                  }

--- The main function of the Curry Preprocessor.
main :: IO ()
main = do
  args <- getArgs
  case args of
    (orgSourceFile:inFile:outFile:options) ->
       maybe (showUsage args)
             (\opts ->
               if optHelp opts
                 then putStrLn (cppBanner ++ usageText) >> exitWith 1
                 else do
                  cpath <- getEnv "CURRYPATH"
                  let (mbdir, modname) = pathToModName cpath orgSourceFile
                  when (optVerb opts > 1) $ putStr cppBanner
                  when (optVerb opts > 2) $ putStr $ unlines
                    ["CURRYPATH          : " ++ cpath
                    ,"Module name        : " ++ modname
                    ,"Original file name : " ++ orgSourceFile
                    ,"Input    file name : " ++ inFile
                    ,"Output   file name : " ++ outFile ]
                  addDir2CurryPath opts cpath mbdir $
                    preprocess opts modname orgSourceFile inFile outFile
                  when (optSave opts) $ saveFile orgSourceFile outFile
                  when (optVerb opts > 3) $ do
                    putStrLn "TRANSFORMED PROGRAM:"
                    putStrLn "===================="
                    readFile outFile >>= putStrLn
                    putStrLn "--------------------"
             )
             (processOptions initOpts options)
    _ -> maybe (showUsage args)
               (\opts -> if optHelp opts
                           then putStrLn (cppBanner ++ usageText)
                           else showUsage args)
               (processOptions initOpts args)
 where
  -- extend CURRYPATH with a directory (if necessary) and execute last argument
  addDir2CurryPath _    _     Nothing    act = act
  addDir2CurryPath opts cpath (Just dir) act
    | dir == "." = act
    | otherwise = do
      when (optVerb opts > 2) $ putStrLn $
        "Adding directory '" ++ dir ++ "' to CURRYPATH"
      let newcpath = if null cpath then dir
                                   else dir ++ [searchPathSeparator] ++ cpath
      setEnv "CURRYPATH" newcpath
      act
      setEnv "CURRYPATH" cpath
 
  saveFile orgSourceFile outFile = do
    let sFile = orgSourceFile++".CURRYPP"
    copyFile outFile sFile
    putStrLn $ "Translated Curry file written to '" ++ sFile ++ "'"

processOptions :: PPOpts -> [String] -> Maybe PPOpts
processOptions opts optargs = case optargs of
    []                -> Just opts
    ("--help":_)      -> Just opts { optHelp = True}
    ("-h":_)          -> Just opts { optHelp = True}
    ("-?":_)          -> Just opts { optHelp = True}
    ("-o":os)         -> processOptions opts { optSave = True } os
    ("-v":os)         -> processOptions opts { optVerb = 2 } os
    (['-','v',vl]:os) -> if isDigit vl
                         then processOptions opts { optVerb = digitToInt vl } os
                         else Nothing
    (('-':'-':ts):os) -> if isPrefixOf "model:" ts
                         then processOptions
                                opts {optModel = tail (dropWhile (/=':') ts) }
                                os
                         else Nothing
    (o:os)  -> if o `elem` ["-e","-t"]
               then processOptions
                      opts {optContracts = optContracts opts ++ [o]} os
               else
                if o `elem` ["nodupscheme","specscheme"]
                then processOptions
                       opts {optDefRules = optDefRules opts ++ [o]} os
                else
                  maybe Nothing
                        (\t -> processOptions
                                 opts {optTgts = t : optTgts opts} os)
                        (parseTarget o)

showUsage :: [String] -> IO ()
showUsage args = do
  putStr cppBanner
  putStrLn $ "\nERROR: Illegal arguments: " ++ unwords args ++ "\n"
  putStrLn usageText
  exitWith 1

usageText :: String
usageText = unlines $
 [ ""
 , "Usage: currypp <OrgFileName> <InputFilePath> <OutputFilePath> <options>\n"
 , "<OrgFileName>   : name of original program source file"
 , "<InputFilePath> : name of the actual input file"
 , "<OutputFilePath>: name of the file where output should be written\n"
 , "where <options> contain preprocessing targets"
 , "(if no target is given, 'foreigncode defaultrules contracts' are used)\n"
 , "foreigncode  : translate foreign code pieces in the source file"
 , "--model:<ERD_Name>_UniSQLCode.info :"
 , "               data model to translate embedded SQL statements"
 , "defaultrules : implement default rules"
 , "contracts    : implement dynamic contract checking"
 , ""
 , "and optional settings:"
 , "-o           : store output also in file <OrgFileName>.CURRYPP"
 , "-v           : same as -v2"
 , "-v<n>        : show more information about the preprocessor:"
 , "               <n>=0 : quiet"
 , "               <n>=1 : show some information (default)"
 , "               <n>=2 : show more information, e.g., version, timing"
 , "               <n>=3 : show much more information, e.g., used file names"
 , "               <n>=4 : show also transformed Curry program"
 , "--help|-h|-? : show help message and quit"
 , ""
 , "For target 'defaultrules':"
 , "specscheme   : default translation scheme (as in PADL'16 paper)"
 , "nodupscheme  : translation scheme without checking conditions twice"
 , ""
 , "For target 'contracts':"
 , "-e           : encapsulate nondeterminism of assertions"
 , "-t           : assert contracts only to top-level (not recursive) calls"
 ]

-- Start the Curry preprocessor:
preprocess :: PPOpts -> String -> String -> String -> String -> IO ()
preprocess opts modname orgfile infile outfile
  | null pptargets
  = -- no target specified: apply all reasonable transformations
    preprocess opts { optTgts = [ForeignCode, DefaultRules, Contracts] }
               modname orgfile infile outfile
  | otherwise
  = do let savefile = orgfile++".SAVEPPORG"
       starttime <- getCPUTime
       renameFile orgfile savefile
       srcprog <- readFile (if orgfile==infile then savefile else infile)
                    >>= return . replaceOptionsLine
       -- remove currypp option to avoid recursive preprocessor calls:
       writeFile orgfile srcprog
       outtxt <- catch (callPreprocessors opts (optionLines srcprog)
                                          modname srcprog orgfile)
                       (\err -> renameFile savefile orgfile >> ioError err)
       writeFile outfile outtxt
       renameFile savefile orgfile
       stoptime <- getCPUTime
       when (optVerb opts > 1) $ putStrLn
         ("Transformation time: " ++
         show (stoptime-starttime) ++ " ms")
 where
  pptargets = optTgts opts

-- Invoke the various preprocessors. The arguments are:
-- * the preprocessor options
-- * the parser options lines to be added if the source text is written
-- * the name of the module
-- * the source text of the module (maybe modified by the code integrator)
-- * the file name of the original module (to overwrite it by some pass)
callPreprocessors :: PPOpts -> String -> String -> String -> String
                  -> IO String
callPreprocessors opts optlines modname srcprog orgfile
  | ForeignCode `elem` pptargets
  = do icouttxt <- translateIntCode verb (optModel opts) orgfile srcprog
       if null (intersect [DefaultRules, Contracts] pptargets)
        then return icouttxt -- no further preprocessors
        else do writeFile orgfile icouttxt
                let rpptargets = delete ForeignCode pptargets
                callPreprocessors opts {optTgts = rpptargets}
                                  optlines modname icouttxt orgfile
  | DefaultRules `elem` pptargets
  = do -- specific handling since DefaultRules requires and process
       -- untyped Curry but Contracts requires typed Curry:
       mbdefprog <- readUntypedCurry modname >>=
                    translateDefaultRulesAndDetOps verb defopts srcprog
       let newsrcprog = maybe srcprog showCProg mbdefprog
       if Contracts `elem` pptargets
        then do
          maybe (return ())
                (\defprog -> writeFile orgfile (optlines ++ showCProg defprog))
                mbdefprog
          readCurry modname >>= translateContracts verb contopts modname
                                                   srcprog 
                            >>= return . maybe newsrcprog showCProg
        else return newsrcprog
  | Contracts `elem` pptargets
  = readCurry modname >>= translateContracts verb contopts modname srcprog
                      >>= return . maybe srcprog showCProg
  | otherwise
  = error "currypp internal error during dispatching"
 where
  pptargets = optTgts opts
  verb      = optVerb opts
  defopts   = optDefRules opts
  contopts  = optContracts opts

--- Transforms a file path name for a module back into a hierarchical module
--- name since only the file path of a module is passed to the preprocessor.
--- This is done if the file path name is prefixed by a directory in the
--- `currypath`, otherwise the directory of the path is returned in
--- the first result component and the plain base name in the second.
--- This might be wrong for a hierachical module not occurring in the
--- `currypath`, but in this case it is difficult to reconstruct
--- the original module name from the file path without looking inside the
--- module.
pathToModName :: String -> String -> (Maybe String, String)
pathToModName currypath psf =
  tryRemovePathPrefix (prefixLast (splitSearchPath currypath))
 where
  pp = stripCurrySuffix psf

  tryRemovePathPrefix [] =
    let (dir,bname) = splitFileName pp
    in (Just (dropTrailingPathSeparator dir), bname)
  tryRemovePathPrefix (dir:dirs)
    | dir `isPrefixOf` pp = (Nothing, dirPath2mod $ drop (length dir + 1) pp)
    | otherwise           = tryRemovePathPrefix dirs

  -- transform dir-path name into hierarchical module name
  dirPath2mod = intercalate "." . splitDirectories

  -- move directories which are prefixed by another one in the path
  -- before the other ones, e.g.,
  --     prefixLast ["src","aaa","src/ab"] == ["src/ab","src","aaa"]
  prefixLast []     = []
  prefixLast (x:xs) =
    let (longer,rest) = partition (x `isPrefixOf`) xs
    in if null longer then x : prefixLast xs
                      else prefixLast (filter (/=x) longer ++ x : rest)

-- Replace OPTIONS_FRONTEND / OPTIONS_CYMAKE line containing currypp call
-- in a source text by blank line (to avoid recursive calls):
replaceOptionsLine :: String -> String
replaceOptionsLine = unlines . map replOptLine . lines
 where
  replOptLine s =
    if isOptionLine s && "currypp" `isInfixOf` s
      then "{-# DO NOT EDIT THIS FILE SINCE IT IS AUTO-GENERATED BY CURRYPP #-}"
      else s

-- Is this an OPTIONS_FRONTEND or OPTIONS_CYMAKE comment line?
isOptionLine :: String -> Bool
isOptionLine s =
 "{-# OPTIONS_CYMAKE "   `isPrefixOf` dropWhile isSpace s || 
 "{-# OPTIONS_FRONTEND " `isPrefixOf` dropWhile isSpace s     -- -}

-- Extract all OPTIONS_FRONTEND / OPTIONS_CYMAKE lines:
optionLines :: String -> String
optionLines = unlines . filter isOptionLine . lines

------------------------------------------------------------------------------
