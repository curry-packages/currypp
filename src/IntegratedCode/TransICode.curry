------------------------------------------------------------------------------
--- Translator from Curry with Integrated Code to Curry
--- ===================================================
---
--- Integrated Code can be used in Curry in the form
---
---   AccentGraves Langtag Whitespaces Code SingleQuotes
---
--- where AccentGraves is a number of ` greater than 2
---       SingleQuotes is the same number of '
---       Langtag is an arbitrary sequence of characters without
---         whitespaces, tabs and newlines,
---       Whitespaces is a combination of spaces, tabs and newlines,
---       and Code is code in the language Langtag.
--- Is is allowed to use ` and ' in the code, as long as they amount of
--- sequential ` or ' is smaller than their number in AccentGraves.
---
--- If there is a corresponding parser to the langtag, the expression can be
--- translated into type-safe Curry code.
---
--- Currently available Langtags:
--- format - see the FormatParser and Format library
--- regex  - see the RegexParser and Regex library
--- html   - see the MLParser and HTML library
--- xml    - see the MLParser and XML library
--- sql    - see the SQLConverter and CDBI-library
---
--- @author Jasper Sikorra (with changes by Michael Hanus)
--- @version January 2018
------------------------------------------------------------------------------

module TransICode where

import Directory ( getAbsolutePath, getDirectoryContents )
import FilePath  ( (</>), joinPath, splitDirectories, takeDirectory )
import IO        ( stderr, hPutStrLn )
import List
import System

import ParseTypes

import qualified CIParser

import DummyParser   as DummyParser
import FormatParser  as FormatParser
import RegexParser   as RegexParser
import MLTranslate   as MLTranslate
import SQLConverter  as SQLParser

-- Parser for Curry with Integrated Code
ciparser :: Filename -> String -> IO (PM [StandardToken])
ciparser = CIParser.parse

-- Selection of parsers for the conversion of Integrated Code expressions
-- to Curry
parsers :: Maybe Langtag -> Either String ParserInfo -> LangParser
parsers = maybe iden pars
  where
    iden _ _ s = return $ cleanPM s
    pars :: Langtag -> Either String ParserInfo -> LangParser
    pars l model p =
      case l of
        "sql"       -> case model of
                         Left err -> const (return $ throwPM p err)
                         _        -> SQLParser.parseSQL True model p
        "sql*"      -> case model of
                         Left err -> const (return $ throwPM p err)
                         _        -> SQLParser.parseSQL False model p
        "dummy"     -> DummyParser.parse p
        "format"    -> FormatParser.parse ""       p
        "printf"    -> FormatParser.parse "putStr" p
        "regex"     -> RegexParser.parse p
        "html"      -> liftIO (mapWarnsPM (addRealFname (getFilename p))) .
                               MLTranslate.translate l p
        "xml"       -> liftIO (mapWarnsPM (addRealFname (getFilename p))) .
                               MLTranslate.translate l p
        _           -> (\_ -> return $ throwPM p ("Bad langtag: " ++ l))

addRealFname :: Filename -> Warning -> Warning
addRealFname f w = setWarnPos w (setFilename (getWarnPos w) f)

-- Formatting and terminating with Errors
formatErrors :: [PError] -> IO _
formatErrors [] =
  error "Internal error in 'TransICode.formatErrors': No errors in list!"
formatErrors es@(e1:_) = do
  hPutStrLn stderr $ "\nERRORS in " ++ getFilename (getPErrorPos e1) ++ ":"
                                    ++ concatMap formatErr es
  error "Failure during preprocessing of Curry source file!"
 where
  formatErr :: PError -> String
  formatErr e = "\n" ++ "Line " ++ show (getLn (getPErrorPos e))
                     ++ " Col " ++ show (getCol (getPErrorPos e))
                     ++ ": "   ++ getPErrorMsg e

-- Formatting Warnings
formatWarnings :: [Warning] -> String
formatWarnings []              = ""
formatWarnings ws@((p,_):_) = "\nWARNINGS in " ++ getFilename p ++ ":"
                                             ++ foldr (++) "" (map formatW ws)
                                             ++ "\n\n"
  where
    formatW :: Warning -> String
    formatW w = "\n" ++ "Line " ++ show (getLn (getWarnPos w))
                     ++ " Col " ++ show (getCol (getWarnPos w))
                     ++ " | "   ++ getWarnMsg w

--- Translates a string containing a Curry program with Integrated Code
--- into a string with pure Curry code.
--- The second argument is, if non-empty, the name of an info file containing
--- information about the data model in case of integrated SQL code.
--- @param verb  - verbosity level
--- @param model - name of file containing information about the datamodel
---                in case of SQL,  an empty string otherwise
--- @param fname - The name of the original Curry file
--- @param s - The string that should be translated
--- @return The translated string
translateIntCode :: Int -> String -> String -> String -> IO String
translateIntCode verb model fname s = do
  pinfo <- tryReadParserInfoFile verb model fname
  stw <- concatAllIOPM $ applyLangParsers pinfo
                       $ ciparser fname s
  putStr (formatWarnings (getWarnings stw))
  escapePR (discardWarnings stw) formatErrors

--- Try to read parser info file for the SQL preprocessor.
tryReadParserInfoFile :: Int -> String -> String
                      -> IO (Either String ParserInfo)
tryReadParserInfoFile verb model orgfname = do
  if null model
   then do orgdir  <- getAbsolutePath (takeDirectory orgfname)
           fresult <- findParserInfoFile (splitDirectories orgdir)
           case fresult of
             Left err    -> return (Left err)
             Right fname -> readParserInfo verb (orgdir </> fname)
   else readParserInfo verb model

findParserInfoFile :: [String] -> IO (Either String String)
findParserInfoFile dirpath = do
  let dir = joinPath dirpath
  --putStrLn $ "Searching info file in: " ++ dir
  dirfiles <- getDirectoryContents dir
  case filter ("_SQLCode.info" `isSuffixOf`) dirfiles of
    []  -> let uppath = init dirpath
           in if null uppath
                then return (Left "No .info file provided or found!")
                else findParserInfoFile uppath
    [m] -> return (Right $ dir </> m)
    ms  -> return (Left $ "Multiple .info files found in directory '" ++ dir ++
                          "':\n" ++ unwords ms)

--- Handles the IO and PM monads around the StandardTokens for the
--- concatenation, so they will not disturb in the real concat function
--- concatAll
--- @param ioprpt - A list of StandardTokens wrapped in IO and a ParserMonad
concatAllIOPM :: IO (PM [StandardToken]) -> IO (PM String)
concatAllIOPM ioprpt =
  do prpt <- ioprpt
     return $ liftPM (\pt -> concatAll pt) prpt

{-
Problems with insertion of newlines:
The case that a Curry expression directly follows integrated expression,
without a newline is problematic, if the integrated expression has multiple
lines. This stems from the Curry layout rule.  The problem is depicted in the
example:
                  -- Ln. 1: isEmail s = s ``regex
                  -- Ln. 2:  a'' && True
                  -- Ln. 3:
                  -- Ln. 4:  || False
                  -- Result:
                  -- Ln. 1: isEmail s = s `match` [(Literal 'a')] && True
                  -- Ln. 2:
                  -- Ln. 3:
                  -- Ln. 4:  || False
For this line, wrong positions will be calculate in the Curry compiler, if an
error occurs. In the example: Ln 1 instead of Ln 2. All other lines have
the right positions.
-}

--- Concatenates the result of the translation process, inserting newlines
--- and offsets if necessary
--- @param tks - A list of StandardTokens containing the results
--- @result    - The resulting program code
concatAll :: [StandardToken] -> String
concatAll []      = ""
concatAll (t1:tks) = getCode t1 ++ (concatAllHelper
                                   (getIdentPos t1)
                                   (containsDSL t1)
                                   tks)
  where
    concatAllHelper :: Pos -> Bool -> [StandardToken] -> String
    concatAllHelper _ _ []        = ""
    concatAllHelper op b (t:toks) =
      let s      = getCode t
          p      = getIdentPos t
         -- if generated dsl code was processed before
      in if b
        then
          let lnDiff = lnDifference op p
          in
            -- if the first word of s was in a newline after the dsl
            if (null s)
              then genLines lnDiff ++ concatAllHelper p (containsDSL t) toks
              else
                if (head s == '\n')
                      then (genLines lnDiff ++ s
                            ++ concatAllHelper p (containsDSL t) toks)
                  -- If the first word of s was in the last line of the dsl.
                      else
                        let (headLine,restOfCurry) = splitByLine s
                        in
                            headLine ++ genLines lnDiff ++ restOfCurry
                            ++ concatAllHelper p (containsDSL t) toks
        else (s ++ concatAllHelper p (containsDSL t) toks)

--- The function genLines generates lines
--- @param n - The number of line to be generated
--- @result  - A string containing n lines
genLines :: Int -> String
genLines = flip replicate '\n'

--- The function splitByLine splits a string at the first newline
--- @param s - The string
--- @result A pair of strings, one containg the string before the newline
---         with the newline, the other containing the string after the newline
splitByLine :: String -> (String,String)
splitByLine s = splitByLineIter "" s
  where
    splitByLineIter acc "" = (reverse acc,"")
    splitByLineIter acc (c:cs) | c == '\n' = (reverse ('\n':acc),cs)
                               | otherwise = splitByLineIter (c:acc) cs

--- Applies the corresponding translators of the DSL to Curry on the
--- StandardTokens
--- @param model - data model information (required in case of SQL code),
---                otherwise an error message
--- @param iotks - The input StandardTokens wrapped in IO and ParserMonad
--- @result      - The translated StandardTokens wrapped in IO and ParserMonad
applyLangParsers :: Either String ParserInfo
                 -> IO (PM [StandardToken])
                 -> IO (PM [StandardToken])
applyLangParsers model iotks = do
  prtks <- iotks
  prpr <- swapIOPM (liftPM (mapIO (applyLangParser model)) prtks)
  return (crumplePM (liftPM (\prpt -> sequencePM prpt) prpr))

--- Select the right translator and apply it to a single StandardToken
--- @param model - data model information in case of SQL code,
---                error message otherwise
--- @param t - The input StandardToken
--- result   - The translated StandardToken wrapped in IO and ParserMonad
applyLangParser :: Either String ParserInfo
                -> StandardToken
                -> IO (PM StandardToken)
applyLangParser model (StTk p pexp l c) =
  do parsedStringNoIO <- (parsers l model) pexp c
     return (bindPM parsedStringNoIO (\s -> cleanPM (StTk p pexp l s)))
