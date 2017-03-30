--- This module implements the Interface for the CurryPP Translator.
--- To provide the tranformation of SQL-statements it calls in this order:
--- Scanner
--- Parser
--- Consistency check 
--- Type check
--- Translator (to functions of CDBI interface)
--- Reads the .info file which contains information about the data model and
--- passes the information to the corresponding subroutines.
--- Aborts compilation process in case a stage returns with an error.
---@author Julia Krone
---@version 0.1
-- ---------------------------------------------------------------------------

module SQLConverter(parse, readParserInfo, ParserInfo) where


import IO(openFile, IOMode(..), hGetContents)
import ReadShowTerm(readsQTerm)

import ParseTypes

import SQLConsistency
import SQLNamer
import SQLParser
import SQLParserInfoType
import SQLScanner
import SQLTranslator
import SQLTyper


--- Converts a string representing SQL-requests into functions defined
--- in the CDBI interface by calling the different stage of transformation.
---@parserInfo - either the parser information or an error message
---@param pos - Position of the integrated SQL-String in the orginal file
---@param code - the SQL-request as string
---@return A String in Curry-Syntax (CDBI-functions).
parse :: Either String ParserInfo -> LangParser
parse parserInfo pos code = 
   case parserInfo of
         Left err -> return (throwPM pos err)
         Right pi -> processCompilation pi pos code 
  
--- Reader for parser information file.
--- @param verb - verbosity level
--- @param filename - path/name of the .info file
--- @return either an error message or the parser information 
readParserInfo :: Int -> String -> IO (Either String ParserInfo)
readParserInfo verb filename = do
  when (verb > 0) $ putStrLn $ "Read SQL model info file '" ++filename++ "'..."
  handle <- openFile filename ReadMode
  contents <- (hGetContents handle)
  case (readsQTerm contents) of
           []        -> return (Left ("ParserInfo-file was not found"++
                                       " or is corrupted."))
           ((a,_):_) -> return (Right a)
 
-- auxiliary function to check Result after each stage 
checkResult :: PM a -> Either (PM String) (PM a)
checkResult (WM (Errors err) ws) = Left (WM (throwPR err) ws)
checkResult pm@(WM (OK _ ) _)    = Right pm

-- Calls the stages of parsing process in the right order
-- and passes corresponding parts of the parser information.
-- Aborts process in case a stage returns with an error.
processCompilation :: ParserInfo -> LangParser
processCompilation parserInfo pos code =
  let parsedCode = parseTkLs pos (scan code)
  in case checkResult parsedCode of
      Left pm -> return pm
      Right _ -> callNamer parsedCode
   where callNamer res =
          let namedCode = nameStatements res pos
           in case checkResult namedCode of
                     Left pm -> return pm
                     Right _ -> callConsistency namedCode
         callConsistency res =
          let consCheckedCode = checkConsistency res
                                                 parserInfo
                                                 pos
           in case checkResult consCheckedCode of
                     Left pm -> return pm
                     Right _ -> callTyper consCheckedCode
         callTyper res =
          let typeCheckedCode = checkTypes res 
                                           (getTypes parserInfo)
                                            pos
           in case checkResult typeCheckedCode of
                     Left pm -> return pm
                     Right _ -> return (translate typeCheckedCode 
                                                  (dbName parserInfo)
                                                  (cdbiModule parserInfo)
                                                  pos)

         