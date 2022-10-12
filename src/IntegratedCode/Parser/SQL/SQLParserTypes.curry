--- Defines datatypes and corresponding
--- constructor functions, combinators and
--- selectors used by the SQLParser
---
--- @author Julia Krone
--- @version 0.1
-- ------------------------------------------

module SQLParserTypes where

import ParseTypes

import SQLToken

infix 2 .~>.
infix 2 .<~.


--- Datatype for organization of the parsing process used in the monadic
--- structure SPMParser (therefore its name).
--- Note that the SPM itself is not used as a monad here although
--- corresponding functions (satisfying monadic laws) could easily be defined.
--- Consists of the position of integrated Code, a PM which contains
--- the result/errors and warnings that were calculated before and
--- the list of SQLToken which to parse.
--- It is parameterized over a, which represents the result type.
data SPM a = SPM Pos (PM a) [Token]

--- Datatype for an Empty SQLParserMonad.
--- Same as SPM but without a result.
data EmptySPM = ESPM Pos [Token]

--- Monadic structure which is the basic type for parsing process.
--- Takes an EmptySPM and passes it down, parsing the Token
--- and generating the result which is finally passed back.
--- Returns the SPM with the result which is constructed
--- bottom-up.
type SPMParser a = EmptySPM -> SPM a
--- constructor function for SPM
--- @param pos - position of the integrated Code
--- @param ele - a value of type a to initialize the PM
--- @param tks - list of Token which to parse
--- @return an initialized SQLParserMonad with initialized PM
newSPM :: Pos -> a -> [Token] -> SPM a
newSPM pos ele tks = SPM pos (cleanPM ele) tks

--- constructor function for an EmptySPM
--- @param pos - position of the integrated code
--- @param tks - list of Token which to parse
--- @return an EmptySPM
newEmptySPM :: Pos -> [Token] -> EmptySPM
newEmptySPM pos tks = ESPM pos tks

--- initializes a SPM - return function of SPMParser
---@ param ele - a value of type a to initialize the PM
---@ param espm - the EmptySPM to insert the element
---@return the initialized SPM with initialized PM. Token and
---        Symboltable are passed from the EmptySPM
initializeSPM :: a -> EmptySPM -> SPM a
initializeSPM ele (ESPM pos tks) = SPM pos (cleanPM ele) tks

-- Concats two SQLParserMonads by given function.
-- If at least one of the PMs contains an error it will be thrown.
-- Otherwise the Warnings of the PMs will be concatenated and the results
-- will be combined by f.
-- The Token of the least given Monad are used to continue.
concatSPMs :: (a -> b -> c) -> SPM a -> SPM b -> SPM c
concatSPMs f (SPM pos pm1 _) (SPM _ pm2 tks2) =
                (SPM pos (combinePMs f pm1 pm2) tks2)

--- Returns whether the List of Token contained by the given EmptySPM
--- is not empty
--- @param espm - the EmptySPM
--- @return False if list is empty, true otherwise
hasToken :: EmptySPM -> Bool
hasToken (ESPM _ tk) | tk == [] = False
                     | otherwise = True

--- Sets the List of Token.
--- @param tks - the list of Token
--- @param espm - the EmptySPM
--- @return the altered EmptySPM
setToken :: [Token] -> EmptySPM -> EmptySPM
setToken tks (ESPM pos _)  = ESPM pos tks

--- Returns the list of Token.
--- @param espm - the EmptySPM
token :: EmptySPM -> [Token]
token (ESPM _ tk) = tk

--- Partially defined! Returns first token of the non-empty List of Token.
headToken :: EmptySPM -> Token
headToken (ESPM _ (t:_)) = t

--- Cuts the first Token of the List without doing anything else.
--- Does Nothing if List of Token is empty.
continue :: EmptySPM -> EmptySPM
continue espm@(ESPM _ []) = espm
continue (ESPM pos (_:tks)) = ESPM pos tks

--- Lift: Applies a given function to the result of the
--- given parser.
---@param f - function to apply
---@param parser - parser function generating SPM of type a
---@param espm - EmptySPM which parser function is applied to
---@return SPM genrated by parser function and altered by f
liftSPM :: (a -> b) -> SPMParser a -> SPMParser b
liftSPM f parser espm =
  let (SPM pos pm tks) = parser espm
  in (SPM pos (liftPM f pm) tks)

--- Bind-function for SPMParser.
--- The additional list of Token is for error recovery, normally the follow set
--- of the piece of code that is parsed by the first parser.
--- In case the first parser fails the second one is never invoked, the list of
--- remaining tokes is cut until the first token that is member of the follow set.
bindSPM :: SPMParser a -> (a -> SPMParser b) -> [Token] -> SPMParser b
bindSPM parserA f toks espm =
  case parserA espm of
     SPM pos (PM (WM (Errors err) ws)) tks  ->
                 let rTks = (dropWhile (\t ->  not (t `elem` toks || t== Semi))
                                       tks)
                  in  SPM pos (PM $ returnWM (Errors err) ws) rTks
     SPM pos (PM (WM (OK res) _)) tks       -> f res (ESPM pos tks)

--- Bind-function for SPMParser with superior error menagement.
--- In case the first parser fails, the
--- second one is invoked with a default value and the tokens set to the
--- next one that is element of the given list (typically the follow-set).
--- @param parserA - first parser with result type a
--- @param defEle - default value of type a that is used if first parser fails
--- @param f - second parser binding the result of the first one
--- @param toks - list of token to follow with if first parser fails
--- @return SPM - result of second parser
bindDefSPM :: SPMParser a -> a -> (a -> SPMParser b) -> [Token] -> SPMParser b
bindDefSPM parserA defEle f toks espm =
 let resA = parserA espm
   in case resA of
     SPM pos (PM (WM (Errors err) ws)) tks ->
              let rTks = dropWhile (\t -> not (t `elem` toks || t== Semi)) tks
                  (SPM  _ pm tks2) = f defEle (ESPM pos rTks)
               in (SPM pos (combinePMs proj2 --(\ _ y -> y)
                                       (PM (WM (Errors err) ws))
                                       pm)
                           tks2)
     SPM pos (PM (WM (OK res) _)) tks      -> f res (ESPM pos tks)
 where
  -- This explicitly typed auxiliary function is necesary to avoid
  -- a typing error in GHC/KiCS2:
  proj2 :: b -> b -> b
  proj2 _ y = y

--- Concats a terminal-Parser to a SPMParser.
--- Invokes the second one just if the first one did not fail.
(.~>.) :: (EmptySPM -> Either EmptySPM (SPM a)) -> SPMParser a -> SPMParser a
(.~>.) eparser parser espm =
         case (eparser espm) of
             Left espm1 -> parser espm1
             Right spm  -> spm


--- Concats a SPMParser to a following terminal-Parser.
--- If the terminal-Parser fails, the errors are concatenated to the former
--- otherwise the result of the SPMParser is returned.
(.<~.) :: SPMParser a -> (EmptySPM -> Either EmptySPM (SPM a)) -> SPMParser a
(.<~.) parser eparser espm =
  let spm@(SPM pos pm tks) = parser espm
      res = eparser (ESPM pos tks)
  in case res of
           Left (ESPM _ rtks) -> (SPM pos pm rtks)
           Right spm1 -> case pm of
                           (PM (WM (Errors _) _)) -> concatSPMs (\_ b -> b) spm spm1
                           (PM (WM (OK _) _))     -> spm1


--- Combines two SPMParsers in an alternate manner:
--- Both parsers are applied independently , the second one taking the
--- list of token altered by the first one.
--- The resulting PMs are combined afterwards.
--- @param f - function to combine results
--- @return The resulting SPM
combineSPMs :: (a -> b -> c) -> SPMParser a -> SPMParser b -> SPMParser c
combineSPMs f spma spmb espm =
  let (SPM p pm1 tks1) = spma espm
      (SPM _ pm2 tks2) = spmb (ESPM p tks1)
  in (SPM p (combinePMs f pm1 pm2) tks2)


--- Drop token until given token or Semi is reached.
proceedWith :: Token -> EmptySPM -> EmptySPM
proceedWith tok (ESPM p tks) =
  (ESPM p (dropWhile (\t -> t /= tok && t /= Semi) tks))

--- Drop Token until one of the token in the list or Semi is reached.
--- Tokens are tried in given order.
--- As soon as one token is found, the remaining ones are not tried anymore.
proceedWithOneOf :: [Token] -> EmptySPM -> EmptySPM
proceedWithOneOf toks (ESPM p tks) =
  ESPM p (dropWhile (\t ->  not (t `elem` toks || t== Semi)) tks)

--- Drop Token including the given one.
proceedAfter :: Token -> EmptySPM -> EmptySPM
proceedAfter tok (ESPM p tks) =
  (ESPM p (tail(dropWhile (\t -> t /= tok) tks)))

--- Parses a terminal.
--- @return EmptySPM with corresponding token consumed if there was no error.
---         A SPM containing the error message otherwise.
terminal :: Token -> EmptySPM -> Either EmptySPM (SPM _)
terminal tk  espm
  | hasToken espm =
       if tk == headToken espm
          then Left $ continue espm
          else Right $ parseError
                             ("Expected " ++ tokentoString tk ++
                               " but got "++ tokentoString (headToken espm))
                                  espm
  |otherwise = Right $ emptyTkErr espm

--- Alternate terminal-parser: Additionally takes token with which to proceed
--- in case of an error.
terminalOrProc :: Token -> [Token] -> EmptySPM -> Either EmptySPM (SPM _)
terminalOrProc tk rtoks espm
  | hasToken espm =
       if tk == headToken espm
          then Left $ continue espm
          else Right $ parseError
                         ("Expected " ++ tokentoString tk ++
                          " but got "++ tokentoString (headToken espm))
                         (proceedWithOneOf rtoks espm)
  |otherwise = Right $ emptyTkErr espm

--- alternative terminal parser which in case of error consumes all
--- token including the given one
terminalOrConsume :: Token -> EmptySPM -> Either EmptySPM (SPM _)
terminalOrConsume tk  espm
  | hasToken espm =
      if tk == headToken espm
         then Left $ continue espm
         else Right $ parseError
                         ("Expected " ++ tokentoString tk ++
                          " but got "++ tokentoString (headToken espm))
                           (proceedAfter tk espm)

--- Returns Error with given message.
--- @param errMsg - the error message
--- @param espm - the EmptySPM
parseError :: String -> SPMParser _
parseError errMsg (ESPM pos tks) = SPM pos (throwPM pos errMsg) tks

--- Returns Standarderror in case the TokenList is empty.
--- Inserts a single semicolon as Tokenlist to avoid subsequent errors.
emptyTkErr :: SPMParser _
emptyTkErr (ESPM pos _) = SPM pos
                              (throwPM pos "Statement ended unexpectedly")
                              [Semi]
