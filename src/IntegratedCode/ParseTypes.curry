------------------------------------------------------------------------------
--- A interface for parsers
---
--- @author Jasper Sikorra - jsi@informatik.uni-kiel.de
--- @version March 2014
------------------------------------------------------------------------------
module ParseTypes (
  module ParseMonad,
  module ParsePos,
  module ParseError,
  module ParseTypes,
  module ParseWarning
  ) where

import ParseMonad
import ParsePos
import ParseError
import ParseWarning

--- Type of a Translator for a DSL
type LangParser = Pos -> String -> IO (PM String)
type Langtag = String

--- The StandardToken is used by the CIParser and Translator to store
--- informations on Integrated Code
data StandardToken = StTk Pos Pos (Maybe Langtag) Code
type Code = String
type Offset = Int

getIdentPos :: StandardToken -> Pos
getIdentPos (StTk p _ _ _) = p

getCodePos :: StandardToken -> Pos
getCodePos (StTk _ p _ _) = p

getOffset :: StandardToken -> Offset
getOffset = getCol . getCodePos

getLangtag :: StandardToken -> Maybe Langtag
getLangtag (StTk _ _ l _) = l

getCode :: StandardToken -> Code
getCode (StTk _ _ _ c) = c

--- Check wether a StandardToken contains a DSL or normal Curry
containsDSL :: StandardToken -> Bool
containsDSL (StTk _ _ (Just _) _) = True
containsDSL (StTk _ _ Nothing _) = False
