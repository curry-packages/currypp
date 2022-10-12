--------------------------------------------------------------------------------
--- A simple dummy parser that replaces '\n' with ' '.
--- This parser was written for testing purposes.
---
--- @author Jasper Sikorra
--- @version January 2014
--------------------------------------------------------------------------------

module CPP.ICode.Parser.DummyParser ( parse ) where

import CPP.ICode.ParseTypes

parse :: LangParser
parse _ s = return $ cleanPM (map (\c -> if (c == '\n') then ' ' else c) s)
