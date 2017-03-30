--- This module scans an SQL-Statement, which is passed as a string.
---@author Julia Krone
---@version 0.1
-- -------------------------------------------------------------------

module SQLScanner(scan) where

import Char (isDigit, isAlpha, isAlphaNum, toLower)
import FiniteMap
import List (splitOn)
import Read (readInt)
import ReadShowTerm (readsQTerm)
import Time 

import SQLToken

---Scans an SQL-statement - returning it as list of Token.
---@param chs - the statement as string
---@return the statement as list of token
scan :: String -> [Token]
scan chs = 
   case chs of
      ""            -> []
      ('*':cs)      -> Asterix : scan cs
      ('(':cs)      -> LParen : scan cs
      (')':cs)      -> RParen : scan cs
      (',':cs)      -> Comma : scan cs
      (';':cs)      -> Semi : scan cs
      ('.':cs)      -> Stop : scan cs
      ('!':'=': cs) -> (BinOp Uneq) : scan cs
      ('=':cs)      -> (BinOp Equal) : scan cs
      ('{':cs)      -> readEmbedExp cs
      ('<':cs)      -> readLessOp cs
      ('>':cs)      -> readGreaterOp cs
      ('\"':cs)     -> readVarString cs
      ('\'':cs)     -> readVarChar cs
      (' ':cs)      -> scan cs
      ('\n':cs)     -> scan cs
      _             -> if (isDigit (head chs) || (head chs) == '-')
                         then (getNumberOrDate nStr) : scan nRs
                         else if (isAlpha (head chs))
                                 then (specialOrIdent s) : scan rs
                                 else (Unexpected (head chs)) : scan (tail chs)
  where (nStr, nRs) = span isDigitOrNChar chs
        (s,rs)      = span isAlphaNumUnderScore chs                                         

-- auxiliary function to check if a character is digit or one of the 
--following: '-', '.', ':'
isDigitOrNChar :: Char -> Bool 
isDigitOrNChar c = isDigit c || (c == '-') || (c == '.') || (c == ':')

--auxiliary function which checks if a given character is alphanumerical
-- or '_' (used in relation names)
isAlphaNumUnderScore :: Char -> Bool
isAlphaNumUnderScore c = isAlphaNum c || (c == '_')

-- auxiliary function to determine whether a given string is a keyword 
-- or an identifier
specialOrIdent:: String -> Token
specialOrIdent str = 
  case lookupFM keywords lstr of
          (Just tk) -> tk
          Nothing -> case lookupFM (plusFM constants operators) lstr of
                                          (Just tk) -> tk
                                          Nothing -> Ident str
  where lstr = map (toLower) str
    
-- auxiliary function to determine whether a given string is a number 
-- or a date and to sample the date
getNumberOrDate :: String -> Token  
getNumberOrDate str = if (':' `elem` str) 
                           then (Constant 
                                  (Date 
                                   (CalendarTime (readInt y) 
                                                 (readInt m) 
                                                 (readInt d) 
                                                 (readInt h) 
                                                 (readInt mi) 
                                                 (readInt s) 0)))
                           else getNumber str      
  where dateLs = splitOn [':'] str
        (y, dLs1) = getNext dateLs
        (m, dLs2) = getNext dLs1
        (d, dLs3) = getNext dLs2
        (h, dLs4) = getNext dLs3
        (mi, dLs5) = getNext dLs4
        (s, _) = getNext dLs5
        getNext [] = ("0" , [])
        getNext (num:ls) = (num, ls)

-- auxiliary function to read float and int values
getNumber :: String -> Token
getNumber str = if ('.' `elem` str) then Constant (NumFloat f)
                                    else Constant (NumInt (readInt str)) 
 where ((f,_):_) = readsQTerm str 

-- reader for embedded curry expression surrounded by {}
readEmbedExp :: String -> [Token]
readEmbedExp str = (EmbedExp embed):(scan rrest)
  where (embed,rest) = break (\c -> c == '}') str
        rrest = if (null rest) then rest else tail rest
        
-- reader for string values surrounded by ""
readVarString :: String -> [Token]
readVarString str = (Constant (VarStr var)):(scan rrest)
  where (var,rest) = break (\c -> c == '\"') str
        rrest = if (null rest) then rest else (tail rest)

-- reader for char values or string values surrounded by ' '        
readVarChar :: String -> [Token]
readVarChar str = (Constant var):(scan rrest)
  where (svar,rest) = break (\c -> c== '\'') str 
        var = if (length svar) == 1 
                 then (VarChar (head svar))
                 else if (length svar) == 0
                       then (VarChar ' ')
                       else (VarStr svar)
        rrest = if (null rest) then rest else (tail rest)       

-- reader for operations < and <=        
readLessOp :: String -> [Token] 
readLessOp [] = [(BinOp Lth)]
readLessOp (c:cs) = if ( c == '=') then (BinOp Lte) : scan cs
                                   else (BinOp Lth) : scan (c:cs)
 
-- reader for operations > and >=  
readGreaterOp :: String -> [Token]
readGreaterOp [] = [(BinOp Gth)]
readGreaterOp (c:cs) = if (c == '=') then (BinOp Gte) : scan cs
                                     else (BinOp Gth) : scan (c:cs)

-- constructor of a finite map containing all keywords    
keywords:: FM String Token
keywords = addListToFM (emptyFM (>)) [("select", KW_Select),
                                      ("from", KW_From),
                                      ("where", KW_Where),
                                      ("order", KW_Order),
                                      ("group", KW_Group),
                                      ("by", KW_By),
                                      ("having", KW_Having),
                                      ("insert", KW_Insert),
                                      ("into", KW_Into),
                                      ("values", KW_Values),
                                      ("update", KW_Update),
                                      ("set", KW_Set),
                                      ("delete", KW_Delete),
                                      ("transaction", KW_Transaction),
                                      ("commit", KW_Commit),
                                      ("rollback", KW_Rollback),
                                      ("begin",KW_Begin),
                                      ("inner", KW_Inner),
                                      ("cross", KW_Cross),
                                      ("join", KW_Join),
                                      ("on", KW_On),
                                      ("as", KW_As),
                                      ("satisfies", KW_Satisfies),
                                      ("distinct", KW_Distinct),
                                      ("all", KW_All),   
                                      ("case", KW_Case),
                                      ("when", KW_When),
                                      ("then", KW_Then),
                                      ("else", KW_Else),
                                      ("end", KW_End),
                                      ("asc", KW_Asc),
                                      ("desc", KW_Desc),
                                      ("limit", KW_Limit),
                                      ("exists", KW_Exists),
                                      ("not", KW_Not),
                                      ("table", KW_Table)]

-- constructor of a finite map containing all constants      
constants :: FM String Token
constants = addListToFM (emptyFM (>)) [("true", Constant (Boolean True)),
                                       ("false", Constant (Boolean False)),
                                       ("null",Constant Null) ]

-- constructor of a finite map containing all (literal) operators and functions
operators :: FM String Token       
operators = addListToFM (emptyFM (>)) [("like", BinOp Like),
                                       ("between", Between),
                                       ( "and", LogOp And ),
                                       ( "or", LogOp Or),
                                       ( "in", In),
                                       ( "is", Is),
                                       ("count", Fun Count),
                                       ("avg", Fun Avg),
                                       ("min", Fun Min),
                                       ("max", Fun Max),
                                       ("sum", Fun Sum),
                                       ("union", SetOp Union),
                                       ("intersect", SetOp Intersect),
                                       ("except", SetOp Except)  ] 