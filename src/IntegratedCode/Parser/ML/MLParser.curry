------------------------------------------------------------------------------
--- Markup Language Parser.
---
--- @author Max Deppert
--- @version February 2015
------------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-missing-signatures -Wno-incomplete-patterns #-}

module MLParser (lex,layout,parse) where

import ParseTypes
import HTMLContentModel
import MLTypes
import MLWarning

import Char
import List

-- characters
HT = chr 009 -- '\t'
BR = chr 010 -- '\n'
BL = chr 032 -- ' '
DQ = chr 034 -- '\"'
SQ = chr 039 -- '\''
SL = chr 047 -- '/'
LT = chr 060 -- '<'
EQ = chr 061 -- '='
GT = chr 062 -- '>'
BS = chr 092 -- '\\'
OB = chr 123 -- '{'
CB = chr 125 -- '}'

-- the lexical scanner
lex :: String -> TPos -> ([Symbol],[Warning])
lex s start = tokenize (breakup s start)

-- splits input in atomic words and their positions
breakup :: String -> TPos -> [(String,TPos)]
breakup s start = reverse (map reverseStr (filter notNullStr result))
  where result = raw 0 s start "" start []
        raw :: Int -> String -> TPos -> String -> TPos -> [(String,TPos)] -> [(String,TPos)]
        raw _ "" pos w _ res = (w,pos) : res
        raw q (c:cs) pos@((a,_),_) w next@((i,j),tbs) res
         -- data state
          | q == 0 && c == LT = raw 1 cs next [c] ((i,j+1),tbs) ((w,pos):res)
          | q == 0 && c == BR = raw 2 cs ((a+1,col start),0) "" ((a+1,col start),0) ((c:w,pos):res)
         -- tag state
          | q == 1 && c == BR = raw 1 cs pos (c:w) ((i+1,col start),0) res
          | q == 1 && c == GT = raw 2 cs next "" ((i,j+1),tbs) ((c : w,pos):res)
          | q == 1 && c == SQ = raw 6 cs pos (c:w) ((i,j+1),tbs) res
          | q == 1 && c == DQ = raw 5 cs pos (c:w) ((i,j+1),tbs) res
         -- indent state
          | q == 2 && c == BL = raw 3 cs pos (c:w) ((i,j+1),tbs) res
          | q == 2 && c == HT = raw 4 cs pos (c:w) ((i,j+1),tbs+1) res
          | q == 2            = raw 0 (c:cs) next "" next res
         -- blank state
          | q == 3 && c == BL = raw 3 cs pos (c:w) ((i,j+1),tbs) res
          | q == 3            = raw 2 (c:cs) next "" next ((w,pos):res)
         -- tab state
          | q == 4 && c == HT = raw 4 cs pos (c:w) ((i,j+1),tbs+1) res
          | q == 4            = raw 2 (c:cs) next "" next ((w,pos):res)
         -- tag attribute double quote state
          | q == 5 && c == DQ = raw 1 cs pos (c:w) ((i,j+1),tbs) res
         -- tag attribute single quote state
          | q == 6 && c == SQ = raw 1 cs pos (c:w) ((i,j+1),tbs) res
         -- stay in current state
          | otherwise         = raw q cs pos (c:w) ((i,j+1),tbs) res
        reverseStr :: (String,TPos) -> (String,TPos)
        reverseStr (a,b) = (reverse a,b)
        notNullStr :: (String,TPos) -> Bool
        notNullStr = not . null . fst

-- converts the atomic words into tokens
tokenize :: [(String,TPos)] -> ([Symbol],[Warning])
tokenize ys = wmap (dataTokenizer . tk) ys
  where tk :: (String,TPos) -> (Symbol,[Warning])
        tk (s,pos)
          | isBreak s  = ((Break,pos),[])
          | isTabs s   = ((Tabs (length s),pos),[])
          | isBlanks s = ((Blanks (length s),pos),[])
          | isData s   = ((Data [Raw s],pos),[])
          | otherwise  = let (sym,w) = tagTokenizer s pos in ((sym,pos),w)
        isBreak (c:cs) = c == BR && null cs
        isTabs = all (HT==)
        isBlanks = all (BL==)
        isData (c:_) = c /= LT

-- convers a string in its representing tag
tagTokenizer :: String -> TPos -> (Token,[Warning])
tagTokenizer s pos = (release result,warnings)
  where (result,warnings) = toke 00 s "" [empty] []
        toke :: Int -> String -> String -> [(String,String)] -> [Warning] -> (Token,[Warning])
        toke q (c:cs) name attr@((a,v):attrs) ws
         -- pre state
          | q == 00 && c == LT = toke 11 cs "" attr ws
          | q == 00            = (Data [Raw s],ws)
         -- tag opening state
          | q == 11 && c == SL = toke 16 cs "" attr ws
          | q == 11            = toke 14 (c:cs) "" attr ws
         -- pre endtag name state
          | q == 16 && isAlpha c = toke 12 (c:cs) "" attr ws
          | q == 16 && isDigit c = toke 16 cs "" attr (warn pos TagNameFirstDigit ws)
          | q == 16            = (Data [Raw s],ws)
         -- endtag name state
          | q == 12 && c == GT = toke 13 (c:cs) name attr ws
          | q == 12 && c == BL = toke 13 (c:cs) name attr ws
          | q == 12 && isAlphaNum c = toke 12 cs (c:name) attr ws
          | q == 12            = toke 12 cs (c:name) attr (warn pos TagNameNotAlphaNum ws)
         -- endtag ending state
          | q == 13 && null cs = (EndTag name,ws)
          | q == 13 && c == BL = toke 13 cs name attr ws
          | q == 13            = (EndTag name,warn pos TagEndsUnexpected ws)
         -- pre tag name state
          | q == 14 && isAlpha c = toke 01 (c:cs) "" attr ws
          | q == 14 && isDigit c = toke 14 cs "" attr (warn pos TagNameFirstDigit ws)
          | q == 14            = (Data [Raw s],ws)
         -- tag name state
          | q == 01 && c == GT = toke 09 (c:cs) name attr ws
          | q == 01 && c == BL = toke 15 cs name attr ws
          | q == 01 && c == SL = toke 15 (c:cs) name attr ws
          | q == 01 && isAlphaNum c = toke 01 cs (c:name) attr ws
          | q == 01            = toke 01 cs name attr (warn pos TagNameNotAlphaNum ws)
         -- pre attribute name state
          | q == 15 && c == BL = toke 15 cs name attr ws
          | q == 15 && c == SL = toke 10 cs name attr ws
          | q == 15            = toke 02 (c:cs) name attr ws
         -- voidtag ending state
          | q == 10 && c == GT && null cs = (VoidTag name (toAttr attr),ws)
          | q == 10            = (VoidTag name (toAttr attr),warn pos TagEndsUnexpected ws)
         -- attribute name state
          | q == 02 && c == BL = toke 05 cs name attr ws
          | q == 02 && c == EQ = toke 03 cs name attr ws
          | q == 02 && c == GT = toke 09 (c:cs) name attr ws
          | q == 02            = toke 02 cs name ((c:a,v):attrs) ws
         -- post attribute name state
          | q == 05 && c == BL = toke 05 cs name attr ws
          | q == 05 && c == EQ = toke 03 cs name attr ws
          | q == 05            = toke 15 (c:cs) name (empty:attr) ws
         -- pre attribute value state
          | q == 03 && c == BL = toke 03 cs name attr ws
          | q == 03 && c == GT = toke 09 (c:cs) name attr (warn pos UnquotedAttributeEmpty ws)
          | q == 03 && c == SQ = toke 04 cs name attr ws
          | q == 03 && c == DQ = toke 06 cs name attr ws
          | q == 03            = toke 08 (c:cs) name attr ws
         -- single quoted attribute value state
          | q == 04 && c == SQ = toke 07 cs name (empty:attr) ws
          | q == 04            = toke 04 cs name ((a,c:v):attrs) ws
         -- double quoted attribute value state
          | q == 06 && c == DQ = toke 07 cs name (empty:attr) ws
          | q == 06            = toke 06 cs name ((a,c:v):attrs) ws
         -- unquoted attribute value state
          | q == 08 && c == SQ = toke 08 cs name attr (warn pos (Unquoted SQ) ws)
          | q == 08 && c == DQ = toke 08 cs name attr (warn pos (Unquoted DQ) ws)
          | q == 08 && c == EQ = toke 08 cs name attr (warn pos (Unquoted EQ) ws)
          | q == 08 && c == BL = toke 07 cs name (empty:attr) ws
          | q == 08 && c == SL = toke 07 (c:cs) name (empty:attr) ws
          | q == 08 && c == GT = toke 09 (c:cs) name attr ws
          | q == 08            = toke 08 cs name ((a,c:v):attrs) ws
         -- post attribute value state
          | q == 07 && c == BL = toke 15 cs name attr ws
          | q == 07 && c == SL = toke 15 (c:cs) name attr ws
          | q == 07 && c == GT = toke 09 (c:cs) name attr ws
          | q == 07            = toke 15 (c:cs) name attr (warn pos AttributesUnseperated ws)
         -- starttag ending state
          | q == 09 && null cs = (StartTag name (toAttr attr) 0,ws)
          | q == 09            = (StartTag name (toAttr attr) 0,warn pos TagEndsUnexpected ws)
        empty :: (String,String)
        empty = ("","")
        toAttr :: [(String,String)] -> [Attribute]
        toAttr [] = []
        toAttr (x:xs) = (fst x,[Raw (snd x)]) : toAttr xs
        release :: Token -> Token
        release (StartTag t a i) =
          StartTag (reverse t)
                   (map (\(x,[Raw y]) -> (reverse x,[Raw (reverse y)]))
                        (reverse (filter (\(x,_) -> (not . null) x) a))) i
        release (VoidTag t a) =
          VoidTag (reverse t)
                  (map (\(x,[Raw y]) -> (reverse x,[Raw (reverse y)]))
                       (reverse (filter (\(x,_) -> (not . null) x) a)))
        release (EndTag t) = EndTag (reverse t)
        release (Data p) = Data p

-- differentiation between raw data and expressions
dataTokenizer :: (Symbol,[Warning]) -> (Symbol,[Warning])
dataTokenizer (sym,ws) = case tok sym of
  Data [Raw s]   -> ((Data (dtk s [Raw ""]),pos sym), ws)
  StartTag t a i -> ((StartTag t (map (\ (x,[Raw s]) -> (x,dtk s [Raw ""])) a) i,
                     pos sym), ws)
  VoidTag t a    -> ((VoidTag t (map (\ (x,[Raw s]) -> (x,dtk s [Raw ""])) a),
                     pos sym), ws)
  _              -> (sym,ws)
 where
  dtk :: String -> [Text] -> [Text]
  dtk [] ds = reverse (map reverseText (filter (not . textNull) ds))
  dtk (c:cs) ys@((Raw  s):ds) | c == OB && (not . null) cs && head cs == OB
                               = dtk (tail cs) ((ExpC ""):ys)
                              | c == OB = dtk cs ((ExpT ""):ys)
                              | c == BS && (not . null) cs && head cs == OB
                               = dtk (tail cs) ((Raw (OB:c:s)):ds)
                              | otherwise = dtk cs ((Raw (c:s)):ds)
  dtk (c:cs) ys@((ExpT s):ds) | c == CB = dtk cs ((Raw ""):ys)
                              | c == BS && (not . null) cs && head cs == CB
                               = dtk (tail cs) ((ExpT (CB:c:s)):ds)
                              | otherwise = dtk cs ((ExpT (c:s)):ds)
  dtk (c:cs) ys@((ExpC s):ds) | c == CB && (not . null) cs && head cs == CB
                               = dtk (tail cs) ((Raw ""):ys)
                              | c == BS && (not . null) cs && head cs == CB
                               = dtk (tail cs) ((ExpC (CB:c:s)):ds)
                              | otherwise = dtk cs ((ExpC (c:s)):ds)

  reverseText :: Text -> Text
  reverseText (Raw  s) = Raw  (reverse s)
  reverseText (ExpT s) = ExpT (reverse s)
  reverseText (ExpC s) = ExpC (reverse s)

  textNull :: Text -> Bool
  textNull (Raw  s) = null s
  textNull (ExpT s) = null s
  textNull (ExpC s) = null s

-- set the right indentation for every StartTag
layout :: String -> TPos -> ([Symbol],[Warning])
layout l start = (join input,r)
  where (input,r) = lex l start
        join :: [Symbol] -> [Symbol]
        join [] = []
        join (x:xs) | isStartTag x = joiner x xs []
                    | otherwise = x : join xs
        joiner :: Symbol -> [Symbol] -> [Symbol] -> [Symbol]
        joiner t [] tmp = t : reverse tmp
        joiner t@(StartTag s a _,p) (x:xs) tmp
          | isAlign x = joiner t xs (x:tmp)
          | otherwise = (:) (StartTag s a (wcol (pos x)),p)
                            (reverse tmp) ++ join (x:xs)

-- parse a string with a start position
parse :: L -> String -> TPos -> ([Tree],[Warning])
parse lang s start = parseLayout lang input [(root,[])] r
  where (input,r) = layout s start
        root :: Symbol
        root = (StartTag "" [] 0,start)

-- minimal stack size: 1
assign :: (Symbol,[Tree]) -> ParseStack -> ParseStack
assign (sym,ts) = update (\(s,trs) -> (s,(Tree (sym2node sym) ts):trs))

-- minimal stack size: 2
reduce :: ParseStack -> ParseStack
reduce ((sym,list):st) = assign (sym,reverse list) st

parseStrict :: L -> [Symbol] -> ParseStack -> [Warning] -> ([Tree],[Warning])
parseStrict lang input stck wrns = strP 0 input stck wrns
  where strP :: Int -> [Symbol] -> ParseStack -> [Warning] -> ([Tree],[Warning])
        strP _ [] stack ws = parseLayout lang [] stack ws
        strP d (x:xs) stack@((sym,_):_) ws
          | isStartTag x = strP (d+1) xs (push (x,[]) stack) ws
          | isEndTag x   = if tgn x == tgn sym
                           then (if d == 0
                                 then parseLayout lang
                                 else strP (d-1)) xs (reduce stack) ws
                           else strP (d-1) (x:xs) (reduce stack) (warn (pos x) UnexpectedEndTag ws)
          | otherwise    = strP d xs (assign (x,[]) stack) ws

parseLayout :: L -> [Symbol] -> ParseStack -> [Warning] -> ([Tree],[Warning])
parseLayout lang [] stack@((_,list):st) ws
  | null st   = (reverse list,ws)
  | otherwise = parseLayout lang [] (reduce stack) ws
parseLayout lang (x:xs) stack@((t,_):st) ws
  | isAlign x    = parseLayout lang xs stack ws
  | outside x    = parseLayout lang (x:xs) (reduce stack) ws
  | isStartTag x = parseStartTag
  | isEndTag x   = parseEndTag
  | otherwise    = parseLayout lang xs (assign (x,[]) stack) ws
  where parseStartTag :: ([Tree],[Warning])
        parseStartTag | isStrictElement x = parseStrict lang xs (push (x,[]) stack) ws
                      | ind x <= ind t    = parseLayout lang xs (assign (x,[]) stack) ws
                      | otherwise         = parseLayout lang xs (push (x,[]) stack) ws
        parseEndTag :: ([Tree],[Warning])
        parseEndTag | tgn x == tgn t      = parseLayout lang xs (reduce stack) ws
                    | foundStartTag st    = parseLayout lang (x:xs) (reduce stack) ws
                    | otherwise           = parseLayout lang xs stack (warn (pos x) SingleEndTag ws)
        foundStartTag :: ParseStack -> Bool
        foundStartTag [] = False
        foundStartTag (y:ys) = isStartTag (fst y) && tgn (fst y) == tgn x || foundStartTag ys
        outside :: Symbol -> Bool
        outside p = wcol (pos p) < ind t
