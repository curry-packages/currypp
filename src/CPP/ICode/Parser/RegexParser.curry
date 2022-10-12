------------------------------------------------------------------------------
--- A Regex Parser
--- It is based on the bachelor thesis "Foreign Code Integration in Curry" of
--- Jasper Sikorra (March 2014).
---
--- @author Corinna Wambsganz
--- @version September 2022
------------------------------------------------------------------------------

{-# OPTIONS_FRONTEND -Wno-missing-signatures -Wno-incomplete-patterns #-}

module CPP.ICode.Parser.RegexParser(parse) where

import Parser    -- from package fl-parser
import Data.Char
import Data.List
import Numeric

import CPP.ICode.ParseTypes

--- The parse function allows the translation of extended regular expression
--- to normal Curry code.
--- @param po - The position of the ERE code
--- @param st - The ERE code
--- @return A string containg normal curry code with the same semantics as the
---         original ERE code
parse :: LangParser
parse po st = let (par, _) = parsen po (lex st) [0] 1
              in return (liftPM (\p -> "( captureG 0 (" ++ showRegex p ++ "))")
                par)


--- The function showRegex is used to generate a string containing the
--- target code
showRegex :: Regex -> String
showRegex r = case r of
   Nil l           -> "eps " ++ show l
   Literal l s     -> "literal " ++ show l ++ " " ++  s
   Xor l x1 x2     -> "alt " ++ show l ++ " (" ++ showRegex x1 ++ ") (" ++
     showRegex x2 ++ ")"
   Seq l x1 x2     -> case x1 of
     Start ls xs -> case x2 of
       End le xe -> "conc " ++ show l ++ " (start " ++ show ls ++ " (" ++
         showRegex xs ++ ") True) (end " ++ show le ++ " (" ++ showRegex xe ++
           ") True)"
       _         -> "conc " ++ show l ++ " (start " ++ show ls ++ " (" ++
         showRegex xs ++ ") True) (" ++ showRegex x2 ++ ")"
     _           -> case x2 of
       End le2 xe2 -> "conc " ++ show l ++ " (" ++ showRegex x1 ++ ") (end " ++
         show le2 ++ " (" ++ showRegex xe2 ++ ") True)"
       _           -> "conc " ++ show l ++ " (" ++ showRegex x1 ++ ") (" ++
         showRegex x2 ++ ")"
   Star l x        -> "rep " ++ show l ++ " (" ++ showRegex x ++ ")"
   Plus l x        -> "pl " ++ show l ++ " (" ++ showRegex x ++ ")"
   AnyLiteral l    -> "anyL " ++ show l
   Bracket l x     -> "bracket " ++ show l ++ " (" ++ showEither x ++ ")"
   NegBracket l x  -> "negBracket " ++ show l ++ " (" ++ showEither x ++ ")"
   Start l x       -> "start " ++ show l ++ " (" ++ showRegex x ++ ")"++ "False"
   End l x         -> "end " ++ show l ++ " (" ++ showRegex x ++ ")" ++ "False"
   Times l (i,j) x -> "times " ++ show l ++ " " ++ show i ++ " " ++ show j ++
     " ("  ++ showRegex x ++ ")"
   Capture n x   -> "captureG " ++ show n ++ "(" ++ showRegex x ++ ")"
  where
   showEither :: [Either String (String,String)] -> String
   showEither []     = ""
   showEither (x:xs) = showE "" (x:xs)
       where
         showE acc []     = "[" ++ (init acc) ++ "]"
         showE acc (y:ys) = case y of
           (Left s)        -> showE (acc ++ "Left (" ++ s ++ "),") ys
           (Right (s1,s2)) -> showE (acc ++ "Right " ++ "((" ++ s1 ++ "),("
                                        ++ s2 ++ "))" ++ ",") ys

--- An intermediate data type which can be used for code generation
data Regex = Nil [Int]
           | Literal [Int] String
           | Xor [Int] Regex Regex
           | Seq [Int] Regex Regex
           | Star [Int] Regex
           | Plus [Int] Regex
           | AnyLiteral [Int]
           | Bracket [Int] [Either String (String,String)]
           | NegBracket [Int] [Either String (String,String)]
           | Start [Int] Regex
           | End [Int] Regex
           | Times [Int] (Int,Int) Regex
           | Capture Int Regex
 deriving Show

--- Possible regex operators
operators       = ['|','*','.','[',']','^','$','{','}','(',')','?','+']
--- Characters which can be used to escape other characters
escapers        = ['\\']
--- Characters which aren't operators but still escapable
non_op_escp     = ['<','>','\n','\t','\\','-']
--- All characters which are escapable
escapable       = flip elem (non_op_escp ++ operators)
--- POSIX classes describing ranges
posixclasses    = [":alnum:",":alpha:",":blank:",":cntrl:",":digit:",
                   ":graph:",":lower:",":print:",":punct:",":space:",
                   ":upper:",":xdigit:"]

--- Converting POSIX classes to square bracket expressions
posixclassconv :: String -> [Either String (String,String)]
posixclassconv =  map eccToEss . posixclasscon
  where
    eccToEss :: Either Char (Char,Char) -> Either String (String,String)
    eccToEss (Left c) = Left (show c)
    eccToEss (Right (c1,c2)) = Right (show c1,show c2)

posixclasscon :: String -> [Either Char (Char,Char)]
posixclasscon str = case str of
  ":alnum:"   ->  [Right ('A','Z'), Right ('a','z'), Right ('0','9')]
  ":alpha:"   ->  [Right ('A','Z'), Right ('a','z')]
  ":blank:"   ->  [Left ' ', Left '\t']
  ":cntrl:"   ->  [Right ('\x00','\x1F'), Left '\x7F']
  ":digit:"   ->  [Right ('0','9')]
  ":graph:"   ->  [Right ('\x21','\x7E')]
  ":lower:"   ->  [Right ('a','z')]
  ":print:"   ->  [Right ('\x20','\x7E')]
  ":punct:"   ->  [Left ']', Left '[', Left '!', Left '"',  Left '#',
                   Left '$', Left '%', Left '&', Left '\'', Left '(',
                   Left ')', Left '*', Left '+', Left ',' , Left '.',
                   Left '/', Left ':', Left ';', Left '<' , Left '=',
                   Left '>', Left '?', Left '@', Left '\\', Left '^',
                   Left '_', Left '`', Left '{', Left '|',  Left '}',
                   Left '~', Left '-']
  ":space:"   ->  [Left ' ', Left '\t', Left '\r', Left '\n',
                             Left '\v', Left '\f']
  ":upper:"   ->  [Right ('A','Z')]
  ":xdigit:"  ->  [Right ('A','F'), Right ('a','f'), Right ('0','9')]

--- Tokens for the lexer
data Token = TokenStar
           | TokenBar
           | TokenPoint
           | TokenDash
           | TokenDollar
           | TokenPlus
           | TokenOSBracket
           | TokenCSBracket
           | TokenORBracket
           | TokenCRBracket
           | TokenOCBracket
           | TokenCCBracket
           | TokenOABracket
           | TokenCABracket
           | TokenOCap
           | TokenCCap
           | TokenLiteral Char
 deriving Eq

--- Assigning Tokens to Strings
tokenToString :: Token -> String
tokenToString t = case t of
  TokenStar       -> "*"
  TokenBar        -> "|"
  TokenPoint      -> "."
  TokenDash       -> "^"
  TokenDollar     -> "$"
  TokenPlus       -> "+"
  TokenOSBracket  -> "["
  TokenCSBracket  -> "]"
  TokenORBracket  -> "("
  TokenCRBracket  -> ")"
  TokenOCBracket  -> "{"
  TokenCCBracket  -> "}"
  TokenOABracket  -> "<"
  TokenCABracket  -> ">"
  TokenOCap       -> "/("
  TokenCCap       -> ")/"
  TokenLiteral c  -> [c]

--- Lexer
lex :: String -> [Token]
lex ""  = []
lex str@(c:cs) = case str of
  ('*':_)      -> (TokenStar         :lex cs)
  ('|':_)      -> (TokenBar          :lex cs)
  ('.':_)      -> (TokenPoint        :lex cs)
  ('^':_)      -> (TokenDash         :lex cs)
  ('$':_)      -> (TokenDollar       :lex cs)
  ('?':_)      -> lex ('{':'0':',':'1':'}':cs)
  ('+':_)      -> (TokenPlus         :lex cs)
  ('[':_)      -> (TokenOSBracket    :lex cs)
  (']':_)      -> (TokenCSBracket    :lex cs)
  ('(':_)      -> (TokenORBracket    :lex cs)
  (')':_)      -> if (cs == [])
    then (TokenCRBracket:lex cs)
    else if (head cs == '/')
      then (TokenCCap:lex (tail cs))
      else (TokenCRBracket:lex cs)
  ('{':_)      -> (TokenOCBracket    :lex cs)
  ('}':_)      -> (TokenCCBracket    :lex cs)
  ('<':_)      -> (TokenOABracket    :lex cs)
  ('>':_)      -> (TokenCABracket    :lex cs)
  ('-':_)      -> (TokenLiteral '-'  :lex cs)
  ('\\':d:ds)  -> if (escapable d) then
                      case d of
                        'n'       -> TokenLiteral '\n':lex ds
                        't'       -> TokenLiteral '\t':lex ds
                        _         -> TokenLiteral d   :lex ds
                    else (TokenLiteral '\\':lex cs)
  ('\n':_)     -> lex cs
  ('/':('(':r))-> (TokenOCap : lex r)
  _            -> (TokenLiteral c    :lex cs)

--- Parser
parsen :: Pos -> [Token] -> [Int] -> Int -> (PM Regex, Int)
parsen p tks l n = pars p (cleanPM (Nil [0])) tks l n

pars :: Pos -> PM Regex -> [Token] -> [Int] -> Int -> (PM Regex, Int)
pars _ prr []     _ n = (prr, n)
pars p prr (t:ts) l n = case t of
      TokenStar        -> let (par, n1) = parsen p ts l n
                          in (bindPM prr (\r -> liftPM ((\r1 r2 -> Seq l r1 r2)
                            (Star l r)) par), n1)
      TokenBar         -> (bindPM prr (\r -> fst $ parseBar p r ts l n), n)
      TokenPoint       -> let (par,n1) = (pars p (cleanPM (AnyLiteral l))ts l n)
                          in (bindPM prr (\r -> liftPM ((\r1 r2 -> Seq l r1 r2)
                            r) par), n1)
      TokenDash        -> let (par, n1) = parseDash p ts l n
                          in (bindPM prr (\r -> liftPM ((\r1 r2 -> Seq l r1 r2)
                            r) par), n1)
      TokenDollar      -> let (par, n1) = parsen p ts l n
                          in (bindPM prr (\r -> liftPM ((\r1 r2 -> Seq l r1 r2)
                            (End l r)) par), n1)
      TokenPlus        -> let (par, n1) = parsen p ts l n
                          in (bindPM prr (\r -> liftPM ((\r1 r2 -> Seq l r1 r2)
                            (Plus l r)) par), n1)
      TokenOSBracket   -> let (par, n1) = parseOSBracket p ts l n
                          in (bindPM prr (\r -> liftPM ((\r1 r2 -> Seq l r1 r2)
                            r) par), n1)
      TokenCSBracket   -> (throwPM p "No \'[\' for \']\' found", n)
      TokenORBracket   -> let (par, n1) = parseRBracket p ts l n
                          in (bindPM prr (\r -> liftPM ((\r1 r2 -> Seq l r1 r2)
                            r) par), n1)
      TokenCRBracket   -> (throwPM p "No \'(\' for \')\' found", n)
      TokenOCBracket   -> (bindPM prr (\r -> fst $ parseCBracket p r ts l n), n)
      TokenCCBracket   -> (throwPM p "No \'{\' for \'}\' found", n)
      TokenOABracket   -> let prsrs     = parseABracket p ts
                              prs       = fstPM prsrs
                              prrs      = sndPM prsrs
                           in (bindPM prr (\r -> bindPM prs (\s -> bindPM prrs
                             (\rs -> liftPM ((\r1 r2 -> Seq l r1 r2) r) $ fst $
                               pars p (cleanPM (Literal l s)) rs l n))), n)
      TokenCABracket   -> (throwPM p "No \'<\' for \'>\' found", n)
      TokenOCap        -> let (par, n1) = parseCap p ts l n
                          in (bindPM prr (\r -> liftPM ((\r1 r2 -> Seq l r1 r2)
                            r) par), n1)
      TokenCCap        -> (throwPM p "No \'/(\' for \')/\' found", n)
      TokenLiteral c   -> let (par, n1) = pars p (cleanPM (Literal l (show c))) ts l n
                          in (bindPM prr (\r -> liftPM ((\r1 r2 -> Seq l r1 r2)
                            r) par), n1)

--- Alternative
parseBar :: Pos -> Regex -> [Token] -> [Int] -> Int -> (PM Regex, Int)
parseBar p r ts l n = let (par, n1) = parsen p ts l n
                      in (liftPM (\x -> Xor l r x) par, n1)

--- Start
parseDash :: Pos -> [Token] -> [Int] -> Int -> (PM Regex, Int)
parseDash p ts l n = let (par, n1) = parsen p ts l n
                     in (liftPM (\r -> case r of
                       Seq l1 r1 r2 -> Seq l1 r1 (Start l r2)
                       _            -> Start l r) par, n1)

--- Square Bracket (Range)
parseOSBracket :: Pos -> [Token] -> [Int] -> Int -> (PM Regex, Int)
parseOSBracket p []     _ n = (throwPM p "Missing ']'", n)
parseOSBracket p (t:ts) l n = case t of
  TokenDash -> let (cont,rst) = case ts of
                    -- Handle closing square bracket directly after opening,
                    -- meaning the ']' char is an option
                    (TokenCSBracket:tss) ->
                      ((\(x,y) -> ((TokenCSBracket:x),y))
                       (span (\x -> x /= TokenCSBracket) tss))
                    _            -> span (\x -> x /= TokenCSBracket) ts
               in pars p (liftPM (\x -> NegBracket l x) (squareParser p cont))
                         (tail rst) l n
  _         -> let (cont,rst) = case (t:ts) of
                    -- Handle closing square bracket directly after opening,
                    -- meaning the ']' char is an option
                    (TokenCSBracket:tss) ->
                      (\(x,y) -> ((TokenCSBracket:x),y))
                        (span (\x -> x /= TokenCSBracket) tss)
                    _           -> span (\x -> x /= TokenCSBracket)
                                                (t:ts)
               in pars p (liftPM (\x -> Bracket l x) (squareParser p cont))
                         (tail rst) l n

squareParser :: Pos -> [Token] -> PM [Either String (String,String)]
squareParser pos toks =
      let chars  = extractChars toks
      in if (elem chars posixclasses)
          then cleanPM (posixclassconv chars)
          else rangeAndCharParser pos toks
  where
    rangeAndCharParser p tks = case tks of
      []
        -> cleanPM []
      (TokenLiteral a:TokenLiteral '-':TokenLiteral b:ts)
        -> liftPM (\rc -> ((:) (Right (show a,show b))) rc)
                  (rangeAndCharParser p ts)
      (TokenOABracket:ts)
        -> let prsrs = parseABracket p ts
               prs   = fstPM prsrs
               prrs  = sndPM prsrs
            in (bindPM prs $  \s  ->
                bindPM prrs $ \rs ->
              if (head rs == TokenLiteral '-')
                then
                  if (head (tail rs) == TokenOABracket)
                    then
                      let prs2rs2 = parseABracket p (tail (tail rs))
                          prs2    = fstPM prs2rs2
                          prrs2   = sndPM prs2rs2
                      in (bindPM prs2 $  \s2  ->
                          bindPM prrs2 $ \rs2 ->
                          liftPM ((:) (Right (s,s2))) $
                            rangeAndCharParser p rs2)
                    else liftPM ((:)
                            (Right (s,(\(TokenLiteral c) -> (show c))
                                          (head (tail rs)))))
                            (rangeAndCharParser p (tail (tail rs)))
                else liftPM ((:) (Left s)) (rangeAndCharParser p rs))
      (TokenOCap:ts)
        -> rangeAndCharParser p (TokenLiteral '/' : (TokenLiteral '(' : ts))
      (TokenCCap:ts)
        -> rangeAndCharParser p (TokenLiteral ')' : (TokenLiteral '/' : ts))
      _
        -> liftPM ((:) (Left (show $ head $ tokenToString $ head tks)))
                  (rangeAndCharParser p (tail tks))

--- Round bracket (Paranthesis)
parseRBracket :: Pos -> [Token] -> [Int] -> Int -> (PM Regex, Int)
parseRBracket p ts l n = let (par, n1) = parsen p (init cont) l n
                         in pars p par rst l n1
  where
    (cont,rst) = splitAt (cntUntilClosed TokenORBracket TokenCRBracket ts) ts

--- Curly bracket (Multiple times)
parseCBracket :: Pos -> Regex -> [Token] -> [Int] -> Int -> (PM Regex, Int)
parseCBracket p r ts l n = pars p (cleanPM(Times l (curlyParser(init cont)) r))
                              rst l n
  where
    (cont,rst) = splitAt (cntUntilClosed TokenOCBracket TokenCCBracket ts) ts

curlyParser :: [Token] -> (Int,Int)
curlyParser tks = (fst fir,fst sec)
  where
    fir = case readNat (extractChars tks) of
      [v] -> v
      _   -> failed
    sec = case readNat (tail (snd fir)) of
      [v] -> v
      _   -> failed

--- Slash and round bracket (Capture Groups)
parseCap :: Pos -> [Token] -> [Int] -> Int -> (PM Regex, Int)
parseCap p ts l n =
  let (par, n1)  = parsen p (init cont) (l ++ [n]) (n+1)
      (parr, n2) = parsen p rst l n1
  in (bindPM par (\r -> liftPM ((\r1 r2 -> Seq l r1 r2) (Capture n r)) parr),n2)
    where
      (cont, rst) = splitAt (cntUntilClosed TokenOCap TokenCCap ts) ts

--- Arrow bracket (Variable)
parseABracket :: Pos -> [Token] -> PM (String,[Token])
parseABracket = pOAB []
  where
    pOAB _ p []       = throwPM p "Missing '>'"
    pOAB acc p (t:ts) = case t of
      TokenCABracket   -> cleanPM (reverse acc,ts)
      _                -> pOAB ((tokenToString t) ++ acc) p ts

--- Extract a char from a token (necessary for variables)
extractChars :: [Token] -> String
extractChars []     = ""
extractChars (t:ts) = case t of
  (TokenLiteral c) -> (c:extractChars ts)
  _                -> ""

-- | Helper
cntUntilClosed :: Eq a => a -> a -> [a] -> Int
cntUntilClosed c1 c2 li = cUCB 0 0 li
  where
    cUCB n c l =
      if (c < 0) then n else
        if (l == []) then failed else
          if ((head l) == c1) then cUCB (n+1) (c+1) (tail l) else
            if ((head l) == c2) then cUCB (n+1) (c-1) (tail l)
              else cUCB (n+1) c (tail l)
