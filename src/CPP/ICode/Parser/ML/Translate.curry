------------------------------------------------------------------------------
--- Function to translate Markup Language strings into Curry code strings.
---
--- @author Max Deppert, Michael Hanus
--- @version February 2015
------------------------------------------------------------------------------
module CPP.ICode.Parser.ML.Translate (translate) where

import CPP.ICode.ParseError
import CPP.ICode.ParseTypes
import CPP.ICode.Parser.ML.Parser
import CPP.ICode.Parser.ML.Types

import Data.List

showText :: Text -> String
showText (Raw  s) = show (dropTrailingCR s)
showText (ExpT s) = "(" ++ s ++ ")"
showText (ExpC s) = "(" ++ s ++ ")"

showData :: [Text] -> String
showData [] = "\"\""
showData ds@(_:_) = "(" ++ intercalate "++" (map showText ds) ++ ")"

showAttr :: Attribute -> String
showAttr (s,ds) = "(" ++ show s ++ "," ++ showData ds ++ ")"

showAttrs :: [Attribute] -> String
showAttrs xs = "[" ++ (intercalate "," (map showAttr xs)) ++ "]"

translate :: String -> LangParser
translate kind | kind == "html" = translateHTML
               | kind == "xml"  = translateXML
               | otherwise = error "translate: unknown kind"

translateHTML :: LangParser
-- translates a HTML string to a Curry string
translateHTML start input =
  return $ (warnOKPM (showStringList (map showTree trees)) ws)
 where
  (trees,ws) = parse H input (toSimplePos start,0)

  showTree :: Tree -> String
  showTree (Tree (Content ds) _) = intercalate "," (map showCont ds)
  showTree (Tree (Element a par) ys) =
    "htmlStruct " ++ show a ++ " " ++ showAttrs par ++ " "
                  ++ showStringList (map showTree ys)
           
  showCont :: Text -> String
  showCont (Raw  s) = "htmlText " ++ show s ++ ""
  showCont (ExpT s) = "htxt (" ++ s ++ ")"
  showCont (ExpC s) = "(" ++ s ++ ")"


-- translates a XML string to a Curry string
translateXML :: LangParser
translateXML start input =
  return $ (warnOKPM (showStringList (map showTree trees)) ws)
 where
  (trees,ws) = parse X input (toSimplePos start,0)

  showTree :: Tree -> String
  showTree (Tree (Content ds) _) =
    if any isExpC ds
    then intercalate "," (map showCont (dropLastRawCR ds))
    else "XText " ++ showData ds
  showTree (Tree (Element a par) ys) =
    "XElem " ++ show a ++ " " ++ showAttrs par ++ " "
             ++ showStringList (map showTree ys)

  isExpC t = case t of ExpC _ -> True
                       _      -> False

  showCont :: Text -> String
  showCont (Raw  s) = "XText " ++ show (dropTrailingCR s) ++ ""
  showCont (ExpT s) = "xtxt (" ++ dropTrailingCR s ++ ")"
  showCont (ExpC s) = "(" ++ dropTrailingCR s ++ ")"

--
showStringList :: [String] -> String
showStringList xs = "[" ++ (intercalate "," xs) ++ "]"

-- remove last raw "\n" in a text list:
dropLastRawCR :: [Text] -> [Text]
dropLastRawCR [] = []
dropLastRawCR [c] = if c==Raw "\n" then [] else [c]
dropLastRawCR (c1:c2:cs) = c1 : dropLastRawCR (c2:cs)

-- remove trailing \n char in a string:
dropTrailingCR :: String -> String
dropTrailingCR [] = []
dropTrailingCR [c] = if c=='\n' then [] else [c]
dropTrailingCR (c1:c2:cs) = c1 : dropTrailingCR (c2:cs)
