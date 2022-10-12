------------------------------------------------------------------------------
--- A simple Library for the HTML Content Model.
---
--- @author Max Deppert
--- @version March 2014
------------------------------------------------------------------------------
module CPP.ICode.Parser.ML.HTMLContentModel where

import CPP.ICode.Parser.ML.Types

import Data.Char

isHtmlElement :: Symbol -> Bool
isHtmlElement sym = isTag sym && elem (map toLower (tgn sym)) htmlElements

isVoidElement :: Symbol -> Bool
isVoidElement sym = isTag sym && elem (map toLower (tgn sym)) voidElements

isStrictElement :: Symbol -> Bool
isStrictElement sym = isTag sym && elem (map toLower (tgn sym)) strictElements

isPhrasingElement :: Symbol -> Bool
isPhrasingElement sym = isPlain sym || elem (map toLower (tgn sym)) phrasingElements

isFlowElement :: Symbol -> Bool
isFlowElement sym = isTag sym && elem (map toLower (tgn sym)) flowElements

voidElements :: [String]
voidElements = ["area","base","br","col","command","embed","hr",
                "img","input","keygen","link","meta","param",
                "source","track","wbr"]

strictElements :: [String]
strictElements = ["script","style","pre"]

flowElements :: [String]
flowElements = phrasingElements ++
               ["a","p","hr","pre","ul","ol","dl","div","h1",
                "h2","h3","h4","h5","h6","hgroup","address",
                "blockquote","ins","del","object","map",
                "noscript","section","nav","article","aside",
                "header","footer","video","audio","figure",
                "table","form","fieldset","menu","canvas",
                "details"]

phrasingElements :: [String]
phrasingElements = ["a","em","strong","small","mark","abbr",
                    "dfn","i","b","s","u","code","var","samp",
                    "kbd","sup","sub","q","cite","span","bdo",
                    "bdi","br","wbr","ins","del","img","embed",
                    "object","iframe","map","area","script",
                    "noscript","ruby","video","audio","input",
                    "textarea","select","button","label","output",
                    "datalist","keygen","progress","command",
                    "canvas","time","meter"]

htmlElements :: [String]
htmlElements = ["a","abbr","address","area","article","aside","audio",
                "b","base","bdi","bdo","blockquote","body","br","button",
                "caption","cite","code","col","colgroup","command",
                "datalist","dd","del","details","dfn","div","dl","dt",
                "em","embed","fieldset","figcaption","figure","footer",
                "form","h1","h2","h3","h4","h5","h6","head","header",
                "hgroup","hr","html","i","iframe","img","input","ins",
                "kbd","keygen","label","legend","li","link","map","mark",
                "menu","meta","meter","nav","noscript","object","ol",
                "optgroup","option","output","p","param","pre","progress",
                "q","rp","rt","ruby","s","samp","script","section",
                "select","small","source","span","strong","style","sub",
                "summary","sup","table","tbody","td","textarea","tfoot",
                "th","thead","time","title","tr","track","u","ul","var",
                "video","wbr"]
