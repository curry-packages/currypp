{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains examples for integrated code to support
--- easy writing of HTML code in Curry programs.
------------------------------------------------------------------------------

import HTML.Base

{-
Remark:

The integrated code ``html ...'' expands to a list of HTML expressions,
i.e., to an expression of type `[a]` where `a` is an instance of class `HTML`.

A Curry expression enclosed with {...} inside the integrated code
must be of type String and is inserted as (HTML-quoted) text.

A Curry expression enclosed with {{...}} inside the integrated code
must be of type `a`, where `a` is an instance of class `HTML`,
and is inserted as an HTML element.
-}

test1 :: String -> IO ()
test1 name = putStrLn $ showStaticHtmls ``html
 <html>

  <head>
   <title>Simple Test

  <body>
   <h1>Hello {name}!</h1>
    <p>
     Sorry, i've got nothing to say...but:
   <h2>{reverse sey ++ "we " ++ "can!"}
   Have a wonderful time!''

sey :: String
sey = " seY"
