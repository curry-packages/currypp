{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- a form with a text input field and two event handlers
-- to reverse or duplicate the input.
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

-- Example: a form with a text input field and two submit buttons.
revDupForm :: HtmlFormDef ()
revDupForm = simpleFormDef
         ``html
	     Enter a string: {{textField tref ""}}
	     <hr>
             {{button "Reverse string"   revhandler}}
             {{button "Duplicate string" duphandler}}''
 where
  tref free

  revhandler env = return $ page "Answer"
    ``html <h1>Reversed input: {reverse (env tref)}''

  duphandler env = return $ page "Answer"
    ``html
       <h1>
         Duplicated input:
         {env tref ++ env tref}''

-- main HTML page containing the form
main :: IO HtmlPage
main = return $ page "Question" $
  ``html
     <h1>This is an example form:
     {{formElem revDupForm}}''

-- Install with:
-- > cypm exec curry2cgi -o ~/public_html/cgi-bin/revdup.cgi RevDup
