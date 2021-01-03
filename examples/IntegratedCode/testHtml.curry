{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains tests for integrated code to support
--- easy writing of HTML code in Curry programs.
------------------------------------------------------------------------------

import HTML.Base
import Test.Prop

htmlTest1 :: String -> [BaseHtml]
htmlTest1 name = ``html
 <html>

  <head>
   <title>Simple Test

  <body>
   <h1>Hello {name}!</h1>
    <p>
     Bye!
    <p>Bye!
   <h2>{reverse name}
   Bye!''

htmlDoc1 :: [BaseHtml]
htmlDoc1 =
  [htmlStruct "html" []
    [htmlStruct "head" []
      [htmlStruct "title" [] [htmlText "Simple Test\n"]],
     htmlStruct "body" []
      [htmlStruct "h1" []
        [htmlText "Hello ", htmlText "Joe", htmlText "!"],
       htmlStruct "p" [] [htmlText "Bye!\n"],
       htmlStruct "p" [] [htmlText "Bye!\n"],
       htmlStruct "h2" []
        [htmlText "eoJ", htmlText "\n"],
       htmlText "Bye!"]]]

------------------------------------------------------------------------------
-- Partial equality on HTML documents for testing.
instance Eq BaseHtml where
  hexp1 == hexp2 = case (hexp1,hexp2) of
    (BaseText s, BaseText t) -> s == t
    (BaseStruct t ats hes, BaseStruct t' ats' hes') ->
                                        t==t' && ats==ats' && hes == hes'
    _ -> error "BaseHTML.==: cannot compare actions"

------------------------------------------------------------------------------

test_Html_code :: Prop
test_Html_code = always (htmlTest1 "Joe" == htmlDoc1)
