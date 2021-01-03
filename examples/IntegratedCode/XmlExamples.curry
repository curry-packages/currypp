{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=foreigncode --optF=-o #-}

------------------------------------------------------------------------------
--- This program contains examples for integrated code to support
--- easy writing of XML expressions.
------------------------------------------------------------------------------

{-
Remark:

The integrated code ``xml ...'' expands to a list of XML expressions,
i.e., to an expression of type [XmlExp].

A Curry expression enclosed with {...} inside the integrated code
must be of type String and is inserted as text (inside the surrounding
XML element).

A Curry expression enclosed with {{...}} inside the integrated code
must be of type XmlExp and is inserted as a XML element.
-}

import XML

test1 :: IO ()
test1 = putStrLn $ showXmlDoc $ head ``xml
 <contact>
  <entry>
   <phone>+49-431-8807271
   <name>Hanus
   <first>Michael
   <email>mh@informatik.uni-kiel.de
   <email>hanus@email.uni-kiel.de
   
  <entry>
   <name>Smith
   <first>Bill
   <phone>+1-987-742-9388
 ''

test2 :: IO ()
test2 = let name = "ama"
         in putStrLn $ showXmlDoc $ head ``xml
  <contact>
    <entry>
      <name>Ob{reverse name}
      <first>Barack
    {{xml "entry" [xml "name" [xtxt $ "M"++name]]}}
  ''
  