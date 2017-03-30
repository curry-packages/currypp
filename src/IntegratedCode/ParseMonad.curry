------------------------------------------------------------------------------
--- A Monad for Parsers
---
--- @author Jasper Sikorra - jsi@informatik.uni-kiel.de
--- @version January 2014
------------------------------------------------------------------------------
module ParseMonad where

import ParseError
import ParseWarning
import ParsePos

--- Combining ParseResult and Warnings monads into a new monad
type PM a = WM (PR a)

--- Encapsulate an Error Monad with a Warning Monad creating a PM Monad
warnPM :: PR a -> [Warning] -> PM a
warnPM x w = returnWM x w

--- Bind
bindPM :: PM a -> (a -> PM b) -> PM b
bindPM m f = bindWM m $ \b -> case b of
                              Errors p -> cleanWM (Errors p)
                              OK x     -> f x

--- Lift
liftPM :: (a -> b) -> PM a -> PM b
liftPM f m = bindPM m (cleanPM . f)

--- Return without Warnings or Errors
cleanPM :: a -> PM a
cleanPM x = warnOKPM x []

--- Return without Errors but with Warnings
warnOKPM :: a -> [Warning] -> PM a
warnOKPM x = warnPM (okPR x)

--- Return without Warnings but with Errors
throwPM :: Pos -> String -> PM _
throwPM p s = cleanWM (throwPMsg p s)

throwMultiPM :: Pos -> [String] -> PM _
throwMultiPM p strs = cleanWM (throwPR  (map (\s -> (PError p s)) strs))
                                             

--- Return without Errors but with one Warning
singlePM :: a -> Warning -> PM a
singlePM x w = warnOKPM x [w]

--- Remove the Warning Monad from PM
discardWarningsPM :: PM a -> PR a
discardWarningsPM = discardWarnings

--- Extract the Warnings
getWarningsPM :: PM a -> [Warning]
getWarningsPM = getWarnings

--- Apply a function on each Warning
mapWarnsPM :: (Warning -> Warning) -> PM a -> PM a
mapWarnsPM = mapWarns

--- Crumple two Parser Monads
crumplePM :: PM (PM a) -> PM a
crumplePM m = bindPM m id

--- Swap the PM and the IO Monad
swapIOPM :: PM (IO a) -> IO (PM a)
swapIOPM m = swapIOPR (discardWarningsPM m)
             >>= return . flip warnPM (getWarningsPM m)

--- Join multiple Parser Monads into one
sequencePM :: [PM a] -> PM [a]
sequencePM ms = warnPM (sequencePR (map discardWarnings ms))
                       (foldr (++) [] (map getWarnings ms))

--- fst defined on PM
fstPM :: PM (a,b) -> PM a
fstPM = liftPM fst

--- snd defined on PM
sndPM :: PM (a,b) -> PM b
sndPM = liftPM snd

--- combines two PMs by function f, throws error if at least one of
--- the two carries an error
combinePMs :: (a -> b -> c) -> PM a -> PM b -> PM c
combinePMs f p1 p2 = warnPM (combinePRs f (discardWarningsPM p1) 
                                          (discardWarningsPM p2))
                            (concatWarns p1 p2) 
  where                          
   concatWarns (WM _ w1) (WM _ w2) = w1 ++ w2
