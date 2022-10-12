------------------------------------------------------------------------------
--- A Monad for Parsers
---
--- @author Jasper Sikorra - jsi@informatik.uni-kiel.de
--- @version September 2022
------------------------------------------------------------------------------
module CPP.ICode.ParseMonad where

import CPP.ICode.ParseError
import CPP.ICode.ParseWarning
import CPP.ICode.ParsePos

--- Combining ParseResult and Warnings monads into a new monad
newtype PM a = PM (WM (PR a))

instance Functor PM where
  fmap = liftPM

instance Applicative PM where
  pure  = cleanPM

instance Monad PM where
  return = cleanPM
  (>>=)  = bindPM

--- Encapsulate an Error Monad with a Warning Monad creating a PM Monad
warnPM :: PR a -> [Warning] -> PM a
warnPM x w = PM (returnWM x w)

--- Bind
bindPM :: PM a -> (a -> PM b) -> PM b
bindPM (PM m) f = PM $ bindWM m $ \b -> case b of
                                         Errors p -> cleanWM (Errors p)
                                         OK x     -> case f x of
                                                      PM a -> a

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
throwPM p s = PM $ cleanWM (throwPMsg p s)

throwMultiPM :: Pos -> [String] -> PM _
throwMultiPM p strs = PM $ cleanWM (throwPR  (map (\s -> (PError p s)) strs))


--- Return without Errors but with one Warning
singlePM :: a -> Warning -> PM a
singlePM x w = warnOKPM x [w]

--- Remove the Warning Monad from PM
discardWarningsPM :: PM a -> PR a
discardWarningsPM (PM a) = discardWarnings a

--- Extract the Warnings
getWarningsPM :: PM a -> [Warning]
getWarningsPM (PM a) = getWarnings a

--- Apply a function on each Warning
mapWarnsPM :: (Warning -> Warning) -> PM a -> PM a
mapWarnsPM f (PM a) = PM $  mapWarns f a

--- Crumple two Parser Monads
crumplePM :: PM (PM a) -> PM a
crumplePM m = bindPM m id

--- Swap the PM and the IO Monad
swapIOPM :: PM (IO a) -> IO (PM a)
swapIOPM m = swapIOPR (discardWarningsPM m)
             >>= return . flip warnPM (getWarningsPM m)

--- Join multiple Parser Monads into one
sequencePM :: [PM a] -> PM [a]
sequencePM ms = warnPM (sequencePR (map discardWarningsPM ms))
                       (foldr (++) [] (map getWarningsPM ms))

--- fst defined on PM
fstPM :: PM (a,b) -> PM a
fstPM = liftPM fst

--- snd defined on PM
sndPM :: PM (a,b) -> PM b
sndPM = liftPM snd

--- combines two PMs by function f, throws error if at least one of
--- the two carries an error
combinePMs :: (a -> b -> c) -> PM a -> PM b -> PM c
combinePMs f (PM p1) (PM p2) = warnPM (combinePRs f (discardWarnings p1)
                                          (discardWarnings p2))
                            (concatWarns p1 p2)
  where
   concatWarns (WM _ w1) (WM _ w2) = w1 ++ w2
