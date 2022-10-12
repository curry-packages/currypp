------------------------------------------------------------------------------
--- Warnings for Parsers
---
--- @author Jasper Sikorra - jsi@informatik.uni-kiel.de
--- @version January 2014
------------------------------------------------------------------------------
module CPP.ICode.ParseWarning where

import CPP.ICode.ParsePos

--- The Warning Monad
data WM a = WM a [Warning]
type Warning = (Pos,String)

getWarnPos :: Warning -> Pos
getWarnPos = fst

setWarnPos :: Warning -> Pos -> Warning
setWarnPos w p = (p,getWarnMsg w)

getWarnMsg :: Warning -> String
getWarnMsg = snd

setWarnMsg :: Warning -> String -> Warning
setWarnMsg w m = (getWarnPos w,m)

--- The Warning Monad's return function
returnWM :: a -> [Warning] -> WM a
returnWM x ws = WM x ws

--- The Warning Monad's bind function
bindWM :: WM a -> (a -> WM b) -> WM b
bindWM (WM x w1) f = let (WM y w2) = (f x) in WM y (w1 ++ w2)

--- Apply a function on each Warning of a Warning Monad
mapWarns :: (Warning -> Warning) -> WM a -> WM a
mapWarns f w = returnWM (discardWarnings w) (map f (getWarnings w))

-- Return without warnings
cleanWM :: a -> WM a
cleanWM x = returnWM x []

-- Single warning
singleWM :: a -> Warning -> WM a
singleWM x w = returnWM x [w]

-- Escapers
discardWarnings :: WM a -> a
discardWarnings (WM x _) = x

getWarnings :: WM a -> [Warning]
getWarnings (WM _ w) = w
