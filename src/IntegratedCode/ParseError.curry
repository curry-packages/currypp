------------------------------------------------------------------------------
--- Result Monad for Parsers
---
--- @author Jasper Sikorra - jsi@informatik.uni-kiel.de
--- @version January 2014
------------------------------------------------------------------------------
module ParseError where

import ParsePos

err_unknown_msg :: String
err_unknown_msg = "Unknown error"

err_unknown_fname :: String
err_unknown_fname = "Unknown filename"

--- The Error Monad
data PR a = OK a | Errors [PError]
data PError = PError Pos String

getPErrorPos :: PError -> Pos
getPErrorPos (PError p _) = p

getPErrorMsg :: PError -> String
getPErrorMsg (PError _ m) = m

--- Construct a PError
perror :: Pos -> String -> PError
perror p s = PError p s

--- Return without errors
okPR :: a -> PR a
okPR x = OK x

--- Return with errors
throwPR :: [PError] -> PR a
throwPR p = Errors p

--- Bind function
bindPR :: PR a -> (a -> PR b) -> PR b
bindPR (OK x)     f = f x
bindPR (Errors p) _ = Errors p

--- Escape the error monad, basically a catch
escapePR :: PR a -> ([PError] -> IO a) -> IO a
escapePR (OK x)     _ = return x
escapePR (Errors e) f = f e

--- Lift function
liftPR :: (a -> b) -> PR a -> PR b
liftPR f m = bindPR m (okPR . f)

--- Throw an unknown error
throwUnknownPR :: PR a
throwUnknownPR = throwPR [PError (initPos err_unknown_fname) err_unknown_msg]

--- Throw an error with one PError
throwOnePR :: PError -> PR a
throwOnePR p = throwPR [p]

--- Throw an error with one PError that has a position and message
throwPMsg :: Pos -> String -> PR a
throwPMsg p s= throwOnePR (perror p s)

--- Add a list of errors to the Error Monad
addErrorsPR :: PR a -> [PError] -> PR a
addErrorsPR m ps = case m of
  OK _     -> throwPR ps
  Errors p -> Errors (p ++ ps)

-- Add one error to the Error Monad
addOneErrorPR :: PR a -> PError -> PR a
addOneErrorPR m p = addErrorsPR m [p]

--- Swap the PR and the IO Monads
swapIOPR :: PR (IO a) -> IO (PR a)
swapIOPR (OK x)     = x >>= return . okPR
swapIOPR (Errors p) = return (throwPR p)

--- fst defined on the Error Monad
fstPR :: PR (a,b) -> PR a
fstPR m = bindPR m (okPR . fst)

--- snd defined on the Error Monad
sndPR :: PR (a,b) -> PR b
sndPR m = bindPR m (okPR . snd)

--- Crumple two Error Monads
crumplePR :: PR (PR a) -> PR a
crumplePR m = bindPR m (\n -> bindPR n okPR)

--- Join two Error Monads
concatPR :: PR [a] -> PR [a] -> PR [a]
concatPR (OK x) (OK y)           = okPR (x ++ y)
concatPR (Errors p1) (Errors p2) = Errors (p1 ++ p2)
concatPR (Errors p1) (OK _)      = Errors p1
concatPR (OK _)      (Errors p2) = Errors p2

--- Combines two PRs by a given functions
combinePRs :: (a -> b -> c) -> PR a -> PR b -> PR c
combinePRs f (OK x) (OK y)           = okPR (f x y)
combinePRs _ (Errors p1) (Errors p2) = Errors (p1 ++ p2)
combinePRs _ (Errors p1) (OK _)      = Errors p1
combinePRs _ (OK _)      (Errors p2) = Errors p2

--- Join multiple Error Monads into one
sequencePR :: [PR a] -> PR [a]
sequencePR []       = okPR []
sequencePR (pr:prs) = concatPR
                        (bindPR pr $ \x -> okPR [x])
                        (sequencePR prs)
