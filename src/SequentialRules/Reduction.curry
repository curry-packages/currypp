------------------------------------------------------------------------------
--- Module with operations to simplify patterns (like
--- functional patterns, as patterns, lazy patterns).
--- 
--- @author Lasse Folger (with changes by Michael Hanus)
--- @version September 2015
------------------------------------------------------------------------------

module Reduction where

import AbstractCurry.Build
import AbstractCurry.Types
import List
import VariableGenerator

{- In diesem Modul sind Funktionen definiert, die zur Reduktion von
Funktionspattern, As-Pattern und LazyPattern auf Variablen und
Konstruktoren benoetigt werden, definiert.
-}

--Mittels dieser Funktion koennen in ein Modul alle Funktions und Aspattern
--reduziert werden.
newprog:: CurryProg -> CurryProg
newprog (CurryProg a b c fl d) = CurryProg a b c (replaceFuncPattern fl) d 

--Reduziert die Pattern jeder Funktion der uebergebenen Liste
replaceFuncPattern:: [CFuncDecl] -> [CFuncDecl]
replaceFuncPattern funclist = map redefinefuncp funclist
 where
  redefinefuncp (CFunc     b c d e rules) =
                 CFunc b c d e (map transRule rules)
  redefinefuncp (CmtFunc a b c d e rules) =
                 CmtFunc a b c d e (map transRule rules)


--Reduziert die Pattern einer einzelnen Regel
transRule :: CRule -> CRule
transRule (CRule pats (CSimpleRhs rhs ldecls)) =
  transRule (CRule pats (CGuardedRhs [noGuard rhs] ldecls))
transRule r@(CRule pl (CGuardedRhs gl ld)) =
  guardedRule pl' gl' (ld ++ nub lva)
  where (_, npl)        = mapAccumL transPattern vars' pl
        vars'           = vars r
        pl'             = getP npl
        getP []         = []
        getP ((p,_):xs) = p : (getP xs)
        gl'             = if (null (simplifyrep replaced)) 
                                then gl 
                                else mkGuardL gl guardExpr
        (guardExpr,lva) = pToExp (simplifyrep replaced)         
        replaced        = filter notnull (snd (unzip npl))

notnull :: [a] -> Bool
notnull x       = not (null x)

--Hier werden im Pattern durch Variablen ersetzt und die entsprechende Ersetzung
--werden mit zurueckgegeben 
transPattern :: [CVarIName] -> CPattern -> ([CVarIName],(CPattern, [(CVarIName,CPattern)]))
transPattern v (CPComb qn pl)           = (uv,((CPComb qn np'),replaced))
  where (uv,npl)    = mapAccumL transPattern v pl
        np'             = getP npl
        getP []         = []
        getP ((p,_):xs) = p : (getP xs)
        replaced        = simplifyrep (filter notnull (snd (unzip npl)))
transPattern (v:vs) fp@(CPFuncComb _ _) = (vs,((CPVar v),[(v,fp)]++asre))
        where asre = findAs fp
transPattern v x@(CPLit _)              = (v,(x,[]))
transPattern v x@(CPVar _)              = (v,(x,[]))
transPattern v (CPAs n p)               = (uv,((CPAs n np),rl))
  where (uv,(np,rl)) = transPattern v p
transPattern v (CPLazy p)               = (uv,(np,rl)) 
  where (uv,(np,rl)) = transPattern v p
transPattern _ (CPRecord _ _)           = error "Records are not supported in this version"
        
--Erweitert alle Guards um eine Bedingung des Typs Success      
mkGuardL:: [(CExpr,CExpr)] -> CExpr -> [(CExpr,CExpr)] 
mkGuardL [] _           = []
mkGuardL ((g,e):gs) exp = if (g == (CSymbol (pre "success"))) 
                           then [(exp,e)] 
                           else ((applyF (pre "&>") [exp,g]),e): (mkGuardL gs exp)  

--Fast eine Liste von Listen zu einer Liste zusammen
simplifyrep :: [[(CVarIName,CPattern)]] -> [(CVarIName,CPattern)]
simplifyrep []     = []
simplifyrep (x:xs) = x ++ (simplifyrep xs)

--Hier findet die syntaktische Uebersetzung eines Patterns in einen Ausdruck statt.
pToExp :: [(CVarIName,CPattern)] -> (CExpr,[CLocalDecl])
pToExp rep = ((pToExp' rep),freevars)
  where pat             = snd (unzip rep) 
        freevars        = getV' pat
        pToExp' ((va,pa):ys)
          | null ys     = uni pa va
          | otherwise   = applyF (pre "&>") [uni pa va , pToExp' ys]       
        uni pate vn     = applyF (pre "=:<=") [translate pate, CVar vn]
        getV pater      = case pater of
                          (CPVar n) 
                            | (snd n) == "_" -> []
                            | otherwise      -> [CLocalVars [n]]
                          (CPFuncComb _ pl)  -> getV' pl
                          (CPComb _ pl)      -> getV' pl
                          (CPAs n pa)        -> [CLocalVars [n]] ++ getV pa
                          (CPLazy lp)        -> getV lp
                          _                  -> []
        getV' []        = []
        getV' (y:ys)    = (getV y) ++ (getV' ys)

--Findet alle (verschachtelten) As-Pattern
findAs:: CPattern -> [(CVarIName,CPattern)]
findAs (CPVar _)         = []
findAs (CPLit _)         = []
findAs (CPComb _ pl)     = find' pl
  where find' []     = []
        find' (x:xs) = (findAs x) ++ (find' xs)
findAs (CPAs n p)        = (n,simplified p) : (findAs p)
  where simplified x = case x of
                        (CPComb cn ps)     -> CPComb cn (map simplified ps)
                        (CPAs an _)        -> CPVar an
                        (CPFuncComb fn ps) -> CPFuncComb fn (map simplified ps)
                        (CPLazy pa)        -> simplified pa
                        _                 -> x
findAs (CPFuncComb _ pl) = find' pl
  where find' []     = []
        find' (x:xs) = (findAs x) ++ (find' xs)
findAs (CPLazy p)    = findAs p
findAs (CPRecord _ _) = error "Records are not supported in this version"

--Hilfsfunktion zu pToExp
translate:: CPattern -> CExpr
translate (CPComb qn pl)     = applyF qn (map translate pl)
translate (CPVar n)          = CVar n
translate (CPLit v)          = CLit v
translate (CPAs n _)         = CVar n
translate (CPFuncComb qn pl) = applyF qn (map translate pl)
translate (CPLazy p)         = translate p
translate (CPRecord _ _)     = error "Records are not supported in this version of the sequential rule translator!"












