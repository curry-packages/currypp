------------------------------------------------------------------------------
--- Module defining main operations to transform programs in order to
--- implement a sequential rule selection strategy.
--- 
--- @author Lasse Folger (with changes by Michael Hanus)
--- @version September 2015
------------------------------------------------------------------------------

import Reduction
import AbstractCurry.Types
import AbstractCurry.Select(progName)
import List(partition)
import Selection
import System
import Translation

--- Start sequentializer in "preprocessor mode":
transSequentialRules :: Int -> [String] -> String -> CurryProg -> IO CurryProg
transSequentialRules _ _ _ inputProg =
  return (translate inputProg (progName inputProg))

--- Main operation to translate a Curry module into a new one implementing
--- a sequential rule selection strategy. It consists of the selection
--- of operations defined by non-deterministic patterns, reduce patterns
--- to a normalized structure, and renaming qualified names to the
--- name of the output module.
translate :: CurryProg -> String -> CurryProg
translate inputProg outputName = outputProg
 where
  (CurryProg a b c funcs d) = inputProg
  renamedtypes              = renameT a outputName c 
  renamedfuncs              = renameF a outputName funcs
  (ndet,det)                = partition isnondeterministic renamedfuncs
  simpleProg                = Reduction.newprog (CurryProg a b c ndet d)
  (CurryProg a' b' _ d' e)  = Translation.newprog simpleProg outputName
  outputProg                = CurryProg a' b' renamedtypes (d' ++ det) e

-- Operations to rename qualified names to new module name.

-- Rename qualified name.
renameQN :: String -> String -> QName -> QName
renameQN iname oname (a,b) = if a == iname then (oname,b) else (a,b)

-- Rename type declarations.
renameT :: String -> String -> [CTypeDecl] -> [CTypeDecl]
renameT iname oname x = map renameTD x
 where
  rename n = renameQN iname oname n
  
  renameTD (CType n a b c)    = CType (rename n) a b (map renameC c)
  renameTD (CTypeSyn n a b t) = CTypeSyn (rename n) a b (renameTE t)
  renameTD (CNewType n a b t) = CNewType (rename n) a b (renameC t)
  
  renameC (CCons n v t)       = CCons (rename n) v (map renameTE t)
  renameC (CRecord n v fs)    = CRecord (rename n) v (map renameFD fs)
  
  renameFD (CField n v te)    = CField (rename n) v (renameTE te)
  
  renameTE v@(CTVar _)        = v
  renameTE (CFuncType i o)    = CFuncType (renameTE i) (renameTE o)
  renameTE (CTCons n t)       = CTCons (rename n) (map renameTE t)

-- Rename function declarations.
renameF :: String -> String -> [CFuncDecl] -> [CFuncDecl]
renameF iname oname fl = map renameF' fl
 where
  rename n = renameQN iname oname n
  
  renameF' (CFunc n a v te r)     =
            CFunc (rename n) a v (renameTE te) (map renameR r)
  renameF' (CmtFunc c n a v te r) =
            CmtFunc c (rename n) a v (renameTE te) (map renameR r)
            
  renameTE te = case te of
                  (CTVar _)       -> te
                  (CFuncType i o)   -> CFuncType (renameTE i) (renameTE o)
                  (CTCons n t)      -> CTCons (rename n) (map renameTE t)
                  
  renameR (CRule p rhs)         = CRule (map renameP p) (renameRhs rhs)
  renameRhs (CSimpleRhs exp ld) = CSimpleRhs  (renameE exp) (map renameLD ld)
  renameRhs (CGuardedRhs gs ld) = CGuardedRhs (map renameG gs)  (map renameLD ld)
  
  renameP pat@(CPVar _)     = pat
  renameP pat@(CPLit _)     = pat
  renameP (CPComb n pa)     = CPComb (rename n) (map renameP pa)
  renameP (CPAs id p)       = CPAs id (renameP p)
  renameP (CPFuncComb n pa) = CPFuncComb (rename n) (map renameP pa)
  renameP (CPLazy p)        = CPLazy (renameP p)
  renameP (CPRecord m t)    = CPRecord m (map renamePRec t)
  
  renamePRec (n,te) = (n, renameP te)

  renameG (e1,e2) = (renameE e1, renameE e2)
  
  renameE exp@(CVar _)      = exp
  renameE exp@(CLit _)      = exp
  renameE (CSymbol n)       = CSymbol (rename n)
  renameE (CApply e1 e2)    = CApply (renameE e1) (renameE e2)
  renameE (CLambda pa e)    = CLambda (map renameP pa) (renameE e)
  renameE (CLetDecl ld e)   = CLetDecl (map renameLD ld) (renameE e)
  renameE (CDoExpr s)       = CDoExpr (map renameS s)
  renameE (CListComp e s)   = CListComp (renameE e) (map renameS s)
  renameE (CCase ct e b)    = CCase ct (renameE e) (map renameB b)
  renameE (CTyped e t)      = CTyped (renameE e) (renameTE t)
  renameE (CRecConstr n re) = CRecConstr (rename n) (map renameRC re)
  renameE (CRecUpdate e re) = CRecUpdate (renameE e) (map renameRC re)

  renameLD locd = case locd of
    (CLocalFunc fd)    -> CLocalFunc (renameF' fd)
    (CLocalPat p rhs)  -> CLocalPat (renameP p) (renameRhs rhs)
    (CLocalVars _)     -> locd
    
  renameS sta = case sta of
                  (CSExpr e)  -> CSExpr (renameE e)
                  (CSPat p e) -> CSPat (renameP p) (renameE e)
                  (CSLet ld)  -> CSLet (map renameLD ld)
                  
  renameB (p,rhs) = (renameP p, renameRhs rhs)

  renameRC (s,e) = (rename s, renameE e)





