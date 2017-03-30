------------------------------------------------------------------------------
--- Module with operations to generate unused variables.
--- 
--- @author Lasse Folger (with changes by Michael Hanus)
--- @version September 2015
------------------------------------------------------------------------------

module VariableGenerator(vars,varsL) where

import AbstractCurry.Types
import List


-- gathers the used variable names        
varsInRule:: CRule -> [String]
varsInRule rule = nub (getRVars rule)
 where
  getPVars (CPVar (_,n))     = [n]
  getPVars (CPLit _)         = []
  getPVars (CPFuncComb _ pl) = concatMap getPVars pl
  getPVars (CPComb _ pl)     = concatMap getPVars pl
  getPVars (CPAs (_,n) pa)   = n : getPVars pa
  getPVars (CPLazy lp)       = getPVars lp
  getPVars (CPRecord _ fds)  = concatMap (getPVars . snd) fds
                             
  getGVars (x,y) = getEVars x ++ getEVars y

  getRhsVars (CSimpleRhs  rhs ldecls) = getEVars rhs ++ concatMap getLVars ldecls
  getRhsVars (CGuardedRhs gs  ldecls) = concatMap getGVars gs  ++
                                        concatMap getLVars ldecls
  
  getEVars (CVar (_,n))         = [n]
  getEVars (CLit _)             = []
  getEVars (CSymbol _)          = []
  getEVars (CApply e1 e2)       = getEVars e1 ++ getEVars e2
  getEVars (CLambda pl le)      = concatMap getPVars pl ++ getEVars le
  getEVars (CLetDecl ld le)     = concatMap getLVars ld ++ getEVars le
  getEVars (CDoExpr sl)         = concatMap getSVars sl
  getEVars (CListComp le sl)    = getEVars le ++ concatMap getSVars sl
  getEVars (CCase _ ce bl)      = getEVars ce ++ concatMap getBVars bl
  getEVars (CTyped te _)        = getEVars te
  getEVars (CRecConstr _ upds)  = concatMap (getEVars . snd) upds
  getEVars (CRecUpdate re upds) = getEVars re ++ concatMap (getEVars . snd) upds
                
  getSVars (CSExpr e)  = getEVars e
  getSVars (CSPat p e) = getPVars p ++ getEVars e
  getSVars (CSLet ld)  = concatMap getLVars ld
                             
  getBVars (p,rhs) = getPVars p ++ getRhsVars rhs

  getLVars (CLocalFunc f)     = getFVars f
  getLVars (CLocalPat p rhs)  = getPVars p ++ getRhsVars rhs
  getLVars (CLocalVars lvars) = map snd lvars
                             
  getFVars (CFunc _ _ _ _ r)     = concatMap getRVars r
  getFVars (CmtFunc _ _ _ _ _ r) = concatMap getRVars r

  getRVars (CRule pats rhs) = concatMap getPVars pats ++ getRhsVars rhs

-- determines unused variables for a given rule
vars :: CRule -> [CVarIName]
vars r = zip [-1,-2..] newVars
 where
  newVars  = filter (`notElem` usedVars) ['x':(show n) | n <- [1..]]
  usedVars = varsInRule r

-- determines unused variables for a given list of rules
varsL :: [CRule] -> [CVarIName]
varsL rs = zip [-1,-2..] newVars
 where
  newVars  = filter (`notElem` usedVars) ['x':(show n) | n <- [1..]]
  usedVars = concatMap varsInRule rs        
