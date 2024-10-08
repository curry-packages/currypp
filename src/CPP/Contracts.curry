------------------------------------------------------------------------
--- A transformation from a Curry program with pre/postconditions and/or
--- specification into a Curry program where these conditions are integrated
--- into the code as run-time assertions.
---
--- The general idea of contracts and this transformation is described in:
---
--- > S. Antoy, M. Hanus:
--- > Contracts and Specifications for Functional Logic Programming
--- > Proc. 14th International Symposium on Practical Aspects of
--- > Declarative Languages (PADL 2012), pp. 33-47, Springer LNCS 7149, 2012
---
--- @author Michael Hanus
--- @version January 2024
------------------------------------------------------------------------

module CPP.Contracts ( main, translateContracts )
  where

import Control.Monad                 ( when, unless )
import Curry.Compiler.Distribution   ( installDir )
import Data.Char
import Data.List
import System.Environment            ( getArgs )

import AbstractCurry.Types
import AbstractCurry.Files
import AbstractCurry.Pretty
import AbstractCurry.Build
import AbstractCurry.Select
import AbstractCurry.Transform
import Contract.Names
import Contract.Usage   ( checkContractUsage )
import FlatCurry.Files  ( readFlatCurry )
import qualified FlatCurry.Goodies as FCG
import System.CurryPath ( lookupModuleSourceInLoadPath, modNameToPath
                        , stripCurrySuffix )
import System.Directory
import System.FilePath  ( takeDirectory )
import System.Process   ( system )

-- in order to use the determinism analysis:
import Analysis.ProgInfo      ( ProgInfo, lookupProgInfo )
import Analysis.Deterministic ( Deterministic(..), nondetAnalysis )
import CASS.Server            ( analyzeGeneric )

import SimplifyPostConds
import TheoremUsage

import CPP.CompileWithFrontend ( compileImportedModule )
import CPP.Helpers             ( checkRequiredImport, setFunMod )

------------------------------------------------------------------------

banner :: String
banner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText = "Contract Transformation Tool (Version of 01/11/21)"
   bannerLine = take (length bannerText) (repeat '=')

------------------------------------------------------------------------
--- Execute the contract wrapper in "preprocessor mode".
--- The Curry program must be read with `readCurry` (and not
--- `readUntypedCurry`) in order to correctly process arities
--- based on function types!
--- The result is `Nothing` if no transformation was applied or `Just` the
--- transformed program.
translateContracts :: Int -> [String] -> String -> String -> CurryProg
                   -> IO (Maybe CurryProg)
translateContracts verb moreopts modname srcprog inputProg = do
  when (verb>1) $ putStr banner
  opts <- processOpts defaultOptions moreopts
  transformCProg verb opts modname srcprog inputProg (progName inputProg)
 where
  processOpts opts ppopts = case ppopts of
    []          -> return opts
    ("-e":more) -> processOpts (opts { withEncapsulate   = True }) more
    ("-t":more) -> processOpts (opts { topLevelContracts = True }) more
    _           -> showError
   where
    showError = do
      putStrLn $ "Unknown options (ignored): " ++ show (unwords ppopts)
      return opts

------------------------------------------------------------------------
-- Data type for transformation parameters
data Options = Options
  { -- encapsulate assertion checking by set functions?
    withEncapsulate   :: Bool
    -- should contracts be asserted only to top-level entries of an operation
    -- or also to all (recursive) calls?
  , topLevelContracts :: Bool
    -- load and execute transformed program?
  , executeProg       :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { withEncapsulate   = False
  , topLevelContracts = False
  , executeProg       = False
  }

------------------------------------------------------------------------
-- Start the contract wrapper in "stand-alone mode":
main :: IO ()
main = do
  putStrLn banner
  args <- getArgs
  processArgs defaultOptions args
 where
  processArgs opts args = case args of
     ("-e":margs) -> processArgs (opts { withEncapsulate   = True }) margs
     ("-t":margs) -> processArgs (opts { topLevelContracts = True }) margs
     ("-r":margs) -> processArgs (opts { executeProg       = True }) margs
     [mnamec]        -> let mname = stripCurrySuffix mnamec
                         in transformStandalone opts mname
                                      (transformedModName mname ++ ".curry")

     _ -> putStrLn $ unlines $
           ["ERROR: Illegal arguments for transformation: " ++ unwords args
           ,""
           ,"Usage: cwrapper [-e] [-t] [-r] <module_name>"
           ,"-e   : encapsulate nondeterminism of assertions"
           ,"-t   : assert contracts only to top-level (not recursive) calls"
           ,"-r   : load the transformed program into Curry system"
           ]

-- Prepare to call the main transformation for stand-alone mode:
transformStandalone :: Options -> String -> String -> IO ()
transformStandalone opts modname outfile = do
  mmodsrc <- lookupModuleSourceInLoadPath modname
  srcprog <- case mmodsrc of
               Nothing -> error $
                            "Source code of module '"++modname++"' not found!"
               Just (_,progname) -> readFile progname
  let acyfile = abstractCurryFileName modname
  doesFileExist acyfile >>= \b -> if b then removeFile acyfile else return ()
  prog <- readCurry modname
  doesFileExist acyfile >>= \b -> if b then return ()
                                       else error "Source program incorrect"
  let outmodname = transformedModName modname
  newprog <- transformCProg 1 opts modname srcprog prog outmodname
  writeFile outfile (showCProg (maybe prog id newprog) ++ "\n")
  when (executeProg opts) $ loadIntoCurry outmodname

-- Specifies how the name of the transformed module is built from the
-- name of the original module.
transformedModName :: String -> String
transformedModName m = m++"C"

-- start Curry system (PAKCS, KiCS2) and load a module:
loadIntoCurry :: String -> IO ()
loadIntoCurry m = do
  putStrLn $ "\nStarting Curry system and loading module '"++m++"'..."
  system $ installDir++"/bin/curry :l "++m
  return ()

------------------------------------------------------------------------
--- The main transformation operation with parameters:
--- * verbosity level
--- * options
--- * module name
--- * source text of the module
--- * AbstractCurry representation of the module
--- * name of the output module (if it should be renamed)
--- The result is Nothing if no transformation was applied or Just the
--- transformed program.
transformCProg :: Int -> Options -> String -> String -> CurryProg -> String
               -> IO (Maybe CurryProg)
transformCProg verb opts modname srctxt orgprog outmodname = do
  fcyprog <- readFlatCurry modname
  let usageerrors = checkContractUsage modname
                      (map (\fd -> (snd (FCG.funcName fd), FCG.funcType fd))
                           (FCG.progFuncs fcyprog))
  unless (null usageerrors) $ do
    putStr (unlines $ "ERROR: ILLEGAL USE OF CONTRACTS:" :
               map (\ ((mn,fn),err) -> fn ++ " (module " ++ mn ++ "): " ++ err)
                   usageerrors)
    error "Contract transformation aborted"
  let prog         = addCmtFuncInProg orgprog -- to avoid constructor CFunc
      funposs      = linesOfFDecls srctxt prog
      fdecls       = functions prog
      funspecs     = getFunDeclsWith isSpecName prog
      specnames    = map (fromSpecName . snd . funcName) funspecs
      preconds     = getFunDeclsWith isPreCondName prog
      prenames     = map (fromPreCondName  . snd . funcName) preconds
      opostconds   = getFunDeclsWith isPostCondName prog
  -- filter theorems which have a proof file:
  theofuncs <- getTheoremFunctions
                (takeDirectory (modNameToPath (progName prog))) prog
  postconds <- simplifyPostConditionsWithTheorems verb theofuncs opostconds
  let postnames = map (fromPostCondName  . snd . funcName) postconds
      checkfuns = union specnames (union prenames postnames)
  if null checkfuns
   then do
     when (verb>1) $
       putStrLn "Contract transformation not required since no contracts found!"
     return Nothing
   else do
     when (verb>0) $
       putStrLn $ "Adding contract checking to: " ++ unwords checkfuns
     detinfo <- analyzeGeneric nondetAnalysis (progName prog)
                                              >>= return . either id error
     newprog <- transformProgram verb opts funposs fdecls detinfo 
                                 funspecs preconds postconds prog
     return (Just (renameCurryModule outmodname newprog))

-- Get functions from a Curry module with a name satisfying the predicate:
getFunDeclsWith :: (String -> Bool) -> CurryProg -> [CFuncDecl]
getFunDeclsWith pred prog = filter (pred . snd . funcName) (functions prog)

------------------------------------------------------------------------
-- Transform a given program w.r.t. given specifications and pre/postconditions
transformProgram :: Int -> Options -> [(QName,Int)]-> [CFuncDecl]
                 -> ProgInfo Deterministic -> [CFuncDecl]
                 -> [CFuncDecl] -> [CFuncDecl] -> CurryProg -> IO CurryProg
transformProgram verb opts funposs allfdecls detinfo
  specdecls predecls postdecls
  (CurryProg mname imps dfltdecl clsdecls instdecls tdecls
             orgfdecls opdecls) = do
  let -- replace in program old postconditions by new simplified postconditions:
      fdecls = filter (\fd -> funcName fd `notElem` map funcName postdecls)
                      orgfdecls ++ postdecls
      newpostconds = concatMap
                       (genPostCond4Spec opts allfdecls detinfo postdecls)
                       specdecls
      newfunnames  = map (snd . funcName) newpostconds
      -- remove old postconditions which are transformed into postconditions
      -- with specification checking:
      wonewfuns    = filter (\fd -> snd (funcName fd) `notElem` newfunnames)
                            fdecls
      -- compute postconditions actually used for contract checking:
      contractpcs  = postdecls++newpostconds
  checkRequiredImport mname contractMod imps
  unless (contractMod `elem` imps) $ compileImportedModule verb contractMod
  return $ CurryProg
    mname (nub (contractMod : setFunMod : imps))
    dfltdecl clsdecls instdecls tdecls
    (map deleteCmtIfEmpty
        (concatMap (addContract opts funposs allfdecls predecls contractpcs)
                   wonewfuns ++ newpostconds))
    opdecls

-- Add an empty comment to each function which has no comment
addCmtFuncInProg :: CurryProg -> CurryProg
addCmtFuncInProg
  (CurryProg mname imps dfltdecl clsdecls instdecls tdecls fdecls opdecls) =
   CurryProg mname imps dfltdecl clsdecls instdecls tdecls
             (map addCmtFunc fdecls) opdecls
 where
  addCmtFunc (CFunc qn ar vis texp rs) = CmtFunc "" qn ar vis texp rs
  addCmtFunc (CmtFunc cmt qn ar vis texp rs) = CmtFunc cmt qn ar vis texp rs

-- Generate a postcondition from a specification that is parameterized
-- by an "observation function".
-- If the specification is deterministic, generate an equality check,
-- otherwise generate a set containment check.
genPostCond4Spec :: Options -> [CFuncDecl] -> ProgInfo Deterministic
                 -> [CFuncDecl] -> CFuncDecl -> [CFuncDecl]
genPostCond4Spec _ _ _ _ (CFunc _ _ _ _ _) = error "genPostCond4Spec"
genPostCond4Spec _ allfdecls detinfo postdecls
               (CmtFunc _ (m,f) ar vis (CQualType (CContext clscons) texp) _) =
 let fname     = fromSpecName f
     -- is the specification deterministic?
     detspec   = maybe False (== Det) (lookupProgInfo (m,f) detinfo)
     fpostname = toPostCondName fname
     fpgenname = fpostname++"'generic"
     oldfpostc = filter (\fd -> snd (funcName fd) == fpostname) postdecls
     oldcmt    = if null oldfpostc then ""
                                   else '\n' : funcComment (head oldfpostc)
     varg      = (0,"g")
     argvars   = map (\i -> (i,"x"++show i)) [1..(ar+1)]
     spargvars = take ar argvars
     resultvar = last argvars
     gtype     = CTVar (0,"grt") -- result type of observation function
     varz      = (ar+2,"z")
     obsfun    = maybe (pre "id")
                       funcName
                       (find (\fd -> snd (funcName fd) == fpostname++"'observe")
                             allfdecls)
     gspecname = (m,f++"'g")
     gspec     = cfunc gspecname ar Private
                  (CQualType
                   (CContext (singleCConstraint (pre "Eq") gtype : clscons))
                   ((resultType texp ~> gtype) ~> replaceResultType texp gtype))
                  [let gsargvars = map (\i -> (i,"x"++show i)) [1..ar]
                   in  simpleRule (CPVar varg : map CPVar gsargvars)
                                  (CApply (CVar varg)
                                          (applyF (m,f) (map CVar gsargvars)))]
     postcheck =
       CLetDecl
         [CLocalPat (CPVar varz)
                    (CSimpleRhs (CApply (CVar varg) (CVar resultvar)) [])]
         (if detspec
            then applyF (pre "==")
                   [CVar varz, applyF gspecname (map CVar (varg : spargvars))]
            else applyF (pre "&&")
                   [applyF (pre "==") [CVar varz, CVar varz],
                    applyF (sfMod "valueOf")
                      [CVar varz,
                       -- Note that this is not a legal (source code) use
                       -- of set functions due to the application of the
                       -- observation operation `g`. Thus, the transformed
                       -- program should not be checked with source code checks
                       -- by CurryCheck.
                       applyF (sfMod $ "set" ++ show ar)
                         (applyF gspecname [CVar varg] : map CVar spargvars)]])
     rename qf = if qf == (m,fpostname) then (m,fpostname++"'org") else qf
  in [cmtfunc
       ("Parametric postcondition for '"++fname++
        "' (generated from specification). "++oldcmt)
       (m,fpgenname) (ar+2) Private
       (CQualType (CContext (singleCConstraint (pre "Eq") gtype : clscons))
                  ((resultType texp ~> gtype) ~> extendFuncType texp boolType))
       [if null oldfpostc
        then simpleRule (map CPVar (varg:argvars)) postcheck
        else simpleRuleWithLocals
                (map CPVar (varg:argvars))
                (applyF (pre "&&")
                             [applyF (rename (funcName (head oldfpostc)))
                                     (map CVar argvars),
                              postcheck])
                [updQNamesInCLocalDecl rename
                        (CLocalFunc (deleteCmt (head oldfpostc)))]]
     ,gspec
     ,cmtfunc
       ("Postcondition for '"++fname++"' (generated from specification). "++
        oldcmt)
       (m,fpostname) (ar+1) vis
       (CQualType
          (CContext (union (type2EqConstraints (resultType texp)) clscons))
          (extendFuncType texp boolType))
       [simpleRule (map CPVar argvars)
                   (applyF (m,fpgenname)
                           (constF obsfun : map CVar argvars))]
     ]


-- Transform a type into Eq constraints for all type variables occurring
-- in this type. Note: this is not sufficient since one needs also be
-- sure that each type constructor has an Eq instance.
type2EqConstraints :: CTypeExpr -> [CConstraint]
type2EqConstraints texp =
  map (\tv -> singleCConstraint (pre "Eq") (CTVar tv))
      (nub (tvarsOfType texp))

-- Transform a type into Eq constraints for all type variables occurring
-- in this type. Note: this is not sufficient since one needs also be
-- sure that each type constructor has an Eq instance.
type2ShowConstraints :: CTypeExpr -> [CConstraint]
type2ShowConstraints texp =
  map (\tv -> singleCConstraint (pre "Show") (CTVar tv))
      (nub (tvarsOfType texp))

-- Extends a qualified type with Show constraints for all type variables
-- and Eq constraints for all type variables of the result type.
addEqShowConstraints :: CQualTypeExpr -> CQualTypeExpr
addEqShowConstraints (CQualType (CContext clscons) texp) =
  CQualType (CContext (union (type2EqConstraints (resultType texp))
                             (union (type2ShowConstraints texp) clscons)))
            texp

-- adds contract checking to a function if it has a pre- or postcondition
addContract :: Options -> [(QName,Int)] -> [CFuncDecl] -> [CFuncDecl]
            -> [CFuncDecl] -> CFuncDecl -> [CFuncDecl]
addContract _ _ _ _ _ (CFunc _ _ _ _ _) =
  error "Internal error in addContract: CFunc occurred"
addContract opts funposs allfdecls predecls postdecls
            orgfdecl@(CmtFunc cmt qn@(m,f) ar vis qtype _) =
 let argvars     = map (\i -> (i,"x"++show i)) [1..ar]
     encapsSuf   = if withEncapsulate opts then "ND" else ""
     encaps fn n = if withEncapsulate opts then setFun n fn []
                                           else constF fn

     -- Textual comment about function source:
     fref      = string2ac $ "'" ++ f ++ "' (module " ++ m ++
                             maybe ")"
                                   (\l -> ", line " ++ show l ++ ")")
                                   (lookup qn funposs)

     -- call to observation function (if provided):
     obsfunexp = constF $
                  maybe (pre "id")
                       funcName
                       (find (\fd -> snd (funcName fd) == f++"'post'observe")
                             allfdecls)

     ctexp = addEqShowConstraints qtype
     fdecl = setTypeInFuncDecl ctexp orgfdecl

     -- Construct function with precondition added and a function without prec.:
     (precheck,woprefdecl) =
        maybe ([],fdecl)
          (\predecl ->
            let prename = funcName predecl
                rename = updateFunc id qn (withSuffix qn "'WithoutPreCondCheck")
            in ([cmtfunc cmt (m,f) ar vis ctexp
                   [simpleRule (map CPVar argvars)
                      (applyF (cMod $ "withPreContract" ++ show ar ++ encapsSuf)
                         ([fref, encaps prename ar, constF (rename qn)] ++
                          map CVar argvars))]],
                addCmtLine "Without precondition checking!" $
                           rnmFDecl rename fdecl))
          (find (\fd -> fromPreCondName (snd (funcName fd)) == f) predecls)

     -- Construct function with postcond. added and a function without postc.:
     (postcheck,wopostfdecl) =
        maybe ([],woprefdecl)
          (\postdecl ->
            let postname = funcName postdecl
                qnp      = funcName woprefdecl
                rename   = updateFunc id qnp
                                      (withSuffix qnp "'WithoutPostCondCheck")
            in ([cmtfunc (funcComment woprefdecl) qnp ar vis ctexp
                 [simpleRule (map CPVar argvars)
                    (applyF (cMod $ "withPostContract" ++ show ar ++ encapsSuf)
                      ([fref, encaps postname (ar+1), obsfunexp,
                        constF (rename qnp)] ++
                       map CVar argvars))]],
                 setPrivate $ addCmtLine "Without postcondition checking!" $
                              rnmFDecl rename woprefdecl))
          (find (\fd-> fromPostCondName (snd (funcName fd)) == f) postdecls)

     rnmFDecl rnm fdcl = if topLevelContracts opts
                           then updQNamesInCFuncDecl rnm fdcl
                           else renameFDecl rnm fdcl

  in precheck ++ postcheck ++ [wopostfdecl]

--- Updates a function at some point.
updateFunc :: Eq a => (a -> b) -> a -> b -> (a -> b)
updateFunc f x v y = if y==x then v else f y

--- Define a function as private.
setPrivate :: CFuncDecl -> CFuncDecl
setPrivate = updCFuncDecl id id id (const Private) id id

--- Sets the type of a function declaration.
setTypeInFuncDecl :: CQualTypeExpr -> CFuncDecl -> CFuncDecl
setTypeInFuncDecl texp = updCFuncDecl id id id id (const texp) id

--- Adds a suffix to  qualified name.
withSuffix :: QName -> String -> QName
withSuffix (m,f) s = (m, f ++ s)

------------------------------------------------------------------------
-- Auxiliary operations:

-- Replaces a result type of a function type by a new type
replaceResultType :: CTypeExpr -> CTypeExpr -> CTypeExpr
replaceResultType texp ntype =
  case texp of CFuncType t1 t2 -> CFuncType t1 (replaceResultType t2 ntype)
               _               -> ntype

-- Transform a n-ary function type into a (n+1)-ary function type with
-- a given new result type
extendFuncType :: CTypeExpr -> CTypeExpr -> CTypeExpr
extendFuncType t texp = case t of
  CFuncType t1 t2 -> t1 ~> (extendFuncType t2 texp)
  _               -> t ~> texp

--- Renames a function declaration (but not the body).
renameFDecl :: (QName -> QName) -> CFuncDecl -> CFuncDecl
renameFDecl rn (CFunc qn ar vis texp rules) = CFunc (rn qn) ar vis texp rules
renameFDecl rn (CmtFunc cmt qn ar vis texp rules) =
  CmtFunc cmt (rn qn) ar vis texp rules

--- Adds a line to the comment in a function declaration.
addCmtLine :: String -> CFuncDecl -> CFuncDecl
addCmtLine s (CFunc     qn ar vis texp rules) =
  CmtFunc s qn ar vis texp rules
addCmtLine s (CmtFunc cmt qn ar vis texp rules) =
  CmtFunc (if null cmt then s else unlines [cmt,s]) qn ar vis texp rules

--- Deletes the comment in a function declaration.
deleteCmt :: CFuncDecl -> CFuncDecl
deleteCmt (CFunc     qn ar vis texp rules) = CFunc qn ar vis texp rules
deleteCmt (CmtFunc _ qn ar vis texp rules) = CFunc qn ar vis texp rules

--- Deletes the comment in a function declaration if it is the empty string.
deleteCmtIfEmpty :: CFuncDecl -> CFuncDecl
deleteCmtIfEmpty (CFunc qn ar vis texp rules)     = CFunc qn ar vis texp rules
deleteCmtIfEmpty (CmtFunc cmt qn ar vis texp rules) =
  if null cmt then CFunc qn ar vis texp rules
              else CmtFunc cmt qn ar vis texp rules

------------------------------------------------------------------------
-- Compute names and lines numbers of all top-level operations in a program.
linesOfFDecls :: String -> CurryProg -> [(QName,Int)]
linesOfFDecls srctxt prog =
  map (addSourceLineNumber (map firstId (lines srctxt)))
      (map funcName (functions prog))
 where
  addSourceLineNumber ids qn = (qn, maybe 0 (+1) (elemIndex (snd qn) ids))

-- Compute the first identifier (name or operator in brackets) in a string:
firstId :: String -> String
firstId [] = ""
firstId (c:cs)
  | isAlpha c = takeWhile isIdChar (c:cs)
  | c == '('  = let bracketid = takeWhile (/=')') cs
                 in if all (`elem` infixIDs) bracketid
                    then bracketid
                    else ""
  | otherwise = ""

-- Is this an alphanumeric character, underscore, or apostroph?
isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_' || c == '\''

-- All characters occurring in infix operators.
infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

------------------------------------------------------------------------
-- Auxiliaries

-- An operation of the module Control.Search.SetFunctions:
sfMod :: String -> QName
sfMod f = (setFunMod,f)

-- Set function for a function name with given arity and arguments:
setFun :: Int -> QName -> [CExpr] -> CExpr
setFun n qn args = applyF (sfMod $ "set" ++ show n) (constF qn : args)

--- Name of the contract module.
contractMod :: String
contractMod = "Test.Contract"

-- An operation of the module Test.Contract:
cMod :: String -> QName
cMod f = (contractMod,f)

------------------------------------------------------------------------
