--- This module translates the abstract syntax tree
--- into a curry programm using the AbstractCurry libraries.
--- Errors are thrown for limitations caused by CDBI.
--- The translation is provided as a single line string as
--- required by currypp.
--- @author: Julia Krone
--- @version: 0.1
-- ---------------------------------------------------------

module SQLTranslator(translate) where

import AbstractCurry.Types
import AbstractCurry.Pretty
import AbstractCurry.Build
import Char        ( toLower, toUpper )
import List        ( intercalate, splitOn )
import Text.Pretty ( pPrint )

import ParseTypes

import SQLAst

-- module name needed for qualified names
mCDBI :: String
mCDBI = "Database.CDBI.ER"

--- Invokes the translation of the AST into a string of curry code 
--- in case a valid AST is given, does nothing otherwise.
translate :: PM[Statement] -> String -> String -> Pos -> PM String
translate (WM (Errors err) ws) _ _ _ = WM (throwPR err) ws
translate (WM (OK stats) ws) dbPath mModel pos = 
  let (WM resPR warns) = sequencePM (map (transStatement pos mModel) stats )
   in liftPM (writeFunction dbPath) (WM resPR (warns++ws))

-- The list of CExpr representing the statements is concatenated and pretty 
-- printed to obtain a single-line-translation line feeds are replaced by 
-- space characters and indentation is removed
writeFunction :: String -> [CExpr] -> String
writeFunction db stats = 
  let finExpr = (applyF (mCDBI, "runWithDB")
                        [(string2ac db), (concatStatements stats)]) 
      finStr = (pPrint (ppCExpr defaultOptions finExpr))  
      newLines = splitOn ['\n'] finStr
  in '(' : removeIndents (intercalate [' '] newLines) ++ ")"

removeIndents :: String -> String
removeIndents [] = []
removeIndents (s:str) = 
  case s of
     ' ' -> let (spaces, rest) = span (\c -> c == ' ') str 
             in s:(removeIndents rest)
     _   -> s:(removeIndents str)

concatStatements :: [CExpr] -> CExpr
concatStatements [] = CLambda [cpvar "conn"] 
                              (applyF (pre "return") 
                                      [applyE (CSymbol (pre "Right")) 
                                              [(CSymbol (pre "()"))]])
concatStatements [stat] = stat
concatStatements (s1:s2:stats) = applyF (mCDBI, ">+")
                                        [s1, (concatStatements (s2:stats))]

-- -----------------------------------translation -----------------------------
-- Call corresponding translation for each kind of statement
transStatement ::  Pos -> String -> Statement -> PM CExpr
transStatement p mModel (Select selhead order lim)  =
 bindPM  (transLimit lim)
         (transSelHead p mModel selhead order )
transStatement p mModel (Update tab assigns cond) = 
  combinePMs (\table valCond -> applyF (mCDBI, "updateEntries")(table:valCond))
             (transTableName mModel tab)
             (transUpdate p mModel assigns cond) 
transStatement _ mModel (UpdateEntity tab val) =
  liftPM (\table -> applyF (mCDBI, "updateEntry") 
                           [(transValue mModel val False), table])
         (transTableName mModel tab)
transStatement p mModel (Delete tab cond) = transDelete p mModel tab cond       
transStatement _ mModel (Insert tab cols valss) =      
  combinePMs (\func args -> applyF func args) 
             (getInsertfunction valss)
             (combinePMs (\d t -> [d,t])
                         (transInsertData mModel tab cols valss) 
                         (transTableName mModel tab))
transStatement p mModel (InTransaction sts) = transTransaction p mModel sts   
transStatement _ _ Transaction = 
        cleanPM (CLambda [cpvar "c"] 
                (applyF (pre "(>>)")
                        [(applyF (mCDBI, "begin") [cvar "c"]),
                         (applyF (pre "return") 
                                 [applyE (CSymbol (pre "Right")) 
                                         [(CSymbol (pre "()"))]])])) 
transStatement _ _ Commit = 
        cleanPM (CLambda [cpvar "c"] 
                (applyF (pre "(>>)")
                        [(applyF (mCDBI, "commit") [cvar "c"]),
                         (applyF (pre "return") 
                                 [applyE (CSymbol (pre "Right")) 
                                         [(CSymbol (pre "()"))]])])) 
transStatement _ _ Rollback = 
        cleanPM (CLambda [cpvar "c"] 
                (applyF (pre "(>>)")
                        [(applyF (mCDBI, "rollback") [cvar "c"]),
                         (applyF (pre "return") 
                                 [applyE (CSymbol (pre "Right")) 
                                         [(CSymbol (pre "()"))]])])) 

-- ---------------------transaction -------------------------
-- The translation of a transactional statement requires the concatenation
-- of staements with a monadic operator after the translation of
-- each single statement
transTransaction :: Pos -> String -> [Statement] -> PM CExpr 
transTransaction p _ []  = throwPM p ("Transaction without statements found.")
transTransaction p mModel [s] = transStatement p mModel s
transTransaction p mModel (s1:s2:sts) = 
       combinePMs (\trS1 trSts ->  applyF (mCDBI, ">+")
                                          [trS1, trSts])
                  (transStatement p mModel s1)
                  (transTransaction p mModel (s2:sts))

-- ----------------------select statement --------------------
-- The translation of select statements requires a distinction between
-- simple or compound statements and subsequently between single column
-- selection or complete table rows (entities).
transSelHead :: Pos -> String -> SelectHead -> Order -> CExpr -> PM CExpr
transSelHead p mModel q@(Query selClause tab cond gr) order lim =
  case selClause of
     (SelAll sp)      -> transToGetEnt p mModel sp tab cond gr order lim
     (SelColumns _ _) -> (combinePMs 
                            (\(fun, select) trOrd -> 
                                  applyF fun 
                                         ([(list2ac []), 
                                           (list2ac [select]), 
                                           trOrd,
                                           lim]))
                            (combinePMs (,)
                                        (getColFunc p q)
                                        (transToGetCol p mModel q))
                            (transOrder p mModel order))
transSelHead p mModel (Set op head1 head2) order lim = 
   let setops = list2ac (getSetOpList op head2)
   in combinePMs (\fun (trHeads, trOrd) -> 
                      applyF fun ([setops, (list2ac trHeads), trOrd, lim]))
                 (getColFunc p head1)               
                 (combinePMs (,)
                             (transSetParts p mModel head1 head2)
                             (transOrder p mModel order))

getSetOpList :: ASetOp -> SelectHead -> [CExpr]
getSetOpList op (Query _ _ _ _) = [(transSetOp op)]
getSetOpList op (Set nextOp _ head2) =
   ((transSetOp op):(getSetOpList nextOp head2)) 

transSetOp :: ASetOp -> CExpr
transSetOp AUnion = (CSymbol (mCDBI, "Union"))
transSetOp AExcept = (CSymbol (mCDBI, "Except"))  
transSetOp AIntersect = (CSymbol (mCDBI, "Intersect"))   

-- Further set operations can just appear in the second selectHead.
transSetParts :: Pos -> String -> SelectHead -> SelectHead -> PM [CExpr]
transSetParts p mModel head1 head2@(Query _ _ _ _) =
   combinePMs (\h1 h2 -> [h1, h2])
              (transToGetCol p mModel head1)
              (transToGetCol p mModel head2)
transSetParts p mModel head1 (Set _ h1 h2) =
  combinePMs (:)
             (transToGetCol p mModel head1)
             (transSetParts p mModel h1 h2)  


transSp :: ASpecifier -> CExpr
transSp AAll = CSymbol (mCDBI, "All")
transSp ADistinct = CSymbol (mCDBI, "Distinct")

-- tranlsation function for selection of one or more complete entities.
transToGetEnt :: Pos -> 
                 String ->
                 ASpecifier -> 
                 TableRef -> 
                 Condition -> 
                 (Maybe Group) ->
                 Order -> 
                 CExpr -> 
                 PM CExpr
transToGetEnt p mModel sp tab cond gr order lim =
  case tab of
     (TableRef t Nothing)  -> 
            combinePMs (\table condOrd -> 
                             (applyF (mCDBI, "getEntries")
                                     ([(transSp sp), table]++condOrd++[lim])))
                       (transTableName mModel t)
                       (combinePMs (\trCond trOrd -> trCond++[trOrd])
                                   (transSelCond p mModel cond gr)
                                   (transOrder p mModel order))                                         
     (TableRef _ (Just _)) -> 
            combinePMs (\cdsNJoin trCond ->
                            (applyF (mCDBI, "getEntriesCombined") 
                                    ([(transSp sp)]++cdsNJoin++trCond++[lim])))
                       (getCDJoin p mModel tab)
                       (combinePMs (\trCond trOrd -> trCond ++[trOrd])
                                   (transSelCond p mModel cond gr)
                                   (transOrder p mModel order)) 


getCDJoin :: Pos -> String -> TableRef -> PM [CExpr]
getCDJoin p mModel (TableRef tab join) =  
  combinePMs (\ trJoin trCD -> trCD++[trJoin]) 
             (getJoins p mModel join)
             (getCombDesc p mModel tab join)

-- Throws error in case more than three entities are combined which
-- is not supported yet, but can easily be added according to the translation
-- of two and three entities given below.
getCombDesc :: Pos -> String -> Table -> (Maybe JoinClause) -> PM [CExpr]
getCombDesc _ _ _ Nothing = cleanPM []
getCombDesc p mModel tab (Just join) = 
    let tabs = tab:(countTabs (Just join))
    in case length tabs of
            2  -> getTwoEntStr mModel tabs
            3  -> getThreeEntStr mModel tabs
            _  -> throwPM p ("This number of joined tables is not supported. "
                             ++"Reduce number of tables or select up to "
                             ++"5 columns and join an arbitrary number of "
                             ++"tables.")

countTabs :: (Maybe JoinClause) -> [Table]
countTabs Nothing = []
countTabs (Just (CrossJoin tab join)) = tab:(countTabs join)
countTabs (Just (InnerJoin tab _ join)) = tab:(countTabs join)

-- Translation of the combination of two entity types.
getTwoEntStr :: String -> [Table] -> PM [CExpr]
getTwoEntStr mModel  ((Table name1 _ al1):(Table name2 _ al2):_)=
     (cleanPM [(applyF
                (mCDBI, "combineDescriptions") 
                [(constF (mModel, entity2Description name1)),
                 (cvar(show al1)),
                 (constF (mModel, entity2Description name2)),
                 (cvar (show al2)),
                 (CLambda [(cpvar "e1"), (cpvar "e2")] 
                          (applyF (pre "(,)") [(cvar "e1"), (cvar "e2")])),
                 (constF (pre "id"))])])

-- Translation of the combination of three entity types.
getThreeEntStr :: String -> [Table] -> PM [CExpr]
getThreeEntStr mModel ((Table name1 _ al1):(Table name2 _ al2):(Table name3 _ al3):_)=
  (cleanPM 
    [(applyF (mCDBI, "addDescription")
             [(constF (mModel, entity2Description name3)),
              (cvar(show al3)),
              (CLambda [(cpvar "e3"),
                        (tuplePattern [(cpvar "e1"), 
                                       (cpvar "e2"),
                                       (cpvar "_")])]
                        (applyF (pre "(,,)") [(cvar "e1"), 
                                              (cvar "e2"), 
                                              (cvar "e3")])),
              (CLambda [(tuplePattern [(cpvar "_"), 
                                       (cpvar "_"), 
                                       (cpvar "e3")])]
                       (cvar "e3")),
              (applyF (mCDBI, "combineDescriptions") 
                      [(constF (mModel, entity2Description name1)),
                       (cvar(show al1)),
                       (constF (mModel, entity2Description name2)),
                       (cvar(show al2)),
                       (CLambda [(cpvar "e1"), (cpvar "e2")]
                                (applyF (pre "(,,)") [(cvar "e1"), 
                                                      (cvar "e2"), 
                                                      (cvar "_")])),
                       (CLambda [(tuplePattern [(cpvar "e1"), (cpvar "e2"), (cpvar "_")])]
                                (applyF (pre "(,,)") [(cvar "e1"), (cvar "e2")]))])])])

getJoins :: Pos -> String -> (Maybe JoinClause) -> PM CExpr
getJoins p mModel join = liftPM (\joinList -> list2ac joinList)
                                (getNextJoin p mModel join)  

getNextJoin :: Pos -> String -> (Maybe JoinClause) -> PM [CExpr]
getNextJoin _ _ Nothing = cleanPM []
getNextJoin p mModel (Just (CrossJoin _ join)) = 
    combinePMs (:) (cleanPM (constF (mCDBI, "crossJoin")))
                   (getNextJoin p mModel join)
getNextJoin p mModel (Just(InnerJoin _ cond join)) = 
    combinePMs (:) (getInnerJoin p mModel cond)
                   (getNextJoin p mModel join)

getInnerJoin :: Pos -> String -> JoinCond -> PM CExpr
getInnerJoin p mModel (JC cond) = 
    liftPM (\trCond -> (applyF (mCDBI, "innerJoin") [trCond]))
           (transCond p mModel cond)

-- Returns correct function name according to number of columns.
-- Returns an error for more than five columns or compound selects
-- for complete entity cause neither is supported by CDBI.
getColFunc :: Pos -> SelectHead -> PM (String, String)
getColFunc p (Query (SelColumns _ elems) _ _ _) =
    case length elems of
         1 -> cleanPM (mCDBI, "getColumn")
         2 -> cleanPM (mCDBI, "getColumnTuple")
         3 -> cleanPM (mCDBI, "getColumnTriple")
         4 -> cleanPM (mCDBI, "getColumnFourTuple")
         5 -> cleanPM (mCDBI, "getColumnFiveTuple")
         _ -> throwPM p ("The Selection of more than five Columns are not"
                          ++" supported by the CDBI-Interface.")
getColFunc p (Query (SelAll _) _ _ _) =
  throwPM p ("The Combination of '*' and Setoperators is not supported.")
getColFunc p (Set _ _ _) =
  throwPM p ("Internal parsing error: this should not happen")

-- Returns construction of datatypes for selection of single columns
-- according to number of columns.
-- Returns an error for more than five columns or compound selects
-- for complete entity cause neither is supported by CDBI.
transToGetCol ::  Pos -> String -> SelectHead -> PM CExpr
transToGetCol p mModel q@(Query (SelColumns _ es) _ _ _) =
  case (length es) of
     1 -> transToSingleCol p mModel q 
     2 -> transToTupleCol p mModel q 
     3 -> transToTripleCol p mModel q 
     4 -> transToFourTCol p mModel q 
     5 -> transToFiveTCol p mModel q 
     _ ->  throwPM p ("The Selection of more than five Columns are not"
                          ++" supported by the CDBI-Interface.")
transToGetCol p _ (Query (SelAll _) _ _ _) = 
    throwPM p ("The Combination of '*' and Setoperators is not supported.")
transToGetCol p _ (Set _ _ _ ) = throwPM p ("The Combination of '*' "++
                                               "and Setoperators is "++
                                               "not supported.") 

-- Translation to SingleColumnSelect.
transToSingleCol :: Pos -> String -> SelectHead -> PM CExpr
transToSingleCol p mModel (Query (SelColumns sp es) tab cond gr) = 
  combinePMs (\sc (trTabs, trCond) -> 
                    (applyE (CSymbol (mCDBI, "SingleCS"))
                            ([(transSp sp), sc, trTabs]++trCond)))
             (transColSingleCol p mModel (head es))  
             (combinePMs (,)
                         (transTabsNJoins p mModel tab)
                         (transSelCond p mModel cond gr))

-- Translation to TupleColumnSelect.
transToTupleCol :: Pos -> String -> SelectHead -> PM CExpr
transToTupleCol p mModel (Query (SelColumns sp es) tab cond gr) =
  combinePMs (\tupleCol (trTab, trCond) -> 
                   applyE (CSymbol (mCDBI, "TupleCS"))
                          ([(transSp sp), tupleCol, trTab]++trCond))
             (transColumnCol p mModel "tupleCol" es )
             (combinePMs (,)
                         (transTabsNJoins p mModel tab)  
                         (transSelCond p mModel cond gr)) 

-- Translation to TripleColumnSelect.
transToTripleCol :: Pos -> String -> SelectHead -> PM CExpr
transToTripleCol p mModel (Query (SelColumns sp es) tab cond gr)= 
  combinePMs (\tupleCol (trTab, trCond) -> 
                   applyE (CSymbol (mCDBI, "TripleCS"))
                          ([(transSp sp), tupleCol, trTab]++trCond))
             (transColumnCol p mModel "tripleCol" es)
             (combinePMs (,)
                         (transTabsNJoins p mModel tab)  
                         (transSelCond p mModel cond gr)) 

-- Translation to FourColumnSelect.
transToFourTCol :: Pos -> String -> SelectHead -> PM CExpr
transToFourTCol p mModel (Query (SelColumns sp es) tab cond gr) = 
   combinePMs (\tupleCol (trTab, trCond) -> 
                    applyE (CSymbol (mCDBI, "FourCS"))
                           ([(transSp sp), tupleCol, trTab]++trCond))
             (transColumnCol p mModel "fourCol" es)
             (combinePMs (,)
                         (transTabsNJoins p mModel tab)  
                         (transSelCond p mModel cond gr)) 

-- Translation to FiveColumnSelect.
transToFiveTCol :: Pos -> String -> SelectHead -> PM CExpr
transToFiveTCol p mModel (Query (SelColumns sp es) tab cond gr) = 
  combinePMs (\tupleCol (trTab, trCond) -> 
                  applyE (CSymbol (mCDBI, "FiveCS"))
                         ([(transSp sp), tupleCol, trTab]++trCond))
             (transColumnCol p mModel "fiveCol" es)
             (combinePMs (,)
                         (transTabsNJoins p mModel tab)  
                         (transSelCond p mModel cond gr)) 


-- Translation function for ColumnCollection type for all arities
-- based on function for ColumnSingleCollection.
transColumnCol :: Pos -> String -> String -> [SelElement] -> PM CExpr
transColumnCol p mModel constr es =
 liftPM (\elems -> applyF (mCDBI, constr) elems)
        (sequencePM (map (transColSingleCol p mModel) es))
                     

-- Translation to a ColumnSingleCollection.
transColSingleCol :: Pos -> String -> SelElement -> PM CExpr
transColSingleCol p mModel (Col col)  = 
         (transToColDesc p mModel col (constF (mCDBI, "none")))
transColSingleCol p mModel (Aggregation fun sp col) =
         (transToColDesc p mModel col (transFun fun sp))
transColSingleCol p mModel (Case cond val1 val2) = 
  combinePMs (\c ((tv1,tv2), fun) -> 
                 applyF (mCDBI, "caseThen") 
                        [(applyF (mCDBI, "condition") [c]), tv1, tv2, fun])
             (transCond p mModel cond)
             (combinePMs (,) 
                         (combinePMs (,)
                                    (transOperand p mModel val1)
                                    (transOperand p mModel val2))
                         (valTypeNFun p val1 val2))

transToColDesc :: Pos -> String -> ColumnRef -> CExpr -> PM CExpr
transToColDesc _ mModel (Column (Unique tab) col _ _ al) fun =
  cleanPM (applyF (mCDBI, "singleCol")
                  [(constF (mModel, ((firstLow tab)++col++"ColDesc"))),
                   (cvar (show al)),
                   fun])
-- should not happen at this stage anymore
transToColDesc p _ (Column (Def _) col _ _ _) _ =
  throwPM p ("Translator: Column "++col++" could not be related to any table.")

transFun :: AFun -> ASpecifier -> CExpr
transFun ASum sp = applyF (mCDBI, "sum") [(transSp sp)]
transFun AAvg sp = applyF (mCDBI, "avg") [(transSp sp)]
transFun ACount sp = applyF (mCDBI, "count") [(transSp sp)]
transFun AMin _ = constF (mCDBI, "minV")
transFun AMax _ = constF (mCDBI, "maxV")

valTypeNFun :: Pos -> Operand -> Operand -> PM CExpr
valTypeNFun p (Right (IntExp _ )) v2 = 
  case v2 of
   (Left (Column _ _ (Key _) _ _)) -> throwPM p ("Key columns are not allowed "
                                                 ++"in case-expressions due "
                                                 ++"to restrictions of the CDBI"
                                                 ++" interface.")
   _                             -> cleanPM (constF (mCDBI, "caseResultInt"))
valTypeNFun _ (Right (FloatExp _)) _= cleanPM 
                                          (constF (mCDBI, "caseResultFloat"))
valTypeNFun _ (Right (StringExp _)) _ = cleanPM 
                                          (constF (mCDBI, "caseResultString"))
valTypeNFun _ (Right (DateExp _)) _ = cleanPM (constF (mCDBI,"caseResultDate"))
valTypeNFun _ (Right (BoolExp _)) _ = cleanPM (constF (mCDBI,"caseResultBool")) 
valTypeNFun _ (Right (CharExp _)) _ = cleanPM (constF (mCDBI,"caseResultChar"))
valTypeNFun p (Right (KeyExp _ _)) _ = 
    throwPM p ("Key columns are not allowed in case-expressions"++
                            " due to restrictions of the CDBI interface.")
valTypeNFun p (Right (Emb _ typ)) v2 = 
 case v2 of
   (Left (Column _ _ (Key _) _ _)) -> throwPM p ("Key columns are not allowed "
                                                 ++"in case-expressions due "
                                                 ++"to restrictions of the CDBI"
                                                 ++" interface.")
   _                             -> cleanPM 
                                     (constF (mCDBI, 
                                              ("caseResult"++(transTyp typ)))) 
valTypeNFun p (Right AbsNull) _ = throwPM p ("This should not happen, because"
                                              ++" preprocessing was already "
                                              ++"stopped.")
valTypeNFun p (Left (Column _ _ typ _ _)) _ = 
  case typ of
     (Key _) -> throwPM p ("Key columns are not allowed in case-expressions"++
                            " due to restrictions of the CDBI interface.")
     _       -> cleanPM (constF (mCDBI, ("caseResult"++(transTyp typ))))

-- Translation to TC type.
transTabsNJoins :: Pos -> String -> TableRef -> PM CExpr
transTabsNJoins p mModel  (TableRef tab join)=
  liftPM (\trTab -> applyE (CSymbol (mCDBI, "TC")) 
                           ((transTab mModel tab)++[trTab]))
         (transTables p mModel join)

transTables :: Pos -> String -> (Maybe JoinClause) -> PM CExpr
transTables _ _ Nothing = cleanPM (CSymbol (pre "Nothing"))
transTables p mModel (Just (CrossJoin tab join)) = 
     liftPM (\trJoin -> 
                applyE (CSymbol (pre "Just"))
                       [applyF (pre "(,)") 
                               [(constF (mCDBI, "crossJoin")),
                                (applyE (CSymbol (mCDBI, "TC"))
                                        ((transTab mModel tab)++[trJoin]))]])
            (transTables p mModel join)
transTables p mModel (Just (InnerJoin tab cond join)) =
     combinePMs (\trCond trJoin -> 
                      applyE (CSymbol (pre "Just"))
                             [applyF (pre "(,)") 
                                     [trCond, 
                                      (applyE (CSymbol (mCDBI, "TC")) 
                                      ((transTab mModel tab) ++[trJoin]))]]) 
                (getInnerJoin p mModel cond)
                (transTables p mModel join )  

      
transTab :: String -> Table -> [CExpr]
transTab mModel (Table name _ alias) = 
  [(constF (mModel, ((firstLow name)++"Table"))),
           (cvar (show alias))]

-- Translation to Criteria data type.
transSelCond :: Pos -> String -> Condition -> (Maybe Group) -> PM [CExpr]
transSelCond p mModel cond gr =
  combinePMs (\c trGr -> [applyE (CSymbol (mCDBI, "Criteria")) [c, trGr]])
             (transCond p mModel cond)
             (transGroup p mModel gr)

-- Translation for Group-by-clause.
transGroup :: Pos -> String -> (Maybe Group) -> PM CExpr
transGroup _ _  Nothing = cleanPM (CSymbol (pre "Nothing"))                                    
transGroup p mModel (Just (GroupBy cols hav)) = 
                            (transGroupBy p mModel cols hav)

transGroupBy :: Pos -> String -> [ColumnRef] -> Having -> PM CExpr
transGroupBy _ _ [] _ = cleanPM (CSymbol (pre "Nothing")) 
transGroupBy p mModel (c:cs) hav = 
          combinePMs (\col gbt -> (applyE (CSymbol (pre "Just")) 
                                          [applyF (mCDBI, "groupBy") 
                                                  [col, gbt]]))
                     (transColumn p mModel c)
                     (transGroupByTail p mModel cs hav)

transGroupByTail :: Pos -> String -> [ColumnRef] -> Having -> PM CExpr
transGroupByTail p mModel [] hav = transHaving p mModel hav 
transGroupByTail p mModel (c:cs) hav = 
      combinePMs (\col gbt -> (applyF (mCDBI, "groupByCol") 
                                       [col, gbt]))
                 (transColumn p mModel c)
                 (transGroupByTail p mModel cs hav)

-- Translation for having-clause.
transHaving :: Pos -> String -> Having -> PM CExpr
transHaving p mModel have = 
     case have of 
         NoHave -> cleanPM (constF (mCDBI, "noHave"))
         _      -> liftPM (\cond -> (applyF (mCDBI, "having") [cond]))
                          (transHavCond p mModel have)

transHavCond :: Pos -> String -> Having -> PM CExpr
transHavCond p mModel (SimpleHave cond) = 
    liftPM (\c -> (applyF (mCDBI, "condition") [c]))
           (transCond p mModel cond)
transHavCond p mModel (Neg hav) = 
    liftPM (\c -> (applyE (CSymbol (mCDBI, "Neg")) [c]))
           (transHavCond p mModel hav)
transHavCond p mModel (CmpHave op hav1 hav2) = 
   combinePMs (\ trHav1 trHav2 -> 
                   (applyE (CSymbol (mCDBI, (transLogForHav op)))
                           [list2ac [trHav1, trHav2 ]]))
              (transHavCond p mModel hav1)
              (transHavCond p mModel hav2)
transHavCond p mModel (AggrHave fun sp op1 bop op2) =
  let trfun = case fun of
        ASum   -> getSumFun op1 
        AAvg   -> getAvgFun op1 
        ACount -> (mCDBI, "countCol")
        AMin   -> (mCDBI, "minCol")
        AMax   -> (mCDBI, "maxCol")
  in combinePMs (\(col, operand) binOp -> 
                     (applyF trfun [(transSp sp),
                                     col, 
                                     operand,
                                    (constF binOp)]))
                (combinePMs (,) (transColumn p mModel op1) 
                                (transOperand p mModel op2))
                (transBinOp bop)
  where getSumFun (Column _ _ I _ _) = (mCDBI, "sumIntCol")
        getSumFun (Column _ _ F _ _) = (mCDBI, "sumFloatCol")
        getAvgFun (Column _ _ I _ _) = (mCDBI, "avgIntCol")
        getAvgFun (Column _ _ F _ _) = (mCDBI, "avgFloatCol")
        

transLogForHav :: ALogOp -> String
transLogForHav AAnd = "HAnd"
transLogForHav AOr  = "Or"

-- Translation for order-by-clause.
transOrder :: Pos -> String -> Order -> PM CExpr
transOrder _ _ (OrderBy []) = cleanPM (list2ac [])
transOrder p mModel (OrderBy (cd:colDirs)) = 
  (liftPM (\ords -> list2ac ords)
          (sequencePM (map (transColDir p mModel) (cd:colDirs))))

transColDir :: Pos -> String -> (ColumnRef, Dir) -> PM CExpr
transColDir p mModel (col , Asc) = 
      liftPM (\c -> applyF (mCDBI, "ascOrder") [c])
             (transColumn p mModel col)
transColDir p mModel (col , Desc) = 
    liftPM (\c -> applyF (mCDBI, "descOrder") [c])
           (transColumn p mModel col)

--Translation for Limit-clause.
transLimit :: (Maybe Int) -> PM CExpr
transLimit Nothing = cleanPM (CSymbol (pre "Nothing"))
transLimit (Just lim) = cleanPM (applyE (CSymbol (pre "Just")) 
                                        [(cvar(show lim))])

-- -----------------------update statement -------------------

-- Translation of update statement.
transUpdate :: Pos -> String -> [Assign] -> Condition -> PM [CExpr]
transUpdate p mModel assigns cond =
  combinePMs (\ trAss trCond -> [trAss, trCond])
             (transAssigns p mModel assigns)
             (transCond p mModel cond)

transAssigns :: Pos -> String -> [Assign] -> PM CExpr
transAssigns p mModel assigns = 
  liftPM (\trAss -> list2ac trAss)
         (sequencePM (map (transAssign p mModel) assigns))
           

transAssign :: Pos -> String -> Assign -> PM CExpr
transAssign p mModel (Assign col val) =
   combinePMs (\trVal trCol-> applyF (mCDBI, "colVal") [trCol, trVal])
              (transCondValue p mModel val )
              (transUpdColumn p mModel col)

-- Columns have to be translated differently than in conditions or select.              
transUpdColumn :: Pos -> String -> ColumnRef -> PM CExpr
transUpdColumn _ mModel (Column (Unique tab) col _ _ _) = 
               cleanPM (constF (mModel, ((firstLow tab)++"Column"++col)))
-- this should not happen at this stage anymore
transUpdColumn p _ (Column (Def _) col _ _ _) = 
                    throwPM p ("Column "++col++" could not be related "
                                ++"to any table.")             
 
  
-- ------------------------delete statement -------------------
-- Translation of delete statement.
transDelete :: Pos -> String -> Table -> Condition -> PM CExpr
transDelete p mModel tab cond =  
   combinePMs (\table trCond -> (applyF (mCDBI, "deleteEntries") 
                                      [table, trCond]))
                          (transTableName mModel tab )
                          (transMaybeCond p mModel cond)
                   
-- In delete statements the condition is a maybe-Value.                   
transMaybeCond :: Pos -> String -> Condition -> PM CExpr
transMaybeCond p mModel cond =
  case cond of
       NoCond   -> cleanPM (CSymbol (pre "Nothing"))
       _        -> liftPM (\c -> applyE (CSymbol (pre "Just")) [c])
                          (transCond p mModel cond)

-- ----------------------- insert statement -------------------

-- Depending on the number of given lists of values
-- there are different CDBI functions to apply.
getInsertfunction :: [[Value]] -> PM (String, String)
getInsertfunction valss =
  if length valss > 1
    then cleanPM (mCDBI, "insertEntries") 
    else cleanPM (mCDBI, "insertEntry")

-- Translation of list of lists of values.
transInsertData :: String -> Table ->  [ColumnRef] -> [[Value]] -> PM CExpr
transInsertData mModel (Table tab _ _) cols valss= 
  let entities = (map (transEntity mModel tab cols) valss)
  in if length entities == 1
       then cleanPM (head entities)
       else cleanPM (list2ac entities)

-- Distinguishes between embedded expression or list with values.
-- In latter case apply constructor of entity.
transEntity :: String -> String -> [ColumnRef] -> [Value] -> CExpr
transEntity mModel tab cols vals =
  case vals of
    [(Emb exp _)] -> (cvar exp) 
    _             -> applyE (CSymbol (mModel, (firstUp tab))) 
                            (transInsertValues mModel cols vals)

-- Translates list of values to insert.    
transInsertValues :: String -> [ColumnRef] -> [Value] -> [CExpr]   
transInsertValues _ [] _ = []
transInsertValues _ (_:_) [] = []
transInsertValues mModel ((Column _ _ _ nl _):cs) (v:vs) = 
    ((transValue mModel v nl):(transInsertValues mModel cs vs))                        

-- ----------------------- common elements --------------------

-- Traslation of table name as used in update, insert and delete.
transTableName :: String -> Table -> PM CExpr
transTableName mModel (Table tab _ _) = 
       cleanPM (constF (mModel, entity2Description tab)) 

-- Translation of values as used in insert statements.
transValue :: String -> Value -> Bool -> CExpr
transValue _ (Emb exp _) nullable = 
    if nullable 
       then applyE (CSymbol(pre "Just")) 
                   [(cvar ("("++exp++")"))]
       else (cvar ("("++exp++")"))
transValue _ (IntExp int) nullable = 
    if nullable 
       then applyE (CSymbol(pre "Just")) 
                   [(cvar (show int))]
       else (cvar (show int))
transValue _ (FloatExp float) nullable = 
    if nullable 
       then applyE (CSymbol (pre "Just"))
                   [(cvar(show float))]
       else (cvar(show float))
transValue _ (StringExp string) nullable = 
    if nullable 
       then applyE (CSymbol (pre "Just"))
                   [string2ac string]
       else string2ac string
transValue _ (DateExp date) nullable = 
    if nullable 
       then applyE (CSymbol (pre "Just")) 
                   [(applyF ("Time", "toClockTime")
                            [(cvar (show date))])]
       else applyF ("Time", "toClockTime")
                   [(cvar (show date))]
transValue _ (BoolExp bool) nullable = 
    if nullable 
       then applyE (CSymbol (pre "Just"))
                   [(cvar (show bool))]
       else (cvar (show bool))
transValue _ (CharExp char) nullable = 
    if nullable 
       then applyE (CSymbol (pre "Just"))
                   [(cvar (show char))]
       else (cvar (show char))
transValue mModel (KeyExp tab int) nullable = 
  if nullable then applyE (CSymbol (pre "Just")) 
                          [(applyE (CSymbol (mModel, ((firstUp tab)++"ID")))
                                   [(cvar(show int))])]
              else (applyE (CSymbol (mModel, ((firstUp tab)++"ID")))
                           [(cvar(show int))])
transValue _ AbsNull _ = CSymbol (pre "Nothing")
       
-- Translation of values as used in conditions, update and case-expressions.
transCondValue :: Pos -> String -> Value ->  PM CExpr
transCondValue _  mModel (Emb exp typ) = 
 let mModule = case typ of
                (Key _) -> mModel
                _       -> mCDBI
  in cleanPM (applyF (mModule, (firstLow (transTyp typ)))
                     [cvar ("("++exp++")")])
transCondValue _ _ (IntExp int) = 
     cleanPM (applyF (mCDBI, "int") [cvar (show int)])
transCondValue _ _ (FloatExp float) = 
     cleanPM (applyF (mCDBI, "float") [cvar (show float)])
transCondValue _ _ (StringExp str) = 
     cleanPM (applyF (mCDBI, "string") [string2ac str])
transCondValue _ _ (DateExp date) = 
     cleanPM (applyF (mCDBI, "date") 
             [applyF ("Time", "toClockTime") 
                     [cvar (show date)]])                                                                     
transCondValue _ _ (BoolExp bool) = 
     cleanPM (applyF (mCDBI, "bool") [cvar (show bool)])
transCondValue _ _ (CharExp char) = 
     cleanPM (applyF (mCDBI, "char") [cvar (show char)])
transCondValue _ mModel (KeyExp tab i) = 
     cleanPM (applyF (mModel, ((firstLow tab)++"ID"))
             [applyE (CSymbol (mModel, ((firstUp tab)++"ID"))) 
                     [(cvar (show i))]])
transCondValue p _ AbsNull = throwPM p ("This should not happen, cause "
                                         ++"preprocessing was already"++
                                         " stopped after Consistency Check.")       

-- Translation of condition-clause, finally used for all statements.
transCond :: Pos -> String -> Condition -> PM CExpr
transCond p mModel (FK (tab1, al1) rel (tab2, al2)) = 
  getConstraint p mModel rel (firstLow tab1) al1 (firstLow tab2) al2                                             
transCond p mModel (Cmp logop cond1 cond2) =
  combinePMs (\trLogop conds -> applyE trLogop [list2ac conds])
             (transLogOp logop)
             (combinePMs (\tcond1 tcond2 -> [tcond1,tcond2]) 
                         (transCond p mModel cond1)
                         (transCond p mModel cond2))
transCond  p mModel (Not cond)= 
     liftPM (\trcond -> applyE (CSymbol (mCDBI, "Not")) [trcond])
            (transCond p mModel cond)
transCond p mModel (Exists stat) = transSubquery p mModel stat
transCond p mModel (IsNull op) = liftPM (\trop -> applyF (mCDBI, "isNull") 
                                                         [trop])
                                        (transOperand p mModel op)
transCond p mModel (NotNull op) = liftPM (\trop -> applyF (mCDBI, "isNotNull")
                                                          [trop])
                                         (transOperand p mModel op)  
transCond p mModel (AIn op vals) = 
    combinePMs (\ trop trvals -> applyF (mCDBI,"isIn")  [trop, trvals] )
               (transOperand p mModel op)
               (transValList p mModel vals)
transCond p mModel (ABinOp bop op1 op2) = 
    combinePMs (\trBop (top1, top2) -> applyE (CSymbol trBop) [top1, top2] )
               (transBinOp bop)
               (combinePMs (,)
                           (transOperand p mModel op1) 
                           (transOperand p mModel op2))
transCond p mModel (ABetween op1 op2 op3) = 
    combinePMs (\ top1 top23 -> (applyE (CSymbol (mCDBI, "between")) 
                                        (top1:top23)))
               (transOperand p mModel op1)
               (combinePMs (\ o1 o2 -> [o1,o2])
                           (transOperand p mModel op2)
                           (transOperand p mModel op3))
transCond _ _ NoCond = cleanPM (CSymbol (mCDBI, "None"))

-- Translation of subquery for Exists-constraint.
transSubquery :: Pos -> String -> Statement -> PM CExpr
transSubquery p mModel (Select selHead order limit) = 
 case limit of
   Nothing -> combinePMs (\headStr _ -> applyE (CSymbol (mCDBI, "Exists")) 
                                               headStr)
                         (transHeadForSub p mModel selHead)
                         (transOrderForSub p order)
   (Just _ ) -> throwPM p ("Found limit inside an exists constraint.")

transHeadForSub :: Pos -> String -> SelectHead -> PM [CExpr]
transHeadForSub p mModel (Query _ tab cond gr) =
  case gr of
       (Just _)  -> throwPM p ("Group-Statements inside exists-constraints are"
                                ++ " not supported by the CDBI-Interface")
       Nothing   -> combinePMs (\ trTab trCond -> trTab++[trCond])
                               (transTableForSub p mModel tab)
                               (transCond p mModel cond)
transHeadForSub p _ (Set _ _ _) = throwPM p ("Set-operations inside an exists-"
                                              ++"constraint are not supported"
                                              ++ " by the CDBI-Interface")

transOrderForSub :: Pos -> Order -> PM Order
transOrderForSub _ (OrderBy []) = cleanPM (OrderBy [])
transOrderForSub p (OrderBy (_:_)) = throwPM p ("Found OrderBy inside an "
                                                 ++"exists-constraint.")

transTableForSub :: Pos -> String -> TableRef -> PM [CExpr]
transTableForSub _ mModel (TableRef (Table name _ alias) Nothing) = 
  cleanPM [(constF (mModel, (firstLow name)++"Table")), (cvar(show alias))]
transTableForSub p _ (TableRef _ (Just _)) = 
     throwPM p ("More than one Table inside exists-"
                 ++"constraint is not supported by "
                 ++"the CDBI-Interface")

-- Translation of Satisfies constraint into normal foreign key constraint
-- in condition clauses. 
getConstraint :: Pos -> String -> AbsRel -> String -> Int -> String -> Int 
                                                               -> PM CExpr
getConstraint p mModel rel tab1 al1 tab2 al2 =
  case rel of
    (AOneToN relName) -> 
       cleanPM  
         (applyF (mCDBI, "equal")                         
                 [(applyF (mCDBI, "colNum") 
                          [(constF (mModel, (tab1++"ColumnKey"))), 
                           (cvar (show al1))]),
                  (applyF (mCDBI, "colNum") 
                          [(constF (mModel, (tab2++"Column"++(firstUp tab1)
                                             ++relName++"Key"))),
                           (cvar(show al2))])])
    (ANToOne relName) ->
       cleanPM
         (applyF (mCDBI, "equal")
                 [(applyF (mCDBI, "colNum") 
                          [(constF (mModel ,(tab2++"ColumnKey"))),
                           (cvar (show al2))]),
                  (applyF (mCDBI, "colNum") 
                          [(constF (mModel, (tab1++"Column"++(firstUp tab2)
                                               ++relName++"Key"))),
                           (cvar (show al1))])])
    (AMToN relName) -> 
       cleanPM
         (applyE 
           (CSymbol (mCDBI, "Exists"))
           [(constF (mModel, (firstLow relName)++"Table")),
            (cvar "0"),
            (applyE (CSymbol (mCDBI, "And"))
                    [list2ac
                      [(applyF (mCDBI, "equal")
                               [(applyF (mCDBI, "col") 
                                        [(constF (mModel, ((firstLow relName)
                                                            ++"Column"++(firstUp tab1)
                                                            ++relName++"Key")))]),
                                (applyF (mCDBI, "colNum") 
                                        [(constF (mModel,(tab1++"ColumnKey"))), 
                                         (cvar (show al1))])]),
                       (applyF (mCDBI, "equal")
                               [(applyF (mCDBI, "col") 
                                        [(constF (mModel, ((firstLow relName)++
                                                            "Column"++(firstUp tab2)
                                                            ++relName++"Key")))]), 
                                (applyF (mCDBI, "colNum") 
                                        [(constF (mModel,(tab2++"ColumnKey"))),
                                         (cvar (show al2))])])]])])
    (NotSpec relName)   -> throwPM p ("Internal Error: Relation: " 
                                        ++relName++" could not be resoled.")

transLogOp :: ALogOp ->  PM CExpr
transLogOp AAnd = cleanPM (CSymbol (mCDBI, "And"))
transLogOp AOr  = cleanPM (CSymbol (mCDBI, "Or"))

transOperand :: Pos -> String -> Operand -> PM CExpr
transOperand p mModel (Left col) = transColumn p mModel col 
transOperand p mModel (Right val) = transCondValue p mModel val 

-- Translation of Columns for all but the update statement.
transColumn :: Pos -> String -> ColumnRef -> PM CExpr
transColumn _ mModel (Column (Unique tab) col _ _ al) = 
      cleanPM (applyF (mCDBI, "colNum") 
                      [(constF (mModel, ((firstLow tab)++"Column"++col))),
                       (cvar (show al))])
-- this should not happen at this stage anymore
transColumn p _ (Column (Def _ ) col _ _ _) =
  throwPM p (" Translator: Column "++col++" could not be related to any table.")

-- Translation of value list used in isIn-constraint.
transValList :: Pos -> String -> [Value] -> PM CExpr
transValList p mModel vals = 
    liftPM (\trVals -> list2ac trVals)  
           (sequencePM (map (transCondValue p mModel) vals))

transBinOp :: AstOp -> PM (String, String)
transBinOp ALth = cleanPM (mCDBI, "lessThan")
transBinOp ALe  = cleanPM (mCDBI, "lessThanEqual")
transBinOp AGth = cleanPM (mCDBI, "greaterThan")
transBinOp AGe  = cleanPM (mCDBI, "greaterThanEqual")
transBinOp AEq  = cleanPM (mCDBI, "equal")
transBinOp AUnEq = cleanPM (mCDBI, "notEqual")
transBinOp ALike = cleanPM (mCDBI, "like")

transTyp :: Type -> String
transTyp I = "Int"
transTyp F = "Float"
transTyp C = "Char"
transTyp B = "Bool"
transTyp S = "String"
transTyp D = "Date"
transTyp (Key name) = name++"ID"
transTyp (Entity name) = name
transTyp Unknown = "unknown"

firstUp :: String -> String
firstUp [] = []
firstUp (s:str) = (toUpper s):str 

firstLow :: String -> String
firstLow [] = []
firstLow (s:str) = (toLower s):str 

-- Translates an entity name into its description operation
-- generated by ERD2CDBI.
entity2Description :: String -> String
entity2Description name = firstLow name ++ "_CDBI_Description"
