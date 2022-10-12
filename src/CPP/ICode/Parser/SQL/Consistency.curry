--- This module checks whether all referenced table- and
--- column- and relation names exist in the data model.
--- In addition it prepares the translation of insert statements
--- (checking correct number of values, inserting null-values, and
--- column names for completion). Furthermore
--- values for key columns (insert) are transformed to
--- a default value, because the CDBI library supports just
--- auto incrementing keys.
--- It is also ensured that foreign key constraints are just used in select
--- statements and that null-values are not used in conditions (instead
--- of the operators isNull and notNull).
--- Throws an error if one of the above is not given or can not be applied.
--- Generates a warning if the notation of aliases referencing the same table
--- differs or if the notation of table name, column name or relationship
--- name differs from the one given in the data model.
--- @author: Julia Krone
--- @version: 0.1
-- ---------------------------------------------------------------------

module CPP.ICode.Parser.SQL.Consistency(checkConsistency) where

import Data.Char ( toLower, toUpper )
import Data.List ( delete )

import qualified Data.Map as Map

import CPP.ICode.ParseTypes

import CPP.ICode.Parser.SQL.AST
import CPP.ICode.Parser.SQL.ParserInfoType


--- Invokes the consistency check if a valid AST is given,
--- does nothing otherwise.
checkConsistency :: PM [Statement] -> ParserInfo -> Pos -> PM [Statement]
checkConsistency (PM (WM (Errors err) ws)) _ _ = PM $ WM (throwPR err) ws
checkConsistency (PM (WM (OK ast) ws)) pinfo p =
  let relMap = getRelations pinfo
      colMap = getAttrList pinfo
      nullMap = getNullables pinfo
      (PM (WM resPR warns)) =
           sequencePM (map (checkStatement p relMap colMap nullMap) ast)
   in (PM $ WM resPR (ws ++ warns))

-- Calls the corresponding functions for each kind of statement
-- and passes needed part of parser information.
checkStatement :: Pos ->
                  RelationFM ->
                  AttributesFM ->
                  NullableFM ->
                  Statement ->
                  PM Statement
checkStatement _ _ _ _ Rollback    = cleanPM Rollback
checkStatement _ _ _ _ Commit      = cleanPM Commit
checkStatement _ _ _ _ Transaction = cleanPM Transaction
checkStatement p relMap colMap nullMap (InTransaction stats) =
       liftPM (\chStats -> InTransaction chStats)
              (sequencePM (map (checkStatement p relMap colMap nullMap) stats))
checkStatement p relMap colMap _ (Delete tab cond) =
              (bindPM (checkTable p colMap tab)
              (checkDelete p relMap colMap cond))
checkStatement p _ colMap nullMap (Insert tab cols valss) =
                 bindPM (checkTable p colMap tab)
                        (checkInsert p colMap nullMap cols valss)
checkStatement p _ colMap _ (UpdateEntity tab val) =
     liftPM (\(_, mTab)  -> UpdateEntity mTab val)
            (checkTable p colMap tab)
checkStatement p relMap colMap _ (Update tab assigns cond) =
             bindPM (checkTable p colMap tab)
                    (checkColUpdate p relMap colMap assigns cond)
checkStatement p relMap colMap _ (Select selHead order limit) =
     liftPM (\(chHead, chOrd) -> Select chHead chOrd limit)
            (checkSelect p relMap colMap selHead order (Map.empty))

-- ------------------------delete statement ------------------------------

checkDelete :: Pos ->
               RelationFM ->
               AttributesFM ->
               Condition ->
               ((Map.Map String (String, [String])), Table) ->
               PM Statement
checkDelete p relMap colMap cond (fm, tab) =
     liftPM (\chCond -> (Delete tab chCond))
            (checkCondition p relMap colMap cond fm)

-- ------------------------ select statement ------------------------------
-- During the checking of SelectHead-node a Map is build up containing
-- lists of column names with their corresponding table name as key. This map
-- is passed to the functions which checks the order-by-clause.
-- This is not the same map as contained in the parser info as it does just
-- contain tables really intrduced in the statement not all defined the model.
checkSelect :: Pos ->
               RelationFM ->
               AttributesFM ->
               SelectHead ->
               Order ->
               Map.Map String (String, [String]) ->
               PM (SelectHead, Order)
checkSelect p relMap colMap selhead order fm =
   let (chHead, tabMap) = checkSelHead p relMap colMap selhead fm
    in combinePMs (,)
                  chHead
                  (checkOrder p tabMap order)

-- First of all the Map is build and then used to check the other
-- parts of the SelectHead-node.
-- In case of a compound selectHead both parts are checked seperately in the
-- same way and the FMs are combined afterwards (overwriting bindings in the
-- second one in case there are duplicates).
-- Throws an error if table name is not defined.
-- Finite Maps from the parserInfo have to be passed down for checking of
-- subqueries which can be part of every condition
-- (also in case-exp and having-clauses).
checkSelHead :: Pos ->
                RelationFM ->
                AttributesFM ->
                SelectHead ->
                Map.Map String (String, [String]) ->
                (PM SelectHead, Map.Map String (String, [String]))
checkSelHead p relMap colMap (Query selClause (TableRef tab join) cond gr) fm =
   let chTabs = checkTableRefs p colMap tab join fm
    in case chTabs of
        (Right tabMap) -> (combinePMs
                            (\ (chSelCl, chTables) (chCond, chGr) ->
                                (Query chSelCl chTables chCond chGr))
                            (combinePMs
                              (,)
                              (checkSelClause p tabMap relMap colMap selClause)
                              (checkJoinConds p tab join tabMap relMap colMap))
                           (checkCondGr p relMap colMap tabMap cond gr),
                             tabMap)
        (Left tabname) -> (throwPM p ("Undefined table name: "++tabname),
                          (Map.empty))
checkSelHead p relMap colMap (Set setOp head1 head2) fm =
  let (chHead1, fm1) = checkSelHead p relMap colMap head1 fm
      (chHead2, fm2) = checkSelHead p relMap colMap head2 fm
  in ((combinePMs (\h1 h2 -> Set setOp h1 h2) chHead1 chHead2),(Map.union fm2 fm1))


checkSelClause :: Pos ->
                  Map.Map String (String, [String]) ->
                  RelationFM ->
                  AttributesFM ->
                  SelectClause ->
                  PM SelectClause
checkSelClause _ _ _ _ (SelAll sp) = cleanPM (SelAll sp)
checkSelClause p fm relMap colMap (SelColumns sp elems) =
   liftPM (\chElems -> (SelColumns sp chElems))
          (checkSelElems p elems fm relMap colMap)


checkSelElems :: Pos ->
                 [SelElement] ->
                 Map.Map String (String, [String]) ->
                 RelationFM ->
                 AttributesFM ->
                 PM [SelElement]
checkSelElems p elems fm relMap colMap =
       sequencePM (map (checkSelElem p fm relMap colMap) elems)

checkSelElem :: Pos ->
                Map.Map String (String, [String]) ->
                RelationFM ->
                AttributesFM ->
                SelElement ->
                PM SelElement
checkSelElem p fm _ _ (Col col) = liftPM (\chCol -> (Col chCol))
                                  (checkColumnRef p col fm)
checkSelElem p fm rM cM (Case cond op1 op2) =
        combinePMs (\ chCond (chOp1, chOp2) -> (Case chCond chOp1 chOp2))
                   (checkSelCond p rM cM cond fm)
                   (combinePMs (,) (checkIfCol p op1 fm)
                                   (checkIfCol p op2 fm))
checkSelElem p fm _ _ (Aggregation fun sp col) =
        liftPM (\chCol -> (Aggregation fun sp chCol))
               (checkColumnRef p col fm)

-- Fills a Map with Column names and original notation
-- for each table name referenced in the statement.
-- The column names are fetched from the parser information module.
checkTableRefs :: Pos ->
                  AttributesFM ->
                  Table ->
                  Maybe JoinClause ->
                  Map.Map String (String, [String]) ->
                  Either String (Map.Map String (String, [String]))
checkTableRefs p colMap (Table name al nAl) join fm =
   let columns = Map.lookup (lowerCase name) colMap
    in case columns of
         Nothing  -> Left name
         (Just (tn, cols)) ->
              case join of
                Nothing  -> Right (Map.insert (lowerCase name) (tn,cols) fm)
                (Just (CrossJoin tab j)) ->
                       checkTableRefs p
                                      colMap
                                      tab
                                      j
                                      (Map.insert (lowerCase name) (tn,cols) fm)
                (Just (InnerJoin tab _ j)) ->
                       checkTableRefs p
                                      colMap
                                      tab
                                      j
                                      (Map.insert (lowerCase name) (tn,cols) fm)

checkJoinConds :: Pos ->
                  Table ->
                  (Maybe JoinClause) ->
                  Map.Map String (String, [String]) ->
                  RelationFM ->
                  AttributesFM ->
                  PM TableRef
checkJoinConds p tab Nothing fm _ _ =
    liftPM (\chTab -> (TableRef chTab Nothing))
           (checkTableName p tab fm)
checkJoinConds p tab (Just (CrossJoin tab2 join)) fm relMap colMap =
     combinePMs (\(chTab, chTab2) chJoin ->
                         TableRef chTab (Just (CrossJoin chTab2 chJoin)))
                (combinePMs (,) (checkTableName p tab fm)
                                (checkTableName p tab2 fm))
                (checkJoinConds' p join fm relMap colMap)
checkJoinConds p tab (Just (InnerJoin tab2 cond join)) fm relMap colMap =
  combinePMs (\(chCond, chJoin) (chTab, chTab2) ->
                   TableRef chTab (Just (InnerJoin chTab2 chCond chJoin)))
             (combinePMs (,)(checkJoinCondition p cond fm relMap colMap)
                            (checkJoinConds' p join fm relMap colMap))
             (combinePMs (,) (checkTableName p tab fm)
                             (checkTableName p tab2 fm) )

checkJoinCondition :: Pos ->
                      JoinCond ->
                      Map.Map String (String, [String]) ->
                      RelationFM ->
                      AttributesFM ->
                      PM JoinCond
checkJoinCondition p (JC cond) fm relMap colMap =
      liftPM (\ chCond -> JC chCond)
             (checkSelCond p relMap colMap cond fm)

checkJoinConds' :: Pos ->
                   (Maybe JoinClause) ->
                   Map.Map String (String, [String]) ->
                   RelationFM ->
                   AttributesFM ->
                   PM (Maybe JoinClause)
checkJoinConds' _ Nothing _ _ _ = cleanPM Nothing
checkJoinConds' p (Just (CrossJoin tab join)) fm relMap colMap =
  combinePMs (\chTab chJoin -> Just (CrossJoin chTab chJoin))
             (checkTableName p tab fm)
             (checkJoinConds' p join fm relMap colMap )
checkJoinConds' p (Just (InnerJoin tab cond join)) fm relMap colMap =
  combinePMs (\(chCond, chTab) chJoin -> (Just (InnerJoin chTab chCond chJoin)))
             (combinePMs (,)
                         (checkJoinCondition p cond fm relMap colMap)
                         (checkTableName p tab fm))
             (checkJoinConds' p join fm relMap colMap)

-- Generates warning if different notation is used for table name.
checkTableName :: Pos -> Table -> Map.Map String (String, [String]) -> PM Table
checkTableName p (Table tn al nAl) fm =
  case Map.lookup (lowerCase tn) fm of
            (Just (name, _)) ->
                   if name == tn
                     then cleanPM (Table name al nAl)
                     else warnOKPM (Table name al nAl)
                                   [(p, ("Different notation used for table "++
                                         "name "++name++" : "++tn))]
            Nothing          -> throwPM p ("Undefined table name: "++tn)

checkCondGr :: Pos ->
               RelationFM ->
               AttributesFM ->
               Map.Map String (String, [String]) ->
               Condition ->
               Maybe Group ->
               PM (Condition, (Maybe Group))
checkCondGr p relMap colMap fm cond gr =
  combinePMs (\chCond chGroup -> (chCond, chGroup))
             (checkSelCond p relMap colMap cond fm)
             (checkGroup p relMap colMap gr fm)

-- To ensure that ForeignKey-Constraints are just used in select statements
-- the checking of conditions in selects is done by this function.
-- The type of relationship is fetched from the parser info module
-- and inserted into the AST-node.
checkSelCond :: Pos ->
                RelationFM ->
                AttributesFM ->
                Condition ->
                Map.Map String (String, [String]) ->
                PM Condition
checkSelCond p relMap _ (FK (name1, al1) (NotSpec rel) (name2, al2)) fm =
  let tab1 = maybe name1 fst (Map.lookup (lowerCase name1) fm)
      tab2 = maybe name2 fst (Map.lookup (lowerCase name2) fm)
  in case (lookupRel (tab1, rel, tab2) relMap) of
        Nothing -> throwPM p ("Undefined relation "++rel++" between "++name1++
                              " and "++name2)
        (Just (relType, orgName)) ->
                        (checkRelation
                          p
                          (FK (tab1, al1) (transRel relType) (tab2, al2))
                          rel
                          orgName)
checkSelCond p rM cM (Not cond) fm = liftPM (\chCond -> Not chCond)
                                           (checkSelCond p rM cM cond fm)
checkSelCond p rM cM (Cmp logop cond1 cond2) fm =
             combinePMs (\chC1 chC2 -> (Cmp logop chC1 chC2))
                        (checkSelCond p rM cM cond1 fm)
                        (checkSelCond p rM cM cond2 fm)
checkSelCond p relMap colMap (Exists stat) fm =
             liftPM (\chSub -> Exists chSub)
                    (checkSubquery p relMap colMap stat fm)
checkSelCond p _ _ (IsNull op) fm =
             liftPM (\chOp -> (IsNull chOp))
                    (checkOperand p op fm)
checkSelCond p _ _ (NotNull op) fm =
            liftPM (\chOp -> (NotNull chOp))
                   (checkOperand p op fm)
checkSelCond p _ _ (AIn op vals) fm =
            liftPM (\chOp  -> AIn chOp vals)
                   (checkOperand p op fm)
checkSelCond p _ _ (ABinOp op operand1 operand2) fm =
           combinePMs (\chOp1 chOp2 -> ABinOp op chOp1 chOp2)
                      (checkOperand p operand1 fm)
                      (checkOperand p operand2 fm)
checkSelCond p _ _ (ABetween op1 op2 op3) fm =
          combinePMs (\chOp1 (chOp2, chOp3) -> ABetween chOp1 chOp2 chOp3)
                     (checkOperand p op1 fm)
                     (combinePMs (,) (checkOperand p op2 fm)
                                     (checkOperand p op3 fm))
checkSelCond _ _ _ NoCond _ = cleanPM NoCond

-- Generates warnings if different notation is used for column name.
checkRelation :: Pos -> Condition -> String -> String -> PM Condition
checkRelation p fkCond relName orgName =
  if relName == orgName
     then cleanPM fkCond
     else warnOKPM fkCond
                   [(p, ("Different notation used for relationship "++orgName
                         ++" : "++relName))]

transRel :: RelationType -> AbsRel
transRel (NtoOne relN) = (ANToOne relN)
transRel (OnetoN relN) = (AOneToN relN)
transRel (MtoN   relN) = (AMToN relN)

checkGroup :: Pos ->
              RelationFM ->
              AttributesFM ->
              (Maybe Group) ->
              Map.Map String (String, [String]) ->
              PM (Maybe Group)
checkGroup _ _ _ Nothing _ = cleanPM Nothing
checkGroup p relMap colMap (Just (GroupBy cols hav)) fm =
    combinePMs (\ chCols chHav -> (Just (GroupBy chCols chHav)))
               (sequencePM (map (flip(checkColumnRef p) fm) cols))
               (checkHaving p relMap colMap hav fm)

checkHaving :: Pos ->
               RelationFM ->
               AttributesFM ->
               Having ->
               Map.Map String (String, [String]) ->
               PM Having
checkHaving _ _ _ NoHave _ = cleanPM NoHave
checkHaving p rM cM (CmpHave logop hav1 hav2) fm =
     combinePMs (\chHav1 chHav2 -> (CmpHave logop chHav1 chHav2))
                (checkHaving p rM cM hav1 fm)
                (checkHaving p rM cM hav2 fm)
checkHaving p rM cM (Neg hav) fm = liftPM (\chHav -> (Neg chHav))
                                    (checkHaving p rM cM hav fm)
checkHaving p _ _ (AggrHave fun sp col op operand) fm =
    combinePMs (\chCol chOp -> (AggrHave fun sp chCol op chOp))
               (checkColumnRef p col fm)
               (checkIfCol p operand fm)
checkHaving p rM cM (SimpleHave cond) fm =
          liftPM (\chCond -> SimpleHave chCond)
                 (checkSelCond p rM cM cond fm)

checkOrder :: Pos -> Map.Map String (String, [String]) -> Order -> PM Order
checkOrder p fm (OrderBy colDirs) =
   liftPM (\chColDirs -> (OrderBy chColDirs))
          (sequencePM (map (checkColDir p fm) colDirs))

checkColDir :: Pos ->
               Map.Map String (String, [String]) ->
               (ColumnRef, Dir) ->
               PM (ColumnRef, Dir)
checkColDir p fm (col, dir) = liftPM (\chCol -> (chCol, dir))
                                     (checkColumnRef p col fm)

-- --------------------------update statement ------------------------------

checkColUpdate :: Pos ->
                  RelationFM ->
                  AttributesFM ->
                  [Assign] ->
                  Condition ->
                  (Map.Map String (String, [String]), Table) ->
                  PM Statement
checkColUpdate p relMap colMap assigns cond (fm, tab) =
    combinePMs (\chAssigns chCond -> (Update tab chAssigns chCond))
               (sequencePM (map (checkAssign p fm) assigns))
               (checkCondition p relMap colMap cond fm)

checkAssign :: Pos -> Map.Map String (String, [String]) -> Assign -> PM Assign
checkAssign p fm (Assign col val) =
    liftPM (\chCol -> (Assign chCol val))
           (checkColumnRef p col fm)

-- --------------------- insert statement --------------------------------

-- The FM is already filled with corresponding column names.
-- This function checks whether the number of given values corresponds with
-- number of given columns (or no columns are given and/or value is embedded
-- expression). In case column names are given they have to be defined in the
-- referenced table. Finally each list of values is prepared by inserting a
-- default value as key and null-values where they belong to.
-- Throws an error if number of columns or values is wrong, column name is
-- unknown or no value is given for a column that is not nullable.
-- Generates warnings for different notations.
checkInsert :: Pos ->
               AttributesFM ->
               NullableFM ->
               [ColumnRef] ->
               [[Value]] ->
               (Map.Map String (String, [String]), Table) ->
               PM Statement
checkInsert p colMap nullMap cols valss (fm, tab) =
       combinePMs (\chCols chValss -> (Insert tab chCols chValss))
                  (insertColumnRefs colMap nullMap tab)
                  (checkValueClause p nullMap tab cols valss fm)

checkValueClause :: Pos ->
                    NullableFM ->
                    Table ->
                    [ColumnRef] ->
                    [[Value]] ->
                    Map.Map String (String, [String]) ->
                    PM [[Value]]
checkValueClause p nullMap tab@(Table tn _ _) cols valss fm =
 let colNames = foldr (++) [] (map snd (Map.elems fm))
 in
  if not (and (map (checkValueCnt (length cols)) valss))
   then throwPM p ("Number of values given in insert statement is not equal "
                    ++"to number of columns referenced.")
   else case cols of
           []    -> combinePMs (\ _ vals -> vals)
                               (checkColumnNames p cols colNames tn)
                               (checkColumns p nullMap tab valss colNames)
           (_:_) -> bindPM (checkColumnNames p cols colNames tn)
                           (prepareValues p nullMap tab valss colNames)

checkValueCnt :: Int -> [Value] -> Bool
checkValueCnt n vals | n==0 = True --this case will be checked later on
                     | otherwise = (length vals) == n

checkColumnNames :: Pos -> [ColumnRef] -> [String] -> String -> PM [ColumnRef]
checkColumnNames p cols names tabName =
  sequencePM (map (findCorresName p names tabName) cols)

-- Inserts default key value in each list of values and checks whether null
-- values are allowed.
checkColumns :: Pos ->
                NullableFM ->
                Table ->
                [[Value]] ->
                [String] ->
                PM [[Value]]
checkColumns p nullMap (Table name _ _) valss colNames =
   sequencePM (map (insertKeyDefValue p nullMap name colNames) valss)

insertKeyDefValue :: Pos ->
                     NullableFM ->
                     String ->
                     [String] ->
                     [Value] ->
                     PM [Value]
insertKeyDefValue p nullMap tab cols vals =
 let l = length cols
  in case vals of
       [(Emb _ _)] -> cleanPM vals
       _           ->
           let vlen = (length vals)
           in if vlen == l
                then checkNulls p nullMap tab cols ((KeyExp tab 42):(tail vals))
                else if vlen == (l-1)
                      then checkNulls p nullMap tab cols((KeyExp tab 42):vals)
                      else throwPM p ("Expected values for "++(show l)++" or "
                                      ++(show (l-1))++" (without key) columns,"
                                      ++"but got "++(show vlen))

checkNulls :: Pos -> NullableFM -> String -> [String] -> [Value] -> PM [Value]
checkNulls _ _ _ _ [] = cleanPM []
checkNulls p _ tab [] (_:_) = throwPM p ("Too much values given for table "
                                             ++tab)
checkNulls p nullables tab (c:cols) (v:vals) =
  case v of
     AbsNull -> case Map.lookup ((firstLow tab)++c) nullables of
                 (Just True) -> liftPM (\vs -> (v:vs))
                                 (checkNulls p nullables tab cols vals)
                 _           -> throwPM p ("Column "++c++" is not nullable.")
     _       ->  liftPM (\vs -> (v:vs))
                        (checkNulls p nullables tab cols vals)

-- Checks whether there is a value given for each column that is not nullable
-- or key. In case of columns, which can be null, null values are
-- inserted in case no value is given.
prepareValues :: Pos ->
                 NullableFM ->
                 Table ->
                 [[Value]] ->
                 [String] ->
                 [ColumnRef] ->
                 PM [[Value]]
prepareValues p nls (Table tab _ _) valss colNames cols =
  case (getNullColumns nls tab colNames cols 0 []) of
     Left col    -> throwPM p ("No value given for column "++col++
                                " but it's not nullable.")
     Right nulls -> sequencePM (map (insertAndCheck nulls) valss)
  where insertAndCheck ns vss = checkNulls p
                                           nls
                                           tab
                                           colNames
                                           (insertNullValues tab ns vss)

-- Returns a list of Ints identifying the columns where null values
-- are to be inserted or the name of a column which is given without
-- value but con not be null.
getNullColumns ::  NullableFM ->
                   String ->
                   [String] ->
                   [ColumnRef] ->
                   Int ->
                   [Int] ->
                   Either String [Int]
getNullColumns _ _ [] [] _ nulls = Right nulls
getNullColumns nullables tab (n:ns) [] cnt nulls =
 case Map.lookup ((firstLow tab)++n) nullables of
    (Just True) -> getNullColumns nullables tab ns [] (cnt+1) (cnt:nulls)
    _           -> Left n
getNullColumns _ _ [] ((Column _ col _ _ _):_) _ _ = Left col
getNullColumns nullables tab (n:ns) cols@((Column _ col _ _ _):cs) cnt nulls =
  if n == (firstUp col)
    then getNullColumns nullables tab ns cs (cnt+1) nulls
    else if n == "Key"
          then getNullColumns nullables tab ns cols (cnt+1) (0:nulls)
          else case Map.lookup ((firstLow tab)++n) nullables of
                  (Just True) -> getNullColumns nullables
                                                tab
                                                ns
                                                cols
                                                (cnt+1)
                                                (cnt:nulls)
                  _           -> Left n

-- Inserts null values into the list of values according to the
-- list of Ints.
insertNullValues :: String -> [Int] -> [Value] -> [Value]
insertNullValues tab nulls vals =
  if 0 `elem` nulls
     then (KeyExp tab 42):(insertNullValues' (delete 0 nulls) 1 vals)
     else (KeyExp tab 42):(insertNullValues' nulls 1 (tail vals))
  where insertNullValues' [] _ [] = []
        insertNullValues' [] _ (v:vs) = (v:vs)
        insertNullValues' (_:ns) cnt [] =
                      AbsNull:(insertNullValues' ns cnt [])
        insertNullValues' (n:ns) cnt (v:vs) =
           if cnt `elem` (n:ns)
            then AbsNull:(insertNullValues' (delete cnt (n:ns)) (cnt+1) (v:vs))
            else v:(insertNullValues' (n:ns) (cnt+1) vs)

-- Inserts all columns for referenced table as fetched from the parser info
-- module into the AST whether they were given before or not. That's needed
-- by the type checker and translator.
insertColumnRefs :: AttributesFM ->
                    NullableFM ->
                    Table ->
                    PM [ColumnRef]
insertColumnRefs colMap nullMap (Table name _ _) =
  let cols = Map.lookup (lowerCase name) colMap
   in case cols of
         (Just (tab,cs)) -> cleanPM (map (buildColRef nullMap name) cs)
         Nothing         -> cleanPM []

buildColRef :: NullableFM -> String -> String -> ColumnRef
buildColRef nullMap tab col =
  case Map.lookup ((firstLow tab)++ col) nullMap of
        Nothing  -> (Column (Unique tab) col Unknown False 0) --should not happen
        (Just b) -> (Column (Unique tab) col Unknown b 0)

-- ---------------------common elements --------------------------------

-- Given a table all column names and the original notation of the table name
-- are fetched from the parser info module and inserted into
-- a new Map with the table name (small letters) as key.
-- Throws an error if the table name is not defined. Generates warning
-- if a different notation is used.
checkTable :: Pos ->
              AttributesFM ->
              Table ->
              PM ((Map.Map String (String,[String])), Table)
checkTable p colMap (Table name al nAl) =
 case Map.lookup (lowerCase name) colMap of
      Nothing   -> throwPM p ("There is no Table called "++name)
      (Just (tab,cs)) -> if name == tab
                           then cleanPM ((Map.singleton (lowerCase name) (tab,cs)),
                                         (Table tab al nAl))
                           else warnOKPM ((Map.singleton (lowerCase name) (tab,cs)),
                                         (Table tab al nAl))
                                         [(p, ("Different notation used for "++
                                               "table "++tab++" : "++name))]

-- Checks the condition in update and delete statements.
-- Throws an error in case a foreign key constraint is given.
checkCondition :: Pos ->
                  RelationFM ->
                  AttributesFM ->
                  Condition ->
                  Map.Map String (String, [String]) ->
                  PM Condition
checkCondition p rM cM (Not cond) fm = liftPM (\chCond -> Not chCond)
                                              (checkCondition p rM cM cond fm)
checkCondition p rM cM (Cmp logop cond1 cond2) fm =
             combinePMs (\chC1 chC2 -> (Cmp logop chC1 chC2))
                        (checkCondition p rM cM cond1 fm)
                        (checkCondition p rM cM cond2 fm)
checkCondition p rM cM (Exists stat) fm =
             liftPM (\chSub -> Exists chSub)
                    (checkSubquery p rM cM stat fm)
checkCondition p _ _ (IsNull op) fm =
             liftPM (\chOp -> (IsNull chOp))
                    (checkOperand p op fm)
checkCondition p _ _ (NotNull op) fm =
            liftPM (\chOp -> (NotNull chOp))
                   (checkOperand p op fm)
checkCondition p _ _ (AIn op vals) fm =
            combinePMs (\chOp chVals -> AIn chOp chVals)
                       (checkOperand p op fm)
                       (checkValsNotNull p vals)
checkCondition p _ _ (ABinOp op operand1 operand2) fm =
           combinePMs (\chOp1 chOp2 -> ABinOp op chOp1 chOp2)
                      (checkOperand p operand1 fm)
                      (checkOperand p operand2 fm)
checkCondition p _ _ (ABetween op1 op2 op3) fm =
          combinePMs (\chOp1 (chOp2, chOp3) -> ABetween chOp1 chOp2 chOp3)
                     (checkOperand p op1 fm)
                     (combinePMs (,) (checkOperand p op2 fm)
                                     (checkOperand p op3 fm))
checkCondition p _ _ (FK _ _ _) _ = throwPM p ("Foreign Key Constraints are just"
                                               ++" allowed in Select queries.")
checkCondition _ _ _ NoCond _ = cleanPM NoCond

-- In the subquery the outer tables can be used too, so the FM has
-- to be passed on.
checkSubquery :: Pos ->
                 RelationFM ->
                 AttributesFM ->
                 Statement ->
                 Map.Map String (String, [String]) ->
                 PM Statement
checkSubquery p rM cM (Select selHead order lim) fm =
     liftPM (\(chHead, chOrd) -> Select chHead chOrd lim)
            (checkSelect p rM cM selHead order fm)

-- Checks the operands that are used in the condition.
-- An error is thrown in case a null-value is passed.
checkOperand :: Pos -> Operand -> Map.Map String (String, [String]) -> PM Operand
checkOperand p (Left col) fm = liftPM (\chCol -> Left chCol)
                                      (checkColumnRef p col fm)
checkOperand p op@(Right val) _ =
  case val of
     AbsNull -> throwPM p ("Null values are not allowed in Condition,"++
                            " use 'isNull' or 'notNull' instead.")
     _       -> cleanPM op


-- Checks whether the given column is defined in one of the referenced tables.
-- Throws an error in case column or table name are not found.
-- In case no alias was given for the column, its name is looked up
-- in all tables that were defined without alias. If it is found in exactly
-- one table this one is set. An error is thrown in all other cases.
checkColumnRef :: Pos ->
                  ColumnRef ->
                  Map.Map String (String, [String]) ->
                  PM ColumnRef
checkColumnRef p cr@(Column (Unique tab) _ _ _ _) fm =
     case Map.lookup (lowerCase tab) fm of
          Nothing   -> throwPM p ("Table called "++tab++" is not defined.")
          Just (tn,cols) -> findCorresName p cols tn cr
checkColumnRef p (Column (Def tabs) colName typ n nAl) fm =
   let finalTab = checkAllTables tabs colName fm []
    in case (length finalTab) of
          0 -> throwPM p ("Undefined column name "++colName)
          1 -> checkColumnRef p
                              (Column (Unique (head finalTab)) colName typ n nAl)
                              fm
          _ -> throwPM p ("Column name "++colName++" cannot be related"
                           ++" to a unique table.")

-- Looks up a column name in all tables that were given without an alias.
checkAllTables :: [String] ->
                  String ->
                  Map.Map String (String, [String]) ->
                  [String] ->
                  [String]
checkAllTables [] _ _ res = res
checkAllTables (tab:ts) col fm res =
   case Map.lookup (lowerCase tab) fm of
     Nothing -> checkAllTables ts col fm res
     Just (tn, cols) -> checkAllColumns tn cols col
 where checkAllColumns _ [] cs = checkAllTables ts cs fm res
       checkAllColumns tabName (c:cs) column =
            if (lowerCase c) == (lowerCase column)
              then checkAllTables ts column fm (tabName:res)
              else checkAllColumns tabName cs column

--In a list of column names finds the corresponding one independent of
--notation differences. Throws an error if name can not be found.
--Generates warning if notation differs.
findCorresName :: Pos -> [String] -> String -> ColumnRef -> PM ColumnRef
findCorresName p [] _ (Column _ c _ _ _) = throwPM p
                                                   ("Undefined column name "++c)
findCorresName p (col:cols) tab column@(Column _ c typ n nAl) =
               if (lowerCase c) == (lowerCase col)
                 then if c == col
                        then cleanPM (Column (Unique tab) col typ n nAl)
                        else warnOKPM (Column (Unique tab) col typ n nAl)
                                      [(p,("Different notation used for column "
                                              ++col++": "++c))]
                 else findCorresName p cols tab column

-- Checks operands that are used in case-expressions and having-clauses.
-- All values (including null-values) go through at this stage just
-- columns are checked.
checkIfCol :: Pos -> Operand -> Map.Map String (String, [String]) -> PM Operand
checkIfCol _ (Right val) _ = cleanPM (Right val)
checkIfCol p (Left col) fm =
    liftPM (\chCol -> (Left chCol))
           (checkColumnRef p col fm)

-- Checks that none of the values in a list is null.
checkValsNotNull :: Pos -> [Value] -> PM [Value]
checkValsNotNull p vals =
    sequencePM (map (\v -> case v of
                            AbsNull -> throwPM p ("Found Null-value in"++
                                                  " condition clause.")
                            _       -> cleanPM v)
                    vals)

--auxiliary functions --------------------------

firstUp :: String -> String
firstUp [] = ""
firstUp (s:str) = (toUpper s):str

firstLow :: String -> String
firstLow [] = ""
firstLow (s:str) = (toLower s):str

lowerCase :: String -> String
lowerCase str = map toLower str
