--- This module resolves the pseudonym-tablename-binding for all columns
--- and all relationship constraints in each parsed statement
--- with the help of a symboltable.
--- Errors are thrown whenever an alias can not be resolved,
--- because it was not defined or is not visible or it was defined
--- more than once. An error is also thrown for an alias that was
--- defined but not used.
---@author: Julia Krone
---@version: 0.1
-- ------------------------------------------------------------------------
{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}

module CPP.ICode.Parser.SQL.Namer(nameStatements) where

import Data.Char (toLower)

import CPP.ICode.ParseTypes

import CPP.ICode.Parser.SQL.AST
import CPP.ICode.Parser.SQL.Symboltab


--- Takes a list of completely parsed statements (wrapped in a PM),
--- inserts the name of the corresponding table (resolving the
--- pseudonym if given) and sets the right alias (int) for CDBI
--- in each AST-node representing a column. The correct CDBI-alias
--- is also set for each table-node and relationship-constraint-node.
--- An error is thrown, if the referenced alias was not defined
--- in the corresponding statement or if it is defined more than once.
--- Consistence with the data model is not checked at this stage.
---@param pm - the PM containing list of parsed statements
---@return unchanged PM if it contained errors
---        PM with named columns otherwise
nameStatements ::  PM [Statement] -> Pos -> PM [Statement]
nameStatements (PM( WM (Errors err) ws)) _ = PM $ WM (throwPR err) ws
nameStatements (PM (WM (OK ast) ws))     p =
  let (PM (WM resPR warns)) = sequencePM (map (nameStatement p emptyTable) ast)
   in (PM $ WM resPR (ws ++ warns))

-- Symboltable structure used for the naming process.
-- The first finite map saves the notation given by the user,
-- the pseudonym and the CDBI-alias. The second FM keeps track of how often
-- a table name was referenced to calculate the correct CDBI-alias.
type AliasSymTab = Symboltable [(String,String, Int)] Int

--naming-function for statement node
nameStatement :: Pos -> AliasSymTab -> Statement ->  PM Statement
nameStatement p st (Select selhead order lim)  =
  let (nSelHeadPm, st1) = nameSelHead p st selhead
   in combinePMs (\nSelHead nOrder -> Select nSelHead nOrder lim)
                 nSelHeadPm
                 (nameOrder p st1 order)
nameStatement p st (Update tab assigns cond) = nameUpdate p st tab assigns cond
nameStatement _ _ (UpdateEntity tab val) = cleanPM (UpdateEntity tab val)
nameStatement p st (Delete tab cond) = nameDelete p tab st cond
nameStatement p st (Insert tab cols valss) = nameInsert p tab st cols valss
nameStatement _ _ Transaction = cleanPM Transaction
nameStatement p _ (InTransaction stats) =
   liftPM (\nStats -> (InTransaction nStats))
          (sequencePM (map (nameStatement p emptyTable) stats))
nameStatement _ _ Commit = cleanPM Commit
nameStatement _ _ Rollback = cleanPM Rollback

-- naming-funtion for selectHead-node
nameSelHead :: Pos -> AliasSymTab -> SelectHead -> (PM SelectHead, AliasSymTab)
nameSelHead p st (Query selclause tabs cond group) =
   let symtab = insertTabs tabs st
    in case symtab of
        Right st1 -> ((combinePMs (\(nsc, ntabs) (nCond, nGroup)
                                    -> (Query nsc ntabs nCond nGroup))
                       (combinePMs (,)(nameSelClause p st1 selclause)
                                      (nameTableRefs p st1 tabs))
                       (combinePMs (,)(nameCond p st1 cond)
                                      (nameGroup p st1 group))), st1)
        Left err -> ((throwPM p err), st)
nameSelHead p st (Set op h1 h2) =
   let (nhead1, st1) = nameSelHead p st h1
       (nhead2, st2) = nameSelHead p emptyTable h2
    in ((combinePMs (\nh1 nh2 -> (Set op nh1 nh2))
                   nhead1
                   nhead2)
        ,(combineST st1 st2))

-- naming-function for SelectClause-node
nameSelClause :: Pos -> AliasSymTab -> SelectClause -> PM SelectClause
nameSelClause _ _ sc@(SelAll _)          = cleanPM sc
nameSelClause p st (SelColumns sp elems) =
    liftPM (\nElems -> (SelColumns sp nElems))
           (nameElems p st elems)

-- naming-function for TableRef-node
nameTableRefs :: Pos -> AliasSymTab -> TableRef -> PM TableRef
nameTableRefs p st (TableRef tab Nothing) =
       liftPM (\nTab -> (TableRef nTab Nothing))
              (nameTable p st tab)
nameTableRefs p st (TableRef tab (Just (CrossJoin tab2 join))) =
   combinePMs (\(nTab, nTab2) nJoin ->
                    (TableRef nTab (Just (CrossJoin nTab2 nJoin))))
              (combinePMs (,)
                          (nameTable p st tab)
                          (nameTable p st tab2))
              (nameJoins p st join)
nameTableRefs p st (TableRef tab (Just (InnerJoin tab2 cond join))) =
   combinePMs (\(nTab, nTab2) (nJoinCond, nJoin) ->
                   (TableRef nTab (Just (InnerJoin nTab2 nJoinCond nJoin))))
              (combinePMs (,) (nameTable p st tab)
                              (nameTable p st tab2))
              (combinePMs (,) (nameJoinCond p st cond)
                              (nameJoins p st join))

-- naming-function for JoinClause-node
nameJoins :: Pos -> AliasSymTab ->  Maybe JoinClause -> PM (Maybe JoinClause)
nameJoins _ _ Nothing = cleanPM Nothing
nameJoins p st (Just (CrossJoin tab join)) =
            combinePMs (\nTab nJoin -> (Just (CrossJoin nTab nJoin)))
                       (nameTable p st tab)
                       (nameJoins p st join)
nameJoins p st (Just (InnerJoin tab cond join)) =
       combinePMs (\nTab (nCond, nJoin) -> (Just (InnerJoin nTab nCond nJoin)))
                  (nameTable p st tab)
                  (combinePMs (,)(nameJoinCond p st cond)
                                 (nameJoins p st join))

-- naming-function for JoinCond-node
nameJoinCond :: Pos -> AliasSymTab -> JoinCond -> PM JoinCond
nameJoinCond p st (JC cond)=
   liftPM (\nCond -> (JC nCond))
          (nameCond p st cond)

-- naming-function for Condition-node
nameCond :: Pos -> AliasSymTab -> Condition -> PM Condition
nameCond p st (FK (ps1,_) rel (ps2,_)) =
        combinePMs (\nTab1 nTab2 -> (FK nTab1 rel nTab2))
                   (getTable p ps1 st)
                   (getTable p ps2 st)
nameCond p st (Cmp op cond1 cond2)     =
      combinePMs (\nCond1 nCond2 ->(Cmp op nCond1 nCond2))
                 (nameCond p st cond1)
                 (nameCond p st cond2)
nameCond p st (Not cond) = liftPM (\nCond -> (Not nCond))
                                  (nameCond p st cond)
nameCond p st (Exists stat) = liftPM (\nStat -> (Exists nStat))
                                     (nameStatement p (enterScope st) stat)
nameCond p st (IsNull op) = liftPM (\nOp -> (IsNull nOp))
                                   (nameOperand p st op)
nameCond p st (NotNull op) = liftPM (\nOp -> (NotNull nOp))
                                    (nameOperand p st op)
nameCond p st (AIn op vals) = liftPM (\nOp -> (AIn nOp vals))
                                     (nameOperand p st op)
nameCond p st (ABinOp bop op1 op2) =
   combinePMs (\nOp1 nOp2 -> (ABinOp bop nOp1 nOp2))
              (nameOperand p st op1)
              (nameOperand p st op2)
nameCond p st (ABetween op1 op2 op3) =
   combinePMs (\nOp1 (nOp2,nOp3) -> (ABetween nOp1 nOp2 nOp3))
              (nameOperand p st op1)
              (combinePMs (,) (nameOperand p st op2)
                              (nameOperand p st op3))
nameCond _ _ NoCond = cleanPM NoCond

-- naming-function for Group-node
nameGroup :: Pos -> AliasSymTab -> Maybe Group -> PM (Maybe Group)
nameGroup p st (Just (GroupBy cols hav)) =
    combinePMs (\nCols nHave -> (Just (GroupBy nCols nHave)))
               (nameColumns p st cols)
               (nameHaving p st hav)
nameGroup _ _ Nothing = cleanPM Nothing

nameElems :: Pos -> AliasSymTab -> [SelElement] -> PM [SelElement]
nameElems p st elems = sequencePM (map (nameSingleElem p st) elems)

-- naming-function for an Element-node in SelectClause
nameSingleElem :: Pos -> AliasSymTab -> SelElement -> PM SelElement
nameSingleElem p st (Aggregation fun sp col)  =
      liftPM (\nCol -> (Aggregation fun sp nCol))
             (nameSingleColumn p st col)
nameSingleElem p st (Col col) = liftPM (\nCol -> (Col nCol))
                                       (nameSingleColumn p st col)
nameSingleElem p st (Case cond op1 op2) =
   combinePMs (\nCond (nOp1, nOp2) -> (Case nCond nOp1 nOp2))
              (nameCond p st cond)
              (combinePMs (,) (nameOperand p st op1)
                              (nameOperand p st op2))

-- naming-function for an operand-node
nameOperand :: Pos -> AliasSymTab->  Operand -> PM Operand
nameOperand p st (Left col) = liftPM (\nCol -> (Left nCol))
                                     (nameSingleColumn p st col)
nameOperand _ _ (Right val) = cleanPM (Right val)

-- naming-function for Having-node
nameHaving :: Pos -> AliasSymTab ->  Having -> PM Having
nameHaving p st (SimpleHave cond) = liftPM (\nCond -> (SimpleHave nCond))
                                           (nameCond p st cond)
nameHaving p st (AggrHave fun sp col bop op) =
   combinePMs (\nCol nOp -> (AggrHave fun sp nCol bop nOp))
              (nameSingleColumn p st col )
              (nameOperand p st op)
nameHaving p st (Neg have) = liftPM (\nHave -> (Neg nHave))
                                    (nameHaving p st have)
nameHaving p st (CmpHave lop have1 have2) =
    combinePMs (\nH1 nH2 -> (CmpHave lop nH1 nH2))
               (nameHaving p st have1)
               (nameHaving p st have2)
nameHaving _ _ NoHave = cleanPM NoHave

-- naming-function for Order-node
nameOrder :: Pos -> AliasSymTab -> Order -> PM Order
nameOrder p st (OrderBy colDirs) =
   liftPM (\nOrds -> OrderBy nOrds)
          (sequencePM (map (nameSingleOrder p st) colDirs))

-- naming-function for a single column-direction-pair in Order-clause
nameSingleOrder :: Pos -> AliasSymTab -> (ColumnRef, Dir) -> PM (ColumnRef, Dir)
nameSingleOrder p st (col, dir) = liftPM (\nCol -> (nCol,dir))
                                         (nameSingleColumn p st col)

nameUpdate :: Pos ->
              AliasSymTab ->
              Table ->
              [Assign] ->
              Condition ->
              PM Statement
nameUpdate p st tab assigns cond =
  let symtab = insertTab tab st
   in case symtab of
       Right st1 -> combinePMs (\nAssigns nCond -> (Update tab nAssigns nCond))
                               (nameAssignments p st1 assigns)
                               (nameCond p st1 cond)
         -- this should not happen in update statements
       Left err  -> throwPM p err

nameAssignments :: Pos -> AliasSymTab -> [Assign] -> PM [Assign]
nameAssignments p st assigns =  sequencePM (map (nameAssign p st) assigns)

-- naming-function for Assign-node
nameAssign :: Pos -> AliasSymTab -> Assign -> PM Assign
nameAssign p st (Assign col val) =
   liftPM (\nCol -> (Assign nCol val))
          (nameSingleColumn p st col)

nameInsert :: Pos ->
              Table ->
              AliasSymTab ->
              [ColumnRef] ->
              [[Value]] ->
              PM Statement
nameInsert p tab st cols valss =
 let symtab = insertTab tab st
    in case symtab of
        Right st1 -> liftPM (\nCols -> (Insert tab nCols valss))
                            (nameColumns p st1 cols)
          --this should not happen in isert statements
        Left err  -> throwPM p err

nameDelete :: Pos -> Table -> AliasSymTab -> Condition -> PM Statement
nameDelete p tab st cond =
 let symtab = insertTab tab st
  in case symtab of
         Right st1 -> liftPM (\nCond -> (Delete tab nCond))
                             (nameCond p st1 cond)
          --this should not happen in delete statements
         Left err  -> throwPM p err

--insert of one or more wrapped in TableRef-node
insertTabs :: TableRef -> AliasSymTab -> Either String AliasSymTab
insertTabs (TableRef tab join) st =
                    checkJoinForTabs join (insertTab tab st)


checkJoinForTabs :: Maybe JoinClause ->
                    Either String AliasSymTab ->
                    Either String AliasSymTab
checkJoinForTabs Nothing st = st
checkJoinForTabs (Just (CrossJoin tab join)) (Right st) =
                                  checkJoinForTabs join (insertTab tab st)
checkJoinForTabs (Just (InnerJoin tab _ join)) (Right st) =
                                  checkJoinForTabs join (insertTab tab st)
checkJoinForTabs (Just _) (Left tab) = (Left tab)

-- inserts a single table into the symbol table, returns modified symbol table
-- Increases count for CDBI-alias and also inserts it.
-- In case of default alias "table" also the tablename itself is
-- inserted as alias.
insertTab :: Table -> AliasSymTab -> Either String AliasSymTab
insertTab (Table name al _) st =
  let cnt = lookupSecondTable (toLowerCase name) st
   in let n = case cnt of
                 Nothing -> 0
                 Just c  -> c
       in if(al == "table")
            then case lookupFirstTable (toLowerCase name) st of
                   Nothing -> Right (insertDefFirstTab
                                      (toLowerCase al)
                                      [(name, al, n)]
                                      (++)
                                      (insertSecondTable
                                         (toLowerCase name)
                                         (n+1)
                                         (insertFirstTable (toLowerCase name)
                                                            [(name, name, n)]
                                                            st)))
                   (Just _) -> Left ("Ambiguous table reference: "++name)
            else case lookupFirstTable (toLowerCase al) st of
                   Nothing -> Right (insertFirstTable
                                    (toLowerCase al)
                                    [(name, al, n)]
                                    (insertSecondTable (toLowerCase name)
                                                       (n+1)
                                                       st))
                   (Just _) -> Left ("Alias "++al++" was defined for more "
                                      ++"than one table.")


nameColumns :: Pos -> AliasSymTab -> [ColumnRef] ->  PM [ColumnRef]
nameColumns p st cols = sequencePM (map (nameSingleColumn p st) cols)

-- naming-function for ColumnRef-node
-- replaces alias with table name and inserts CDBI-alias into column node
-- If alias is not found an error is thrown.
-- Warning is generated if notation of aliases differs.
nameSingleColumn :: Pos -> AliasSymTab -> ColumnRef -> PM ColumnRef
nameSingleColumn p st (Column (Unique pseudo) column typ nullable _)  =
  case lookupFirstTable (toLowerCase pseudo) st of
           Nothing          -> throwPM p ("Table alias "++pseudo++
                                                  " was not defined or "++
                                                  "is not visible.")
           (Just ((tab, orgAl, cnt):_)) ->
                 if orgAl == pseudo
                   then cleanPM (Column (Unique tab) column typ nullable cnt)
                   else warnOKPM (Column (Unique tab) column typ nullable cnt)
                                 [(p, ("Found different notation of same "++
                                       "alias: "++pseudo++" and "++orgAl))]
nameSingleColumn p st (Column (Def pseudo) column typ nullable cnt) =
  case lookupFirstTable (toLowerCase (head pseudo)) st of --can just be default name "table"
     Nothing -> throwPM p ("No table given for column "
                            ++column++". Maybe alias was"
                            ++" defined but not used.")
     (Just tabs) -> let tns = map fstOfTriple tabs
                     in cleanPM (Column (Def tns) column typ nullable cnt)

-- inserts correct CDBI-Alias into each table-node
-- ensures that alias is unique
nameTable :: Pos -> AliasSymTab -> Table -> PM Table
nameTable p st (Table name al _) =
  let key = if(al == "table")
               then (toLowerCase name)
               else (toLowerCase al)
  in case lookupFirstTable key st of
         Nothing -> throwPM p ("No Table found for table "++name) --can not happen
         (Just ((n, _ , cnt):_)) -> if ((toLowerCase n) == (toLowerCase name))
                                     then cleanPM (Table name al cnt)
                                     else throwPM p ("Alias "++key++
                                                     " is defined for" ++
                                                     " more than one table.")

-- Lookup-function used by relationship constraint
-- Error is thrown if alias was not defined, a warning
-- is generated if notation differs from defined alias.
getTable :: Pos -> String -> AliasSymTab -> PM (String, Int)
getTable p pseudo st =
 case lookupFirstTable (toLowerCase pseudo) st of
         Nothing           -> throwPM p ("Table alias "++pseudo++
                                              " was not defined.")
         (Just ((tab, orgAl, cnt):_)) ->
              if orgAl == pseudo
                then cleanPM (tab, cnt)
                else warnOKPM (tab, cnt)
                              [(p, ("Found different notation of same "++
                                    "alias or table: "++pseudo++" and "++
                                    orgAl))]


toLowerCase :: String -> String
toLowerCase str = map toLower str

fstOfTriple :: (a,b,c) -> a
fstOfTriple (ele, _ , _) = ele
