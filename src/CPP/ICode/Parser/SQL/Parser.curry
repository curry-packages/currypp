--- This module parses a list of Token according to
--- the supported SQL grammar. It internally makes use of the SQL Parser Monad
--- defined in the SQLParserTypes module.
--- The result is an abstract syntax tree or - in case of errors - 
--- the list of error messages wrapped in the ParseMonad (PM) which is
--- part of curryPP.
--- An error recovery approach is realized.
---@author Julia Krone
---@version 0.1
-- ----------------------------------------------------------------------------

module CPP.ICode.Parser.SQL.Parser(parseTkLs) where

import CPP.ICode.ParseTypes

import CPP.ICode.Parser.SQL.AST
import CPP.ICode.Parser.SQL.Token
import CPP.ICode.Parser.SQL.ParserTypes

                          
--- Organizes the wrapping with respect to the internally used Parser Monad.
--- Invokes the parsing process for each single statement.
--- Returns a list of statements (as AST) wrapped in a ParseMonad.
parseTkLs :: Pos -> [Token] -> PM [Statement]
parseTkLs pos tks =  let (SPM _ pm tk) = parseStatement (newEmptySPM pos tks)
                     in if tk == [] 
                          then liftPM (\st -> [st]) pm
                          else combinePMs (:) pm (parseTkLs pos tk) 

  
-- selects which kind of SQL-Statement to parse
parseStatement :: SPMParser Statement
parseStatement espm 
  | hasToken espm = 
    case headToken espm of
          KW_Select    -> (parseSelect .<~. (terminalOrConsume Semi))
                                           (continue espm)
          KW_Insert    -> (parseInsert 
                              .<~. (terminalOrConsume Semi))
                                   (continue espm)
          KW_Delete    -> (parseDelete
                           .<~. (terminalOrConsume Semi))
                            (continue espm)
          KW_Update    -> bindDefSPM parseTableName 
                                    (Table "" "" 0)
                                    (\t -> (((terminalOrProc KW_Set 
                                                            [KW_Where, Semi]) 
                                            .~>.(parseUpdate t)) 
                                            .<~.(terminalOrConsume Semi)))
                                     [KW_Set]
                                     (continue espm)
          In           -> (parseTransaction
                            .<~. (terminalOrConsume Semi))
                             (continue espm)
          KW_Begin     ->  ((terminalOrConsume Semi) .~>. 
                                  (initializeSPM Transaction))
                                  (continue espm)
          KW_Commit    ->  ((terminalOrConsume Semi) .~>. 
                                     (initializeSPM Commit)) 
                                     (continue espm)
          KW_Rollback  ->  ((terminalOrConsume Semi) .~>. 
                                      (initializeSPM Rollback))
                                      (continue espm)
          _                 -> parseError ("There is no valid keyword at the "
                                            ++"beginning of SQL-Statement") 
                                          (proceedAfter Semi espm)
  |otherwise = emptyTkErr espm    
  
-- --------------------transaction statement -------------------------------
-- Parser for Transaction Statement. The In Transaction Statement is a special
-- functionality of CDBI, which executes the given List of Statement (just
-- returning the last result) and automatically executes a rollback in case of
-- an error and a commit otherwise
parseTransaction :: SPMParser Statement
parseTransaction espm =
      ((terminalOrProc KW_Transaction [Semi])  .~>.                                  
      (liftSPM (\sts -> InTransaction sts)
              (combineSPMs (:) 
                          ((terminalOrProc LParen [Semi]) 
                                       .~>.parseStatement)
                          parseStatements)))
              espm
       
-- Combines several statements to a list.
parseStatements :: SPMParser [Statement]
parseStatements espm
  | hasToken espm =
     case headToken espm of
           RParen -> initializeSPM [] (continue espm)
           _      -> combineSPMs (:) 
                                 parseStatement
                                 parseStatements
                                 espm
  | otherwise = emptyTkErr espm

 
-- --------------------parsing functions for select statement --------------- 
-- Parser for Select-Statement, which consists of 
-- SelectHead, Order-By-Clause and Limit
parseSelect :: SPMParser Statement
parseSelect espm =
   combineSPMs (\selhead (order, lim) -> Select selhead order lim)
               parseSelectHead 
               parseOrderNLimit
               espm

-- Parser for Order-By-Clause and Limit-Clause of Select-Statement                              
parseOrderNLimit :: SPMParser (Order, Maybe Int)
parseOrderNLimit espm = combineSPMs (\order lim -> (order, lim))
                                    parseOrderBy
                                    parseLimit
                                    espm
 

                                 
-- Parser for SelectHead of Select-Statement, which can be followed 
-- by another SelectHead connected by a setoperator
-- Jumps to next symbol in follow-set in case of error using
-- a default selectHead for binding.
parseSelectHead :: SPMParser SelectHead 
parseSelectHead espm = 
  bindDefSPM parseQuery defSelHead parseSetOrSimple follow espm
    where follow = setOps++[KW_Order, KW_Limit]
          defSelHead =  Query (SelAll AAll) 
                              (TableRef (Table "" "" 0) Nothing)
                              NoCond 
                              Nothing   

-- checks if SelectHead is followed by another one and if so
-- invokes corresponding parser
parseSetOrSimple :: SelectHead -> SPMParser SelectHead
parseSetOrSimple selhead espm 
  | hasToken espm =
     case headToken espm of
              (SetOp _ ) -> combineSPMs (\op head2 -> Set op selhead head2)
                                        parseSetOp
                                        ((terminalOrProc KW_Select 
                                                        [KW_Order, KW_Limit]) 
                                         .~>. parseSelectHead)
                                        espm
              _          -> initializeSPM selhead espm
  | otherwise = emptyTkErr espm

-- Parser for setoperator  
-- in case of failure jumps to next Select-keyword or Semicolon
parseSetOp :: SPMParser ASetOp
parseSetOp espm
  | hasToken espm =
     case headToken espm of
            (SetOp Union)     -> initializeSPM AUnion (continue espm)
            (SetOp Intersect) -> initializeSPM AIntersect (continue espm)
            (SetOp Except)    -> initializeSPM AExcept (continue espm)
            _                 -> parseError ("Expected set-operator but got "++
                                              (tokentoString $ headToken espm))
                                            (proceedWithOneOf  [KW_Select,
                                                                Semi] espm)
  | otherwise = emptyTkErr espm

-- Parser for combining all parts of SelectHead
parseQuery :: SPMParser SelectHead
parseQuery espm = combineSPMs 
                     (\ sel (tabs, cond, group) -> Query sel tabs cond group) 
                     parseSelectClause
                     parseFromCondGroup
                     espm

-- Parser to combine parts of Selectclause i.e. all or distinct and elementlist                     
parseSelectClause :: SPMParser SelectClause 
parseSelectClause espm = bindSPM parseSpecifier parseSelElements [KW_From] espm

-- Parser for All or Distinct, All is taken as default
parseSpecifier :: SPMParser ASpecifier
parseSpecifier espm 
  | hasToken espm =
     case headToken espm of
            KW_Distinct -> initializeSPM ADistinct (continue espm)
            KW_All      -> initializeSPM AAll      (continue espm)
            _           -> initializeSPM AAll      espm
  | otherwise = emptyTkErr espm 
 
-- Parser for Selectclause 
parseSelElements :: ASpecifier -> SPMParser SelectClause
parseSelElements spec espm 
  | hasToken espm =
     case headToken espm of 
           Asterix    -> initializeSPM (SelAll spec) (continue espm)
           _          -> liftSPM (\elems -> SelColumns spec elems)
                                 (combineSPMs (:) parseElem parseElemList)
                                 espm
  | otherwise = emptyTkErr espm

-- Parser for List of Elements(Columns, Case-Expression,
-- Aggregation) which to Select
-- Jumps to From-Clause (follow-set) in case of error
parseElemList :: SPMParser [SelElement]
parseElemList espm 
  | hasToken espm =
     case headToken espm of 
       KW_From -> initializeSPM [] espm
       Comma   -> combineSPMs (:) 
                              parseElem 
                              parseElemList
                              (continue espm) 
       _       -> parseError ("Expected ',' or 'From', but got: "++
                               (tokentoString $ headToken espm))
                             (proceedWith KW_From espm)
  | otherwise = emptyTkErr espm

-- Parser for single Select-element  
parseElem :: SPMParser SelElement
parseElem espm 
  | hasToken espm =
     case headToken espm of
        KW_Case       -> ((terminalOrProc KW_When [Comma, KW_From]) .~>. 
                         (combineSPMs (\cond (val1, val2) -> Case cond val1 val2)
                                      parseConstraint
                                      (parseValueTuple .<~. (terminal KW_End))))
                                      (continue espm)
        (Fun _)    -> parseAggregation espm
        _          -> liftSPM (\col -> Col col) parseColumn espm
  | otherwise = emptyTkErr espm

-- Parser for the two values given in a case-expression
parseValueTuple :: SPMParser (Operand, Operand)
parseValueTuple espm = combineSPMs (\val1 val2 -> (val1, val2))
                                   ((terminalOrProc KW_Then [KW_Else]) 
                                                   .~>. parseOperand)
                                   ((terminalOrProc KW_Else [KW_End, KW_From]) 
                                                   .~>. parseOperand)
                                   espm
 
-- Parser for aggrgation function name
-- Jumps to next symbol in follow-set or '(' in case of error
parseFun :: SPMParser AFun
parseFun espm = 
 case headToken espm of
       (Fun Sum)   -> initializeSPM ASum (continue espm)
       (Fun Avg)   -> initializeSPM AAvg (continue espm)
       (Fun Count) -> initializeSPM ACount (continue espm)
       (Fun Min)   -> initializeSPM AMin (continue espm)
       (Fun Max)   -> initializeSPM AMax (continue espm)
       _           -> parseError ("Not supported aggregationfunction"
                                      ++ "given") 
                                 (proceedWithOneOf [LParen, Comma, KW_From]
                                                    espm)

-- Parser to combine whole aggregation expression                                 
parseAggregation :: SPMParser SelElement
parseAggregation espm = 
      combineSPMs (\fun (spec, col) -> Aggregation fun spec col)
                  parseFun
                  (((terminalOrProc LParen [Comma, KW_From]) .~>. 
                    (combineSPMs (,)
                                 parseSpecifier
                                 parseColumn))
                    .<~. (terminal RParen))
                   espm

-- Parser to combine rear parts of SelectHead, i.e.
-- FromClause, WhereClause and Group-By-Clause
parseFromCondGroup :: SPMParser (TableRef, Condition, Maybe Group)
parseFromCondGroup espm = 
    combineSPMs (\tabs (cond, group) -> (tabs, cond, group))
                ((terminalOrProc KW_From follow) 
                                  .~>. parseTableRef)
                parseCondGroup
                espm
 where follow = setOps++[KW_Where, KW_Group, KW_Order, KW_Limit]
 
-- Parser to combine parts of a TableReference 
parseTableRef :: SPMParser TableRef
parseTableRef espm = combineSPMs (\tab join -> TableRef tab join)
                                 parseTabNPseudo 
                                 parseMaybeJoin
                                 espm

-- Combines TableName with Pseudonym if there is one 
-- Jumps to next symbol in follow-set in case of error.                                 
parseTabNPseudo :: SPMParser Table
parseTabNPseudo espm = 
  bindDefSPM parseIdentifier "" parsePseudonym follow espm
   where follow = 
            [KW_As, KW_Cross, KW_Inner, Comma, KW_Where, KW_On, 
                    KW_Group, KW_Order, KW_Limit]++setOps
 
-- Creates TableNode with Pseudonym (if there is one) 
-- otherwise uses default alias "table"
-- Default CDBI-alias 0 will be resolved by Namer.
parsePseudonym :: String -> SPMParser Table
parsePseudonym tab espm
  | hasToken espm =
     case headToken espm of
           KW_As          -> (parsePseudoString tab) (continue espm)          
           _              -> initializeSPM (Table tab "table" 0) espm
  | otherwise = emptyTkErr espm
      
-- Parser for String representing the table-pseudonym.
-- Creates Tablenode with name, alias CDBI-alias.
-- Default CDBI-alias 0 will be resolved by Namer.
-- Jumps to next symbol in follow-set in case of error. 
parsePseudoString :: String -> SPMParser Table
parsePseudoString tab espm
  | hasToken espm =
     case headToken espm of
            (Ident pseudo) -> initializeSPM (Table tab pseudo 0) 
                                            (continue espm)
            KW_Table       -> parseError ("Table is a reserved keyword "++
                                           "and not allowed as alias.")
                                         (proceedWithOneOf follow espm)  
            _              -> parseError ("No valid alias found after 'AS'.")
                                         (proceedWithOneOf follow espm)
  | otherwise = emptyTkErr espm
  where follow = [Comma, KW_Where, KW_Group, KW_Order, KW_Limit]++setOps

-- Checks if there is a Join and in case there is, invokes the right
-- parsing procedure, if not initializes with Nothing
parseMaybeJoin :: SPMParser (Maybe JoinClause)
parseMaybeJoin espm
  | hasToken espm =
      case headToken espm of
         KW_Cross -> ((terminalOrProc KW_Join follow) .~>.
                          (combineSPMs (\tab join -> Just (CrossJoin tab join)) 
                                       parseTabNPseudo
                                       parseMaybeJoin))
                                       (continue espm)
         KW_Inner -> ((terminalOrProc KW_Join follow) .~>. 
                         combineSPMs (\(tab, cond) join -> 
                                             Just (InnerJoin tab cond join))
                                     (combineSPMs (\tab cond -> (tab, cond))
                                                 parseTabNPseudo
                                                 parseJoinCond)
                                     parseMaybeJoin)
                                     (continue espm)
         Comma    -> (combineSPMs (\tab join -> Just (CrossJoin tab join)) 
                                  parseTabNPseudo
                                  parseMaybeJoin)
                                  (continue espm)
         _        -> initializeSPM Nothing espm
  | otherwise = emptyTkErr espm
  where follow = [Comma, KW_Where, KW_Group, KW_Order, KW_Limit]++setOps

-- Parser for JoinCondition (for inner joins)  
-- Jumps to next symbol in follow-set in case of error. 
parseJoinCond :: SPMParser JoinCond 
parseJoinCond espm
  | hasToken espm =
     case headToken espm of
            KW_On -> liftSPM (\cond -> JC cond)
                             (bindSPM parseConstraint                                       
                                      parseConstraints
                                      follow)
                              (continue espm)             
            _    -> parseError ("Missing condition for inner join")
                               (proceedWithOneOf follow espm)
  | otherwise = emptyTkErr espm
  where follow = [KW_Cross, KW_Inner, Comma, KW_Where, KW_Group,
                         KW_Order, KW_Limit]++setOps

-- Combines Condition (Where-Clause) and Group-By-Clause of Select-Statement  
parseCondGroup:: SPMParser (Condition, Maybe Group)
parseCondGroup espm = combineSPMs (\cond group -> (cond, group))
                                  parseSelWhereClause
                                  parseGroup
                                  espm

-- Checks if there is a Where-Clause and if there is, invokes
-- the corresponding parsing procedure
-- Jumps to next symbol in follow-set in case of error. 
parseSelWhereClause :: SPMParser Condition
parseSelWhereClause espm
  | hasToken espm =
     case headToken espm of
           KW_Group    -> initializeSPM NoCond espm
           (SetOp _)   -> initializeSPM NoCond espm
           KW_Order    -> initializeSPM NoCond espm
           Semi        -> initializeSPM NoCond espm
           KW_Limit    -> initializeSPM NoCond espm
           RParen      -> initializeSPM NoCond espm
           KW_Where    -> bindDefSPM parseConstraint 
                                     NoCond                                   
                                     parseConstraints 
                                     followConstr
                                     (continue espm)
           _           -> parseError ("Error while parsing before: "++ 
                                       (tokentoString $ headToken espm))
                                     (proceedWithOneOf followWhere espm)
  | otherwise = emptyTkErr espm
  where followWhere = [KW_Group, KW_Order, KW_Limit]++setOps
        followConstr = [ RParen, KW_Then, KW_Cross, KW_Inner, 
                         KW_Group, KW_Order, KW_Limit]++logOps++setOps

--Parser for Group-By-Clause. Initializes with Nothing if there is non.  
parseGroup :: SPMParser (Maybe Group)
parseGroup espm
  | hasToken espm =
     case headToken espm of
       KW_Group ->  ((terminalOrProc KW_By [KW_Order, KW_Limit]) .~>.
                      (combineSPMs (\ cols have -> Just (GroupBy cols have))
                                   (combineSPMs (:)
                                                parseColumn
                                                parseColumnList)
                                    parseHaving))
                      (continue espm)
       _        -> initializeSPM Nothing espm
  | otherwise = emptyTkErr espm

-- Parser for list of Columns in Group-By-Clause.
-- Jumps to next symbol in follow-set in case of error.
parseColumnList :: SPMParser [ColumnRef]
parseColumnList espm 
  | hasToken espm =
      case headToken espm of
              Semi      -> initializeSPM [] espm
              KW_Having -> initializeSPM [] espm
              RParen    -> initializeSPM [] espm
              KW_Order  -> initializeSPM [] espm
              Comma     -> combineSPMs (:)
                                       parseColumn
                                       parseColumnList
                                       (continue espm)
              _         -> parseError ("Expected list of columns for group by "
                                        ++ "statement") 
                                      (proceedWithOneOf ([KW_Having,
                                                          KW_Order,
                                                          KW_Limit] 
                                                          ++setOps) espm)
  | otherwise = emptyTkErr espm

-- Combines parts of Having-Clause  
-- Jumps to next symbol in follow-set in case of error.
parseHaving :: SPMParser Having 
parseHaving espm
  | hasToken espm =
     case headToken espm of
            KW_Having -> bindDefSPM parseHaveCond
                                    NoHave
                                    parseCompHaveCond 
                                    follow
                                 (continue espm)
            _         -> initializeSPM NoHave espm
  | otherwise = emptyTkErr espm
  where follow = [RParen, KW_Order, KW_Limit]++setOps++logOps

-- Parser for Condition in Having-Clause.
-- Treated seperatly to make sure that aggregation is not 
-- used in Where-Clauses.
-- Jumps to next symbol in follow-set in case of error. 
parseHaveCond :: SPMParser Having
parseHaveCond espm
  | hasToken espm =
     case headToken espm of
           LParen   -> ((bindDefSPM parseHaveCond  
                                    NoHave
                                    parseCompHaveCond
                                    follow)
                        .<~. (terminal RParen)) (continue espm)
           KW_Not   -> liftSPM (\cond -> Neg cond)
                               (bindDefSPM parseHaveCond  
                                           NoHave
                                           parseCompHaveCond
                                           follow)
                               (continue espm)
           (Fun _) -> (combineSPMs (\fun (spec, col, op, operand) -> 
                                           (AggrHave fun spec col op operand))
                                   parseFun
                                   parseAggrHave)
                                   espm
           _         -> liftSPM (\cond -> SimpleHave cond)
                               parseConstraint
                               espm
  | otherwise = emptyTkErr espm
 where follow = [RParen, KW_Order, KW_Limit]++setOps++logOps

-- Parser to combine compound having conditions
-- Jumps to next symbol in follow-set in case of error.  
parseCompHaveCond :: Having -> SPMParser Having
parseCompHaveCond have espm 
  | hasToken espm =
     case headToken espm of
            (LogOp And) -> liftSPM (\have2 -> CmpHave AAnd have have2)
                                   (bindDefSPM parseHaveCond
                                               NoHave
                                               parseCompHaveCond
                                               follow)
                                   (continue espm)
            (LogOp Or)  -> liftSPM (\have2 -> CmpHave AOr have have2)
                                   (bindDefSPM parseHaveCond
                                               NoHave
                                               parseCompHaveCond
                                               follow)
                                   (continue espm)
            _           -> initializeSPM have espm
  | otherwise = emptyTkErr espm
  where follow = logOps ++ setOps ++ [RParen, KW_Order, KW_Limit]
 
--Combines all parts of aggregation inside having-clause
parseAggrHave :: SPMParser (ASpecifier, ColumnRef, AstOp, Operand)
parseAggrHave espm = 
  combineSPMs (\spec (col, op, operand) -> (spec, col, op, operand))
              ((terminalOrProc LParen follow) .~>. parseSpecifier)
              parseComparison
              espm
 where follow = logOps++binOps++setOps++[RParen, KW_Order, KW_Limit]

-- Parser for comparison part inside the having-clause                                 
parseComparison :: SPMParser (ColumnRef, AstOp, Operand)
parseComparison espm = combineSPMs (\col (op, operand) -> (col, op, operand))
                                   (parseColumn .<~. (terminal RParen))
                                   (combineSPMs (\op operand -> (op, operand))
                                               parseBinOperator
                                               parseOperand)
                                   espm           

-- Parser for Order-By-Clause                                   
parseOrderBy :: SPMParser Order
parseOrderBy espm
  | hasToken espm =
     case headToken espm of
           KW_Order -> ((terminalOrProc KW_By [KW_Limit]) .~>.
                        (liftSPM (\cols -> OrderBy cols)
                                  (combineSPMs (:)
                                               parseOrderingTerm
                                               parseOrderingList)))
                                 (continue espm)
           _        -> initializeSPM  (OrderBy []) espm
  | otherwise = emptyTkErr espm

-- Parser for list of Columns and ordering directions
-- Jumps to next limit or semicolon in case of error
parseOrderingList :: SPMParser  [(ColumnRef, Dir)]
parseOrderingList espm 
  | hasToken espm =
     case headToken espm of
            Comma    -> combineSPMs (:)
                                    parseOrderingTerm
                                    parseOrderingList
                                    (continue espm)
            Semi     -> initializeSPM [] espm
            KW_Limit -> initializeSPM [] espm
            RParen   -> initializeSPM [] espm
            _        -> parseError ("Unexpected Token in Order-By-Clause, got: "
                                    ++ (tokentoString $ headToken espm)) 
                                    (proceedWith KW_Limit espm)
  | otherwise = emptyTkErr espm

-- Parser for single column-direction-Tuple   
parseOrderingTerm :: SPMParser (ColumnRef, Dir)
parseOrderingTerm espm = combineSPMs (\ col dir -> (col, dir))
                                     parseColumn
                                     parseDir
                                     espm

-- Parser for direction, default is Ascending order                                     
parseDir :: SPMParser Dir 
parseDir espm
  | hasToken espm = 
     case headToken espm of
            KW_Asc  -> initializeSPM Asc (continue espm)
            KW_Desc -> initializeSPM Desc (continue espm)
            _       -> initializeSPM Asc espm
  | otherwise = emptyTkErr espm 

-- Parser for limit-clause  
parseLimit :: SPMParser (Maybe Int)
parseLimit espm
  | hasToken espm =
     case headToken espm of
           KW_Limit  -> liftSPM (\int -> Just int) 
                                parseIntegerExp
                                (continue espm)
           _          ->  initializeSPM Nothing espm
  | otherwise = emptyTkErr espm

-- Parser for Integer expression in limit-clause
-- jumps to next Semicolon in case of error
parseIntegerExp :: SPMParser Int 
parseIntegerExp espm
  | hasToken espm =
     case headToken espm of
          (Constant (NumInt int)) -> initializeSPM int (continue espm)
          _                       -> parseError "Expected Int to define limit."
                                                (proceedWith Semi espm)
  | otherwise = emptyTkErr espm
                             
-- ------------------------- parsing functions for Insert statement ----------
-- Combines tablename and insert specification for insert statement
parseInsert :: SPMParser Statement
parseInsert espm =
  bindDefSPM ((terminalOrProc KW_Into [LParen, KW_Values]) .~>. parseTableName)
              defTab parseInsertSpec
              [LParen, KW_Values] 
              espm
  where defTab = (Table "" "" 0)
   
parseInsertSpec :: Table -> SPMParser Statement
parseInsertSpec table espm = 
   combineSPMs (\columns valss -> Insert table columns valss)
               parseMaybeColumns 
               ((terminalOrProc KW_Values [Semi]) .~>. parseValueClause)
               espm
  
-- Checks whether there are just Values given or first a list of Columns
-- and invokes corresponding parsing routine.
-- Jumps to next Values-keyword or Semicolon in case of error.
parseMaybeColumns :: SPMParser [ColumnRef]
parseMaybeColumns espm 
  | hasToken espm = 
     case headToken espm of
            KW_Values  ->  initializeSPM [] espm
            LParen     ->  combineSPMs (:)
                                       parseColumn
                                       parseColumns
                                       (continue espm)
            _          ->  parseError ("expected ValueClause starting with "
                                      ++"keyword 'Values' or a list of Columns")
                                      (proceedWith KW_Values espm)
  |otherwise = emptyTkErr espm

-- Parser for simple List of columns in insert statement.
-- Jumps to next Values-keyword or Semicolon in case of error.
parseColumns :: SPMParser [ColumnRef]
parseColumns espm 
  | hasToken espm = 
     case headToken espm of
        RParen      -> initializeSPM [] (continue espm)
        Comma       -> combineSPMs (:)
                                   parseColumn
                                   parseColumns
                                   (continue espm) 
        _           ->  parseError ("Expected List of Columns seperated by "++
                                    "Comma and terminated by )") 
                                   (proceedWith KW_Values espm)
  |otherwise = emptyTkErr espm
  
-- Parser for Value-Clause of Insert statement
parseValueClause :: SPMParser [[Value]]
parseValueClause espm = combineSPMs (:)
                                    parseValsOrEmb 
                                    parseValClTail 
                                    espm 

-- Combines values to a list of lists of values.                                   
parseValClTail :: SPMParser [[Value]] 
parseValClTail espm 
  | hasToken espm = 
     case headToken espm of
         Semi     -> initializeSPM [] espm
         Comma    -> combineSPMs (:) 
                                 parseValsOrEmb 
                                 parseValClTail 
                                 (continue espm) 
         _                  -> parseError ("Found " ++ 
                                           (tokentoString $ headToken espm)
                                           ++" while parsing List of values.")
                                          (proceedWith Semi espm)
  | otherwise = emptyTkErr espm
 
-- Distinguishes a single embedded expression from a list of values.
-- Jumps to next Semicolon in case of error.
parseValsOrEmb :: SPMParser [Value]
parseValsOrEmb espm 
  | hasToken espm =
      case headToken espm of
         (EmbedExp exp) -> initializeSPM [(Emb exp Unknown)] (continue espm)
         LParen             -> parseValueList espm                          
         _                  -> parseError ("Found " ++ 
                                           (tokentoString $ headToken espm)
                                           ++" while parsing List of values.")
                                          (proceedWith Comma espm)      
  | otherwise = emptyTkErr espm

-- -------------------delete statement --------------------------------  

-- Combines tablename with where-clause to obtain a Delete statement.
parseDelete :: SPMParser Statement
parseDelete espm = combineSPMs (\table cond -> (Delete table cond))
                                ((terminalOrProc KW_From [KW_Where, Semi]) 
                                        .~>.parseTableName)
                                parseWhereClause
                                espm
       

-- ---------------------- update statement ---------------------------

-- Parser for Update statement. Checks whether a whole entity or single
-- assignments are given and invokes corresponding parsing routine.
-- Jumps to next semicolon in case of error.
parseUpdate :: Table -> SPMParser Statement
parseUpdate table espm 
 | hasToken espm = 
    case headToken espm of
       (Ident _)       -> combineSPMs 
                                (\ assings cond -> (Update table assings cond)) 
                                parseAssignments
                                parseWhereClause  
                                espm           
       (EmbedExp exp) -> initializeSPM (UpdateEntity table (Emb exp Unknown)) 
                                           (continue espm)                              
       _                  -> parseError ("Expected assignements or an "++
                                          "embedded expression in Update") 
                                        (proceedWith Semi espm)
  |otherwise = emptyTkErr espm

-- Parses list of column names and corresponding values for assignments,
-- Jumps to next Where-Clause or Semicolon in case of error.
parseAssignments :: SPMParser [Assign]
parseAssignments espm 
  | hasToken espm = 
     case headToken espm of
           (Ident _) -> combineSPMs (:) parseAssignment parseAssignments espm
           Comma     -> combineSPMs (:) 
                                    parseAssignment 
                                    parseAssignments 
                                    (continue espm)
           KW_Where  -> initializeSPM [] espm
           Semi      -> initializeSPM [] espm
           _         -> parseError ("Error while parsing Assingments, found: "
                                    ++ (tokentoString $ headToken espm))
                                   (proceedWith KW_Where espm)
  | otherwise = emptyTkErr espm
 
-- Parser for a single assignment.
--Jumps to next symbol in Follow-set in case of error.
parseAssignment :: SPMParser Assign
parseAssignment espm
  | hasToken espm =
     case headToken espm of
        (Ident name) -> ((terminalOrProc (BinOp Equal) [Comma, KW_Where]) .~>. 
                          (liftSPM 
                            (\val -> Assign 
                                      (Column (Def ["table"]) name Unknown False 0)
                                      val)
                             parseValue))
                           (continue espm)
        _            -> parseError "Expected name of a column for assignment." 
                                    (proceedWithOneOf [Comma,
                                                      KW_Where] espm)
  | otherwise = emptyTkErr espm    

-- -------------------common parts -----------------------------------  
-- Parser for tablename (without pseudonym)
-- using default "table" as alias and default CDBI-alias 0.
--Jumps to next symbol in Follow-set in case of error.
parseTableName :: SPMParser Table
parseTableName espm
  | hasToken espm =
     case headToken espm of
           (Ident name) ->  initializeSPM (Table name "table" 0)
                                          (continue espm)
           _            -> parseError ("Expected tablename, but got: "
                                       ++ (tokentoString $ headToken espm))
                                      (proceedWithOneOf follow espm)
  | otherwise = emptyTkErr espm
  where follow = [KW_Where, LParen, KW_Set, KW_Values]

  
--Parser for Where-Clause.
--Jumps to next symbol in Follow-set in case of error.
parseWhereClause :: SPMParser Condition
parseWhereClause espm 
  | hasToken espm = 
      case headToken espm of
            Semi     -> initializeSPM NoCond espm
            KW_Where -> bindDefSPM parseConstraint 
                                   NoCond
                                   parseConstraints 
                                   follow
                                  (continue espm)
            _        -> parseError ("Expected Start of Where-clause" ++
                                       " or end of query, but got "++
                                       (tokentoString $ headToken espm)) 
                                   (proceedWithOneOf follow espm)
  |otherwise = emptyTkErr espm
  where follow = [RParen, KW_Then, KW_Cross, KW_Inner, KW_Group,
                   KW_Order, KW_Limit, Semi]++logOps++setOps 

-- Parser for compound constraints. 
--Jumps to next symbol in Follow-set in case of error. 
parseConstraints :: Condition -> SPMParser Condition
parseConstraints cond espm 
  | hasToken espm =
       case headToken espm of
              (LogOp And) -> liftSPM (\cond2 -> Cmp AAnd cond cond2)
                                     (bindDefSPM parseConstraint 
                                                 NoCond
                                                 parseConstraints
                                                 follow) 
                                     (continue espm)
              (LogOp Or)  -> liftSPM (\cond2 -> Cmp AOr cond cond2)
                                     (bindDefSPM parseConstraint
                                              NoCond 
                                              parseConstraints
                                              follow)
                                     (continue espm)
              _           -> initializeSPM cond espm
  | otherwise = emptyTkErr espm
  where follow = [ RParen, KW_Then, KW_Cross, KW_Inner, KW_Group,
                   KW_Order, KW_Limit, Semi]++logOps++setOps 

-- Parser for a constraints. 
--Jumps to next symbol in Follow-set in case of error.
parseConstraint :: SPMParser Condition
parseConstraint espm 
  | hasToken espm = 
     case headToken espm of
        KW_Satisfies -> combineSPMs (\tab1 (rel, tab2) -> 
                                            FK (tab1,0) (NotSpec rel) (tab2,0))
                                    parseIdentifier
                                    (combineSPMs (\ rel tab -> (rel,tab))
                                                 parseIdentifier
                                                 parseIdentifier)
                                    (continue espm)
        KW_Exists  -> (((terminalOrProc LParen followC) .~>. 
                             parseSubquery)
                            .<~. (terminal RParen)) 
                           (continue espm)
        KW_Not     -> liftSPM (\cons -> Not cons) 
                              parseConstraint 
                              (continue espm)
        LParen     -> ((bindDefSPM parseConstraint 
                                   NoCond
                                   parseConstraints
                                   followC) 
                                   .<~. (terminal RParen)) 
                       (continue espm)
        _          -> bindDefSPM parseOperand defVal parseOperator followO espm
  | otherwise = emptyTkErr espm
  where followC = [RParen, KW_Then, KW_Cross, KW_Inner, KW_Group,
                   KW_Order, KW_Limit, Semi]++logOps++setOps 
        defVal = (Right AbsNull)
        followO = [In, Is , KW_Not, Between]++binOps

-- Parser for a subquery following the exists keyword.
parseSubquery :: SPMParser Condition
parseSubquery espm = (liftSPM (\query -> Exists query) 
                              ((terminalOrProc KW_Select follow) 
                                                 .~>. parseSelect)
                              espm)
  where follow = [ RParen, KW_Then, KW_Cross, KW_Inner, KW_Group,
                   KW_Order, KW_Limit, Semi]++logOps++setOps

-- Parser for an operand. First checks whether operand
-- is a column or a value and invokes the corresponding
-- parser routine.
parseOperand :: SPMParser Operand
parseOperand espm 
  | hasToken espm =  
     case headToken espm of
            (Ident _ ) -> liftSPM (\col -> Left col) 
                                  parseColumn 
                                  espm
            _          -> liftSPM (\ val -> Right val)
                                  parseValue 
                                  espm
  |otherwise = emptyTkErr espm


  
-- Parser for all different kind of constant values.
-- Jumps to next symbol in follow-set
-- in case of an error.
parseValue :: SPMParser Value
parseValue espm 
 | hasToken espm =
   case headToken espm of  
      (EmbedExp exp)               -> initializeSPM (Emb exp Unknown)
                                                    (continue espm)                                  
      (Constant (VarStr str))      -> initializeSPM (StringExp str) 
                                                    (continue espm)
      (Constant (NumInt int))      -> initializeSPM (IntExp int) 
                                                    (continue espm)
      (Constant (NumFloat float))  -> initializeSPM (FloatExp float) 
                                                    (continue espm)
      (Constant (Boolean bool))    -> initializeSPM (BoolExp bool) 
                                                    (continue espm)
      (Constant (Date date))       -> initializeSPM (DateExp date) 
                                                    (continue espm) 
      (Constant (VarChar char))    -> initializeSPM (CharExp char) 
                                                    (continue espm)
      (Constant Null)              -> initializeSPM AbsNull (continue espm)
      _                            -> parseError ("No valid constant value "
                                                   ++"found, but got: " 
                                                   ++ (tokentoString 
                                                           (headToken espm)))
                                                 (proceedWithOneOf follow espm)
 | otherwise = emptyTkErr espm
  where follow = [RParen, Comma, KW_Then, KW_Else, KW_End, Is, 
                  In, Between, KW_Not, KW_Inner, KW_Cross, KW_Where,
                  KW_Group, KW_Order, KW_Limit]++setOps++logOps++binOps

-- Combines identifier to a column reference.
--Jumps to next symbol in Follow-set in case of error.
parseColumn :: SPMParser ColumnRef
parseColumn espm = bindSPM parseIdentifier parseColumnRef follow espm
  where follow = [RParen, Comma, KW_Asc, KW_Desc, In, Is, KW_Not,
                   Between, KW_Else, KW_End]++logOps++binOps

-- Checks if the first identifier is the column name or a tablename.
-- In case a tablename or alias is given it will be inserted in the 
-- ColumnRef-Node otherwise the default name "table" will bi inserted.
parseColumnRef :: String -> SPMParser ColumnRef
parseColumnRef ident espm
  | hasToken espm =
     case headToken espm of
       Stop -> liftSPM (\col -> Column (Unique ident) col Unknown False 0)
                       parseIdentifier
                       (continue espm)
       _    -> initializeSPM (Column (Def ["table"]) ident Unknown False 0)
                              espm                                                       

-- Parser for an identifier.      
parseIdentifier :: SPMParser String
parseIdentifier espm
  | hasToken espm =
      case headToken espm of
            (Ident ident) -> initializeSPM ident (continue espm)
            _             -> parseError ("Expected identifier but got "++
                                          (tokentoString $ headToken espm))
                                        espm
  | otherwise = emptyTkErr espm

-- Parser for the second part of a binary-operation. Selects which operator
-- is applied, invokes corresponding parser routine and combines the results.
parseOperator :: Operand -> SPMParser Condition
parseOperator operand espm 
  | hasToken espm =  
     case headToken espm of
         Between -> combineSPMs (\op2 op3 -> ABetween operand op2 op3) 
                                parseOperand
                                ((terminalOrProc (LogOp And) follow) 
                                                  .~>. parseOperand)
                                (continue espm) 
         Is      -> ((terminalOrProc (Constant Null) follow) .~>. 
                      (initializeSPM (IsNull operand))) (continue espm) 
         In      -> liftSPM (\vals -> AIn operand vals)
                            parseValueList  
                            (continue espm)
         KW_Not  -> ((terminalOrProc (Constant Null) follow) .~>. 
                      (initializeSPM (NotNull operand))) (continue espm)
         _       -> combineSPMs (\op op2 -> ABinOp op operand op2 ) 
                                parseBinOperator
                                parseOperand
                                espm  
  |otherwise = emptyTkErr espm
  where follow = [ RParen, KW_Then, KW_Cross, KW_Inner,
                  KW_Group, KW_Order, KW_Limit, Semi]++logOps++setOps 

-- Parser for a list of values, used in the insert statement and with
-- the "In"-operator.
--Jumps to next symbol in Follow-set in case of error.
parseValueList :: SPMParser [Value]
parseValueList espm 
  | hasToken espm = 
       case headToken espm of
               LParen -> combineSPMs (:) parseValue
                                         parseValueList
                                         (continue espm)
               Comma  -> combineSPMs (:) parseValue
                                         parseValueList
                                         (continue espm)
               RParen -> initializeSPM [] (continue espm)
               _      -> parseError ("Expected a list of values seperated " ++
                                     "by comma and surrounded by parenthesis, got" 
                                      ++ (tokentoString $ headToken espm)) 
                                    (proceedWithOneOf follow espm)
  |otherwise = emptyTkErr espm    
 where follow =  [RParen, KW_Then, KW_Inner, KW_Cross, Comma, 
                  KW_Group, KW_Order, KW_Limit]++setOps

--Parser for all simple kind of binary operator. 
--Jumps to next symbol in Follow-set in case of error.
parseBinOperator :: SPMParser AstOp
parseBinOperator espm  
  | hasToken espm =
     case headToken espm of
                (BinOp Lth)   -> initializeSPM ALth (continue espm)
                (BinOp Gth)   -> initializeSPM AGth (continue espm) 
                (BinOp Lte)   -> initializeSPM ALe (continue espm)
                (BinOp Gte)   -> initializeSPM AGe (continue espm)
                (BinOp Equal) -> initializeSPM AEq (continue espm)
                (BinOp Uneq)  -> initializeSPM AUnEq (continue espm)
                (BinOp Like)  -> initializeSPM ALike (continue espm) 
                (Ident _)     -> parseError ("No Valid BinaryOperator found," 
                                             ++ " but got: " ++ 
                                            (tokentoString $ headToken espm)) 
                                             espm
                (Constant _) -> parseError ("No Valid BinaryOperator found," 
                                             ++ " but got: " ++ 
                                            (tokentoString $ headToken espm)) 
                                             espm
                (EmbedExp _) -> parseError ("No Valid BinaryOperator found," 
                                             ++ " but got: " ++ 
                                            (tokentoString $ headToken espm)) 
                                             espm
                _            -> parseError ("No Valid BinaryOperator found," 
                                             ++ " but got: " ++ 
                                            (tokentoString $ headToken espm)) 
                                             (continue espm)
  |otherwise = emptyTkErr espm
                
--auxiliary definitions to build up follow sets --------------------------
setOps :: [Token]
setOps = [(SetOp Union), (SetOp Intersect), (SetOp Except)]

logOps :: [Token]
logOps = [(LogOp And), (LogOp Or)]

binOps :: [Token]
binOps = [(BinOp Lth), (BinOp Gth), (BinOp Lte), (BinOp Gte), (BinOp Equal),
          (BinOp Uneq), (BinOp Like)]
