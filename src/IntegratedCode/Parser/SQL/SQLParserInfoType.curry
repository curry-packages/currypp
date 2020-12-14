--- This module defines the structure of the parser information
--- used to pass information of the data model from an ERD-term to
--- the SQL-Translator which is part of currypp.
--- It consists of a tuple of the path to the corresponding database as string,
--- and the name of the corresponding CDBI-module as String,
--- a list or tupels mapping relationships to their types,
--- a list of tupels representing column names and a nullable flag,
--- a list of tupels of table name and list of corresponding column names,
--- a list of tupels representing column name and its type as string.
--- Furthermore a getter function for each part of the information
--- is defined returning it as FM instead of a list. A lookup function for
--- relationships is provided either.
---@author: Julia Krone
-- ----------------------------------------------------------------------------
module SQLParserInfoType
 (dbName, getRelations, getNullables, getAttrList, RelationType(..),
  ParserInfo(..), getTypes, lookupRel, RelationFM, cdbiModule,
  NullableFM, AttributesFM, AttrTypeFM)
where

import Data.Char ( toLower )
import Data.List ( partition )

import qualified Data.Map as Map

--- Type synonyms for all parts of the parser information
type RelationFM = Map.Map String (Map.Map String [(String, RelationType)])
type RelationTypes = [((String, String, String), RelationType)]

type NullableFlags =  [(String, Bool)]
type NullableFM = Map.Map String Bool

type AttributeLists =  [(String,(String, [String]))]
type AttributesFM = Map.Map String (String, [String])

type AttributeTypes = [(String, String)]
type AttrTypeFM = Map.Map String String

--- Basic data type for parser information.
data ParserInfo = PInfo String
                        String
                        RelationTypes
                        NullableFlags
                        AttributeLists
                        AttributeTypes

--- Data type representing possible relationships in the relational model.
data RelationType = MtoN String
                   | NtoOne String
                   | OnetoN String

--- Getter for path to used database.
dbName :: ParserInfo -> String
dbName (PInfo db _ _ _ _ _ ) = db

--- Getter for CDBI-module-name
cdbiModule :: ParserInfo -> String
cdbiModule (PInfo _ cdbi _ _ _ _) = cdbi

--- Getter for relationships represented as nested FM for faster access.
getRelations :: ParserInfo -> RelationFM
getRelations (PInfo _ _ rels _ _ _) = splitRelations rels

--- Getter for nullable-flag returned as a FM
getNullables :: ParserInfo -> NullableFM
getNullables (PInfo _ _ _ nulls _ _) = Map.fromList nulls

--- Getter for for lists of attributes (columns)
--- returned as FM.
getAttrList :: ParserInfo -> AttributesFM
getAttrList (PInfo _ _ _ _ attrs _ ) = Map.fromList attrs

--- Getter for column types returnd as FM.
getTypes :: ParserInfo -> AttrTypeFM
getTypes (PInfo _ _ _ _ _ types) = Map.fromList types

--- Lookup function for RelationFM.
--- In case of success return tuple of relation type
--- and original notation.
lookupRel :: (String, String, String) ->
             RelationFM ->
             Maybe (RelationType, String)
lookupRel (e1,rel,e2) fm =
  case Map.lookup e1 fm of
    Nothing  -> Nothing
    Just fm2 -> case Map.lookup e2 fm2 of
                   Nothing -> Nothing
                   Just rels -> fetchRel rel rels
  where fetchRel _ [] = Nothing
        fetchRel rName ((relName, relType):rs) =
              if (toLowerCase rName) == (toLowerCase relName)
                then (Just (relType, relName))
                else fetchRel rName rs


-- auxiliary functions for the transformation of a list of tupels to
-- a nested FM

splitRelations ::  [((String, String, String), RelationType)] -> RelationFM
splitRelations []       = Map.empty
splitRelations (r:rels) =
  let (frst, rest) = selectFirstEntity (frsEnt r) (r:rels)
   in Map.union (splitRelations rest) (createFM frst)

frsEnt :: ((String, String, String), RelationType) ->  String
frsEnt ((ent, _ , _ ),_) = ent

sndEnt :: ((String, String, String), RelationType) ->  String
sndEnt (( _ , _ , ent ),_) = ent

selectFirstEntity :: String -> [((String, String, String), RelationType)]
                            -> ([((String,String, String), RelationType)],
                                [((String, String, String), RelationType)])
selectFirstEntity name rels = partition (\(( n, _, _),_) -> n == name) rels

createFM :: [((String, String, String), RelationType)] -> RelationFM
createFM []        = Map.empty
createFM (e:elems) = Map.singleton (frsEnt e) (insertElems (e:elems))

insertElems :: [((String, String, String), RelationType)] ->
               (Map.Map String [(String, RelationType)])
insertElems [] = Map.empty
insertElems (e:elems) = let (snds, rest) = (selectSecondEntity (sndEnt e)
                                                               (e:elems))
                         in Map.union (insertElems rest) snds

selectSecondEntity :: String ->
                      [((String, String, String), RelationType)] ->
                      ((Map.Map String [(String, RelationType)]),
                          [((String, String, String), RelationType)])
selectSecondEntity name rels =
  let (fit, nfit) = partition (\((_ , _, n),_) -> n == name) rels
   in ((createSubFM fit), nfit)
  where createSubFM [] = Map.empty
        createSubFM (((_,r,e2),rt):rs) = Map.singleton
                                                e2
                                                ((r,rt):(map selectRelation rs))

selectRelation :: ((String, String, String), RelationType) ->
                  (String, RelationType)
selectRelation ((_ , r, _), rt) = (r, rt)


toLowerCase :: String -> String
toLowerCase str = map toLower str
