--- This module defines a Symboltable optimized for the SQLNamer.
--- It contains two Maps, both using strings as keys and <
--- as comparison operation. It is parameterized over both value types.
--- Only the first Map is supporting a scope concept.
---@author Julia Krone
---@version 0.1
-- -----------------------------------------------------------------------

module CPP.ICode.Parser.SQL.Symboltab where

import qualified Data.Map as Map 

--- A Symboltable consists of at least one pair of Maps.
--- There can be another table representing a surrounding scope.
data Symboltable a b = ST ((Map.Map String a),(Map.Map String b))
                          (Maybe (Symboltable a b))

---Constructor function.
emptyTable :: Symboltable _ _
emptyTable = ST (Map.empty, Map.empty) Nothing

--- Lookupfunction: First looks up the key in the current scope (first table)
--- if it is not found the surrounding scope will be used and so on.
--- Returns Nothing if no value was found.
lookupFirstTable :: String -> Symboltable a b -> Maybe a
lookupFirstTable k (ST (m,_) Nothing ) = Map.lookup k m
lookupFirstTable k (ST (m,_) (Just s)) = case Map.lookup k m of
                                       Just v -> Just v
                                       Nothing -> lookupFirstTable k s

--- Looks up the key just in the current scope of the first table.
lookupCurrentScope :: String -> Symboltable a b -> Maybe a
lookupCurrentScope k (ST (m,_) _) = Map.lookup k m

--- Looks up the key in the second table (which does not support scopes).
lookupSecondTable :: String -> Symboltable a b -> Maybe b
lookupSecondTable k (ST (_,n) _ ) = Map.lookup k n

--- Inserts a key-value-pair into the current scope (first table).
insertFirstTable :: String -> a -> Symboltable a b -> Symboltable a b
insertFirstTable k v (ST (m,n) ms) = ST ((Map.insert k v m),n) ms

--- Inserts a key-value-pair into the current scope (first table)
--- without throwing away previous bindings. Values are combined
--- by given combinator.
insertDefFirstTab :: String ->
                     a ->
                     (a -> a -> a) ->
                     Symboltable a b ->
                     Symboltable a b
insertDefFirstTab k v c (ST (m,n) ms) = ST ((Map.insertWith c k v m),n) ms

--- Inserts a key-value-pair into the second table
--- (which does not support scopes).
insertSecondTable :: String -> b -> Symboltable a b -> Symboltable a b
insertSecondTable k v (ST (m,n) ms) = ST (m,(Map.insert k v n)) ms

--- Create a new scope inside the last one
--- (first table, leaving the secong one unchanged).
enterScope :: Symboltable a b  -> Symboltable a b
enterScope s@(ST (_,n) _ ) = ST (Map.empty ,n) (Just s)

--- Exits current scope, so that the surrounding one will be used.
--- If the current scope is the most general one, nothing will be changed.
exitScope :: Symboltable a b -> Symboltable a b
exitScope s@(ST _ Nothing ) = s
exitScope (ST _   (Just s)) = s

--- Combines two Symboltables. The current Scopes will be merged,
--- all the remaining scopes are taken from the first table.
--- Bindings for the same key will be overwritten by the
--- binding in the current scope.
combineST :: Symboltable k v -> Symboltable k v -> Symboltable k v
combineST (ST (m1,n1) ms) (ST (m2,n2) _) =
     ST ((Map.union m2 m1), (Map.union n2 n1)) ms
