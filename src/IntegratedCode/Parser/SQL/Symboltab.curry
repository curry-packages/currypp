--- This module defines a Symboltable optimized for the SQLNamer.
--- It contains two FiniteMaps, both using strings as keys and <
--- as comparison operation. It is parameterized over both value types.
--- Only the first Map is supporting a scope concept.
---@author Julia Krone
---@version 0.1
-- -----------------------------------------------------------------------

module Symboltab where

import FiniteMap 

--- A Symboltable consists of at least one pair of FiniteMaps.
--- There can be another table representing a surrounding scope.
data Symboltable a b = ST ((FM String a),(FM String b)) 
                          (Maybe (Symboltable a b))

---Constructor function.
emptyTable :: Symboltable _ _
emptyTable = ST ((emptyFM (<)),(emptyFM (<))) Nothing

--- Lookupfunction: First looks up the key in the current scope (first table)
--- if it is not found the surrounding scope will be used and so on.
--- Returns Nothing if no value was found.
lookupFirstTable :: String -> Symboltable a b -> Maybe a
lookupFirstTable k (ST (m,_) Nothing ) = lookupFM m k
lookupFirstTable k (ST (m,_) (Just s)) = case lookupFM m k of
                                       Just v -> Just v
                                       Nothing -> lookupFirstTable k s

--- Looks up the key just in the current scope of the first table.                                       
lookupCurrentScope :: String -> Symboltable a b -> Maybe a
lookupCurrentScope k (ST (m,_) _) = lookupFM m k
 
--- Looks up the key in the second table (which does not support scopes).                                      
lookupSecondTable :: String -> Symboltable a b -> Maybe b
lookupSecondTable k (ST (_,n) _ ) = lookupFM n k                                     

--- Inserts a key-value-pair into the current scope (first table).                                       
insertFirstTable :: String -> a -> Symboltable a b -> Symboltable a b
insertFirstTable k v (ST (m,n) ms) = ST ((addToFM m k v),n) ms

--- Inserts a key-value-pair into the current scope (first table)
--- without throwing away previous bindings. Values are combined
--- by given combinator.
insertDefFirstTab :: String -> 
                     a -> 
                     (a -> a -> a) ->
                     Symboltable a b ->
                     Symboltable a b
insertDefFirstTab k v c (ST (m,n) ms) = ST ((addToFM_C c m k v),n) ms

--- Inserts a key-value-pair into the second table 
--- (which does not support scopes).
insertSecondTable :: String -> b -> Symboltable a b -> Symboltable a b
insertSecondTable k v (ST (m,n) ms) = ST (m,(addToFM n k v)) ms

--- Create a new scope inside the last one 
--- (first table, leaving the secong one unchanged).
enterScope :: Symboltable a b  -> Symboltable a b
enterScope s@(ST (_,n) _ ) = ST ((emptyFM (<)),n) (Just s)

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
     ST ((plusFM m1 m2), (plusFM n1 n2)) ms



