------------------------------------------------------------------------------
--- Auxiliary operations for the preprocessor.

module CPP.Helpers where

------------------------------------------------------------------------------
--- Checks whether a module is in the import list.
--- Aborts with an error if this is not the case.
checkRequiredImport :: String -> String -> [String] -> IO ()
checkRequiredImport currmod reqmod imps =
 if reqmod `elem` imps
   then return ()
   else do
     putStrLn $ unlines $
       [ "WARNING: Module '" ++ currmod ++ "': '" ++ reqmod ++
         "' is required but not imported!"
       , "Please add it to the list of imports to avoid compilation problems."
       , "In the meantime, I try to insert this import..."
       ]
     --error "Transformation aborted"

--- Name of the set functions module.
setFunMod :: String
setFunMod = "Control.SetFunctions"

------------------------------------------------------------------------------
