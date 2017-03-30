{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=seqrules #-}

import Test.EasyCheck

-- Dijsktra's Dutch National Flag problem with functional patterns

data Color = Red | White | Blue

-- Formulation with sequential rule application:
solveD (x++[White]++y++[Red  ]++z) = solveD (x++[Red]++y++[White]++z)
solveD (x++[Blue ]++y++[Red  ]++z) = solveD (x++[Red]++y++[Blue]++z)
solveD (x++[Blue ]++y++[White]++z) = solveD (x++[White]++y++[Blue]++z)
solveD flag = flag

uni color = [] ? color : uni color

iflag = [White,Red,White,Blue,Red,Blue,White]

main = solveD iflag
 --> [Red,Red,White,White,White,Blue,Blue]
 -- but also many more (identical) solutions!

test_solveD =  solveD iflag  <~>  [Red,Red,White,White,White,Blue,Blue]

-------------------------------------------------------------------------------
-- Sergio's version to obtain a single solution:
dutch (r@(uni Red) ++ w@(uni White) ++ b@(uni Blue) ++ (Red:xs))
  | (w++b==[])=:=False = dutch (Red:r ++ w ++ b ++ xs)
dutch (r@(uni Red) ++ w@(uni White) ++ b@(uni Blue) ++ (White:xs))
  | (b==[])=:=False = dutch (r ++ White:w ++ b ++ xs)
dutch z = z

testDutch =  dutch iflag  -=-  [Red,Red,White,White,White,Blue,Blue]
