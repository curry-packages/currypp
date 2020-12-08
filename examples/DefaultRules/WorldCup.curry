{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}
{-# OPTIONS_CYMAKE -Wnone #-}

import Test.Prop

-- Example: parse World Cup soccer scores (e.g., "_:_", "3:2")

import Data.Char (isDigit)

parse (team1++" _:_ "++team2) = (team1, team2, Nothing)
parse (team1++[' ',x,':',y,' ']++team2)
  | isDigit x && isDigit y
  = (team1, team2, Just (toInt x,toInt y))
parse'default _ = error "Wrong format!"

toInt :: Char -> Int
toInt c = ord c - ord '0'

main = [parse "GER _:_ USA",
        parse "GER 1:0 USA"]

test1 = parse "GER _:_ USA"  -=-  ("GER","USA",Nothing)
test2 = parse "GER 1:0 USA"  -=-  ("GER","USA",Just (1,0))
