------------------------------------------------------------------------------
--- Handling parse positions
---
--- @author Jasper Sikorra - jsi@informatik.uni-kiel.de
--- @version January 2014
------------------------------------------------------------------------------
module CPP.ICode.ParsePos where

-- Tab width
tw :: Int
tw = 8

-- Unkown filename
unknown_fname :: String
unknown_fname = "Unknown file"

type Filename = String
type Absolute = Int
type Line     = Int
type Column   = Int

-- A reduced representation of the position
type SimplePos = (Int,Int)

--- The Pos data type contains the name of the file, the absolute,
--- where every character is counted as 1 (newlines, tabs too) 
--- (starting with 0), the line (starting with 1) and the column (starting
--- with 1) of the character's position.
data Pos = Pos Filename Absolute Line Column

--- Construct a new position
setPos :: Filename -> Absolute -> Line -> Column -> Pos
setPos fname abs line col = Pos fname abs line col

--- Construct the initial position
initPos :: Filename -> Pos
initPos fname = Pos fname 0 1 1

getFilename :: Pos -> Filename
getFilename (Pos f _ _ _) = f

setFilename :: Pos -> Filename -> Pos
setFilename (Pos _ a x y) fname = Pos fname a x y

getAbs :: Pos -> Absolute
getAbs (Pos _ a _ _) = a

setAbs :: Pos -> Absolute -> Pos
setAbs (Pos f _ x y) abs = Pos f abs x y

moveAbs :: Pos -> Absolute -> Pos
moveAbs (Pos f a x y) n = Pos f (a+n) x y

getLn :: Pos -> Line
getLn (Pos _ _ x _) = x

setLn :: Pos -> Line -> Pos
setLn (Pos f a _ y) lin = Pos f a lin y

moveLn :: Pos -> Line -> Pos
moveLn (Pos f a x y) n = Pos f a (x+n) y

getCol :: Pos -> Column
getCol (Pos _ _ _ y) = y

setCol :: Pos -> Column -> Pos
setCol (Pos f a x _) col = Pos f a x col

moveCol :: Pos -> Column -> Pos
moveCol (Pos f a x y) n = Pos f a x (y+n)

--- The difference in lines between two positions
lnDifference :: Pos -> Pos -> Line
lnDifference p1 p2 = getLn p2 - getLn p1

--- The difference in columns between two positions
colDifference :: Pos -> Pos -> Column
colDifference p1 p2 = getCol p2 - getCol p1

--- The absolute difference between two positions
absDifference :: Pos -> Pos -> Absolute
absDifference p1 p2 = getAbs p2 - getAbs p1

--- The line and column difference between two positions
fullDifference :: Pos -> Pos -> (Line,Column)
fullDifference p1 p2 = (lnDifference p1 p2,colDifference p1 p2)

--- Move the position one character
movePosByChar :: Pos -> Char -> Pos
movePosByChar (Pos f a x y) c | c == '\n' = Pos f (a+1) (x+1) 1
                              | c == '\t' = Pos f (a+1) x (y+tw-mod (y-1) tw)                                           
                              | otherwise = Pos f (a+1) x     (y+1)

--- Move the position multiple characters
movePosByString :: Pos -> String -> Pos
movePosByString p ""     = p
movePosByString p (c:cs) = movePosByString (movePosByChar p c) cs

--- Convert to SimplePos
toSimplePos :: Pos -> SimplePos
toSimplePos p = (getLn p,getCol p)

--- Convert from SimplePos
fromSimplePos :: SimplePos -> Pos
fromSimplePos p = fromSimplePosWithFname p unknown_fname

--- Convert from SimplePos with Filename
fromSimplePosWithFname :: SimplePos -> Filename -> Pos
fromSimplePosWithFname (x,y) fn = setPos fn (x+y) x y
