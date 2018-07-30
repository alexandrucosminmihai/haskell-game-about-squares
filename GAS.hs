{-
    Alexandru Cosmin Mihai, 2018
    Game About Squares implementation in Haskell 
    including solving mechanism for any given input game map
-}

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import Data.Maybe

import qualified Data.Map.Strict as M


{-
    Position on the game table, having the (line, column) format where both
    coordinates may be negative
-}
type Position = (Int, Int)

-- The colors of the squares and the circles
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

-- The heading (direction) of squares and arrows
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

-- An object from the game map: square / circle / arrow
data Object = Square Position Color Heading | Circle Position Color | Arrow Position Heading
    deriving (Eq, Ord)

-- Textual representation of an object.
instance Show Object where
    show (Square position colour heading)
        |colour == Red = "R" ++ (show heading) ++ " "
        |colour == Blue = "B" ++ (show heading) ++ " "
        |otherwise = "G" ++ (show heading) ++ " "

    show (Circle position colour) 
        |colour == Red = "  r"
        |colour == Blue = "  b"
        |otherwise = "  g"

    show (Arrow position heading) = "  " ++ (show heading)

-- A game level
data Level = LevelFromMap (M.Map Position [Object]) -- a level is represented by a Map with positions as keys and objects as values
    deriving (Eq, Ord)

getColumnsInfo :: Level -> (Int, Int, Int)  -- returns the number of columns from the map representation, the min column index, the max column index
-- I use a foldl to iterate through all the keys and store insinde an accumulator the (min, max) columns indices
-- (min, max) is initialized to (firstColumn, firstColumn)
getColumnsInfo (LevelFromMap someMap) = (colMax - colMin + 1, colMin, colMax)
    where themKeys = M.keys someMap
          fMinMax acc pereche = ( (min (fst acc) (snd pereche)), (max (snd acc) (snd pereche)) )
          (colMin, colMax) = foldl fMinMax (snd $ head $ themKeys, snd $ head $ themKeys) themKeys

fillingCols :: Int -> String -- creates empty columns (cells on a row) to fill rows
fillingCols no 
    |no <= 0 = ""
    |otherwise = concat $ take no (repeat "   |")

-- Creates empty columns (cells on a row) to fill rows
fillingRows :: Int -> Int -> String x
fillingRows no colNo
    |no <= 0 || colNo <= 0 = ""
    |otherwise = concat $ take no (repeat ((fillingCols (colNo - 1)) ++ "   \n"))

-- Creates empty columns, but takes into consideration the fact that these columns will be placed at the end of a row
fillingColsEnd :: Int -> String
fillingColsEnd no 
    |no <= 0 = ""
    |otherwise = fillingCols (no - 1) ++ "   "

-- Displays a list of objects
showObjs :: [Object] -> String
showObjs objs 
    |length objs == 1 = show (head objs) -- when there is only one element in the objects list
    |otherwise = zipWith coverSpace (show (head objs)) (show (head (tail objs))) -- combines the two strings (lists of Chars) by "overlapping them"
    where coverSpace c1 c2 -- cel putin unul dintre caractere este spatiu
            |c1 == ' ' = c2
            |otherwise = c1

-- Textual representation of a level.
instance Show Level where
    {-
        Go through all the entries from the map useing a foldl. I store in a pair-accumulator the key of the last 
        visited entry (i.e. the position of the last object that I displayed) which I initially set to (firstLine, colMin - 1)
        and, as the second element inside the pair-accumulator, the String constructed until this moment
    -}
    show (LevelFromMap someMap) = (snd res) ++ fillingLastLine
        where themKeys = M.keys someMap
              firstLine = fst $ head $ themKeys
              lastLine = fst $ head $ reverse $ themKeys
              (colNo, colMin, colMax) = getColumnsInfo (LevelFromMap someMap)
              fToString (lastPos, accString) currPos currObjs 
                |( (fst currPos) == (fst lastPos) ) && ( (snd currPos) == colMax ) = (currPos, accString ++ (fillingCols ((snd currPos) - (snd lastPos) - 1)) ++ (showObjs currObjs)) -- current object is on the same row as the last object
                |( (fst currPos) == (fst lastPos) ) && ( (snd currPos) /= colMax ) = (currPos, accString ++ (fillingCols ((snd currPos) - (snd lastPos) - 1)) ++ (showObjs currObjs) ++ "|")
                |snd currPos == colMax = (currPos, accString ++ (fillingColsEnd (colMax - (snd lastPos))) ++ "\n" ++ (fillingRows ((fst currPos) - (fst lastPos) - 1) colNo) ++ (fillingCols ((snd currPos) - colMin)) ++ (showObjs currObjs) ) -- current object is on a lower row. I fill the row of the last object, I fill the empty rows between them and I add empty cells until I reach the current object's column
                |otherwise = (currPos, accString ++ (fillingColsEnd (colMax - (snd lastPos))) ++ "\n" ++ (fillingRows ((fst currPos) - (fst lastPos) - 1) colNo) ++ (fillingCols ((snd currPos) - colMin)) ++ (showObjs currObjs) ++ "|" )
              res = M.foldlWithKey fToString ((firstLine, colMin - 1), []) someMap
              fillingLastLine
                |snd (fst res) /= colMax = (fillingCols (colMax - (snd (fst res)) - 1)) ++ "   "
                |otherwise = ""

-- Empty level (has no objects)
emptyLevel :: Level
emptyLevel = LevelFromMap (M.empty)

-- Adds a square with the given characteristics at the given position inside the level
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare color heading position (LevelFromMap someMap)
    |alreadyThere == Nothing = LevelFromMap $ M.insert position [(Square position color heading)] someMap
    |otherwise = LevelFromMap $ M.insert position ( (Square position color heading):(fromJust alreadyThere) ) someMap
    where alreadyThere = M.lookup position someMap

-- Adds a circle with the given characteristics at the given position inside the level
addCircle :: Color -> Position -> Level -> Level
addCircle color position (LevelFromMap someMap)
    |alreadyThere == Nothing = LevelFromMap $ M.insert position [(Circle position color)] someMap
    |otherwise = LevelFromMap $ M.insert position ( (Circle position color):(fromJust alreadyThere) ) someMap
    where alreadyThere = M.lookup position someMap

-- Adds an arrow with the given characteristics at the given position inside the level
addArrow :: Heading -> Position -> Level -> Level
addArrow heading position (LevelFromMap someMap)
    |alreadyThere == Nothing = LevelFromMap $ M.insert position [(Arrow position heading)] someMap
    |otherwise = LevelFromMap $ M.insert position ( (Arrow position heading):(fromJust alreadyThere) ) someMap
    where alreadyThere = M.lookup position someMap

moveWithHeading :: Position -> Level -> Heading -> Level
{-
    If there is a square on the given position, I must make a move:
     I check the position on which the square should end up
     If on that position is an Arrow, it will change the orientation of my square
        => I will add to theRest, from which I remove the current square, a new square with a modified orientation
     Otherwise,
        => I will add to theRest, from which I remove the current square, a square with the same orientation, on the new position
-}
moveWithHeading (l, c) (LevelFromMap someMap) heading
    |lookupVal == Nothing = LevelFromMap someMap
    |containsSquare (fromJust lookupVal) = addSquare sqcolor newHeading nextPos (removeSquare (l, c) theRest) -- when there is a square on the given position
    |otherwise = LevelFromMap someMap
    where lookupVal = (M.lookup (l, c) someMap) 
          nextPos
            |heading == South = (l + 1, c)
            |heading == East = (l, c + 1)
            |heading == North = (l - 1, c)
            |otherwise = (l, c - 1)
          theRest = moveWithHeading nextPos (LevelFromMap someMap) heading -- I make sure that, if there is another square on the next position, it gets pushed
          (Square sqposition sqcolor sqheading) = fromJust (getSquare (fromJust (M.lookup (l, c) someMap)))
          nextCell = (M.lookup nextPos someMap) -- the contents of the cell that the current square will move on
          newHeading
            |nextCell == Nothing = sqheading
            |containsArrow (fromJust nextCell) = getHeadingFromArrow (fromJust (getArrow (fromJust nextCell)))
            |otherwise = sqheading


removeSquareFromList :: [Object] -> [Object]
removeSquareFromList objs = foldl (\ acc obj -> if (isSquare obj) then acc else (obj:acc)) [] objs

removeSquare :: Position -> Level -> Level
removeSquare position (LevelFromMap someMap)
    |lookupVal == Nothing = LevelFromMap someMap -- there is nothing from which I could remove a square
    {-
        If in the given cell there is only one square and I remove the square 
        => there is nothing left inside the Map for that position
    -}
    |(length (fromJust lookupVal) == 1) && (containsSquare (fromJust lookupVal)) = LevelFromMap (M.delete position someMap)
    |otherwise = LevelFromMap (M.insert position (removeSquareFromList (fromJust lookupVal)) (M.delete position someMap))
    where lookupVal = (M.lookup position someMap)

getHeadingFromArrow :: Object -> Heading
getHeadingFromArrow (Arrow position heading) = heading

getArrow :: [Object] -> Maybe Object
getArrow objs = foldl (\ acc obj -> if (isArrow obj) then Just obj else acc) Nothing objs

isArrow :: Object -> Bool
isArrow (Arrow _ _) = True
isArrow _ = False

containsArrow :: [Object] -> Bool
containsArrow objs = foldl (||) False (map isArrow objs)

isSquare :: Object -> Bool
isSquare (Square _ _ _) = True
isSquare _ = False

containsSquare :: [Object] -> Bool
containsSquare objs = foldl (||) False (map isSquare objs)

getSquare :: [Object] -> Maybe Object
getSquare objs = foldl (\ acc obj -> if (isSquare obj) then Just obj else acc) Nothing objs

isCircle :: Object -> Bool
isCircle (Circle _ _) = True
isCircle _ = False

containsCircle :: [Object] -> Bool
containsCircle objs = foldl (||) False (map isCircle objs)

getCircle :: [Object] -> Maybe Object
getCircle objs = foldl (\ acc obj -> if (isCircle obj) then Just obj else acc) Nothing objs

-- Move the square from the given position and the given level
move :: Position  -- Position to move square from
     -> Level     -- Initial level, given as an argument
     -> Level     -- Final level, returned by the function
move position (LevelFromMap someMap) 
    |lookupVal == Nothing = (LevelFromMap someMap)
    |containsSquare (fromJust lookupVal) = moveWithHeading position (LevelFromMap someMap) heading
    |otherwise = (LevelFromMap someMap)
    where lookupVal = (M.lookup position someMap) 
          (Square sqposition sqcolor heading) = fromJust (getSquare (fromJust lookupVal))

validSquareCirclePair :: [Object] -> Bool
validSquareCirclePair objs
    |containsCircle objs == False = False
    |sqColor == cirColor = True
    |otherwise = False
    where Square _ sqColor _ = fromJust (getSquare objs)
          Circle _ cirColor = fromJust (getCircle objs)

getLinesNo :: Level -> Int
getLinesNo (LevelFromMap someMap)
    |M.size someMap <= 1 = M.size someMap
    |otherwise = (fst (fst (head (M.toList someMap)))) - (fst (fst (head (reverse (M.toList someMap))))) + 1

instance ProblemState Level Position where
    successors (LevelFromMap someMap) = zip sqPositions genLevels
        where
            {- 
            The filter returns a Map that contains entries for which the value is a list of objects that contains a square. 
            Then we transform the Map in a list and we pick only the keys in order to get a list of square positions
            -}
            sqPositions = map fst (M.toList (M.filter containsSquare someMap))
            genLevels = map ((flip move) (LevelFromMap someMap)) sqPositions

    
    -- Check all positions that hold a square if they also hold a circle having the same colour as the square.
    isGoal (LevelFromMap someMap) = foldl (&&) True (map validSquareCirclePair (map (fromJust . ((flip M.lookup) someMap)) sqPositions))
        where
            sqPositions = map fst (M.toList (M.filter containsSquare someMap))

    {-
        I use the dimension of the game table as a heuristic in order to improve the search for the solution. 
        The final state of the game must have the dimensions given by the positions of the objects that are not squares,
        these dimensions being the lower bound for the dimensions of the game table (because the table resizez
        to accomodate all the pieces in the game)
    -}
    heuristic (LevelFromMap someMap) = noCol * noLin 
        where (noCol, _, _) = getColumnsInfo (LevelFromMap someMap)
              noLin = getLinesNo (LevelFromMap someMap)
