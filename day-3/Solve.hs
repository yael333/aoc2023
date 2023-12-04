import Data.Char (isDigit, isSpace)
import Text.Parsec.Token (GenTokenParser (symbol))

type Position = (Int, Int)

type Range = (Position, Position)

data Cell
  = Number {getPosition :: Position, getNumber :: Int}
  | Symbol {getPosition :: Position, getSymbol :: Char}
  deriving (Show, Eq, Ord)

isSymbol, isNumber :: Cell -> Bool
isSymbol Symbol {} = True
isSymbol _ = False
isNumber Number {} = True
isNumber _ = False

getWidth :: Cell -> Int
getWidth (Number _ n) = length $ show n
getWidth (Symbol _ _) = 1

isPositionInRange :: Position -> Position -> Range -> Bool
isPositionInRange (px1, py1) (px2, py2) ((rx1, rx2), (ry1, ry2)) =
  (px2 >= px1 - rx1 && px2 <= px1 + rx2) && py2 >= py1 - ry1 && py2 <= py1 + ry2

parseLine :: Int -> String -> [Cell]
parseLine y line = go line 0 []
  where
    go [] _ acc = reverse acc
    go cs@(c : t) x acc
      | isDigit c =
          let (numStr, rest) = span isDigit cs
              num = read numStr
           in go rest (x + length numStr) (Number (x, y) num : acc)
      | not (isSpace c || c == '.') =
          go t (x + 1) (Symbol (x, y) c : acc)
      | otherwise = go t (x + 1) acc

parseInput :: String -> [Cell]
parseInput input = concat $ zipWith parseLine [0 ..] (lines input)

findNeighbors :: [Cell] -> Cell -> (Cell -> Cell -> Range) -> [Cell]
findNeighbors cells cell range =
  [ cell'
    | cell' <- cells,
      cell' /= cell,
      isPositionInRange (getPosition cell) (getPosition cell') $ range cell cell'
  ]

partOneRange :: Cell -> Cell -> Range
partOneRange c1 c2 = ((1, getWidth c1), (1, 1))

partTwoRange :: Cell -> Cell -> Range
partTwoRange c1 c2 =
  let xRange =
        if fst (getPosition c1) < fst (getPosition c2)
          then (1, 1)
          else (getWidth c2, 1)
   in (xRange, (1, 1))

main :: IO ()
main = do
  scheme <- parseInput <$> readFile "input.txt"
  let numbers = filter isNumber scheme
  let symbols = filter isSymbol scheme

  -- PART 1
  let symbolNeighbors = map (\c -> findNeighbors symbols c (\c1 c2 -> ((1, length $ show $ getNumber c), (1, 1)))) numbers
  print $ sum $ map (getNumber . fst) $ filter (not . null . snd) $ zip numbers symbolNeighbors

  -- PART 2
  let numberNeighbors = map (\c -> findNeighbors numbers c partTwoRange) $ filter ((== '*') . getSymbol) symbols
  print $ sum $ map (product . map getNumber) $ filter ((== 2) . length) numberNeighbors