import Control.Monad (liftM2)
import Control.Monad.RWS (Product (Product))
import Data.Function (on)
import Data.List (group, groupBy, maximumBy, sort, sortBy, sortOn)
import System.Exit (die)
import Text.Parsec
import Text.Parsec.String (Parser)

data Color = Red | Green | Blue deriving (Show, Eq, Ord)

data ColorCount = ColorCount Integer Color deriving (Show)

instance Eq ColorCount where
  (ColorCount count1 color1) == (ColorCount count2 color2) = count1 == count2 && color1 == color2

instance Ord ColorCount where
  compare (ColorCount count1 color1) (ColorCount count2 color2) =
    compare (count1, color1) (count2, color2)

type ColorSet = [ColorCount]

data Game = Game Integer [ColorSet] deriving (Show) -- ID, [[ColorCount]]

number :: Parser Integer
number = read <$> many1 digit

color :: Parser Color
color =
  (string "red" >> return Red)
    <|> (string "green" >> return Green)
    <|> (string "blue" >> return Blue)

colorCount :: Parser ColorCount
colorCount = ColorCount <$> number <* space <*> color

colorSet :: Parser ColorSet
colorSet = colorCount `sepBy` string ", "

game :: Parser Game
game = liftM2 Game (string "Game " >> number) (string ": " >> colorSet `sepBy` string "; ")

compareColors :: ColorCount -> ColorCount -> Ordering
compareColors (ColorCount _ a) (ColorCount _ b) = compare a b

equalColors :: ColorCount -> ColorCount -> Bool
equalColors (ColorCount _ a) (ColorCount _ b) = a == b

reduceGame :: Game -> ColorSet
reduceGame (Game _ colorCounts) = map maximum $ groupBy equalColors $ sortBy compareColors (concat colorCounts)

parseGame :: String -> Either ParseError Game
parseGame = parse game ""

compareColorSets :: ColorSet -> ColorSet -> Bool
compareColorSets cs1 cs2 = all smaller $ zip cs1 cs2
  where
    smaller (ColorCount count1 color1, ColorCount count2 color2) = count1 <= count2 && color1 == color2

powerColorSet :: ColorSet -> Integer
powerColorSet s = product $ map (\(ColorCount count _) -> count) s

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  case mapM parseGame input of
    Left err -> die $ show err
    Right games -> do
      print $ sum $ map (\(Game id _) -> id) $ filter (\g -> compareColorSets (reduceGame g) c1) games
      print $ sum $ map (powerColorSet . reduceGame) games
  where
    c1 = sort [ColorCount 12 Red, ColorCount 13 Green, ColorCount 14 Blue]