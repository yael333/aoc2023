import Data.Char (isDigit)
import Data.List
  ( find,
    inits,
    isPrefixOf,
    isSuffixOf,
    sortBy,
    tails,
  )

type Representation = [(String, Int)]

findValue :: Representation -> (String -> String -> Bool) -> [String] -> Int
findValue repr isRepresented = value
  where
    value [] = error "No value found"
    value (s : ss) = case find (\(a, _) -> a `isRepresented` s) repr of
      Nothing -> value ss
      Just (_, b) -> b

findCalibrationValue :: Representation -> String -> Int
findCalibrationValue r s = 10 * first + last
  where
    first = findValue r isPrefixOf (tails s)
    last = findValue r isSuffixOf (reverse $ inits s)

firstChallengeRepresentation :: Representation
firstChallengeRepresentation = zip (map show [0 .. 9]) [0 .. 9]

secondChallengeRepresentation :: Representation
secondChallengeRepresentation =
  firstChallengeRepresentation
    <> zip ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [0 .. 9]

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print $ sum $ map (findCalibrationValue firstChallengeRepresentation) input
  print $ sum $ map (findCalibrationValue secondChallengeRepresentation) input
