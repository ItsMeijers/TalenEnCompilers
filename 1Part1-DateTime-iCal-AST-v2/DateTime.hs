{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

import ParseLib.Abstract
import Data.List
import Data.Time.Calendar (gregorianMonthLength)

-- Starting Framework


-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord)


-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseUtc

parseUtc :: Parser Char Bool
parseUtc = succeed True <* symbol 'Z' <|> succeed False

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = Year <$> parseNatural 4

parseMonth :: Parser Char Month
parseMonth = Month <$> parseNatural 2

parseDay :: Parser Char Day
parseDay = Day <$> parseNatural 2

parseTime :: Parser Char Time
parseTime = Time
            <$> (symbol 'T' *> parseHour) -- Symbol T has to be parsed but result can be ignored
            <*> parseMinute
            <*> parseSecond

parseHour :: Parser Char Hour
parseHour = Hour <$> parseNatural 2

parseMinute :: Parser Char Minute
parseMinute = Minute <$> parseNatural 2

parseSecond :: Parser Char Second
parseSecond = Second <$> parseNatural 2

parseNatural :: Int -> Parser Char Int
parseNatural n = digitsToNumber <$> (ParseLib.Abstract.sequence (replicate n natural))

digitsToNumber :: [Int] -> Int
digitsToNumber = foldl addDigit 0
  where addDigit acc c = 10 * acc + c

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p = fmap fst . find (null . snd) . parse p

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime DateTime {..} = printDate date ++ "T" ++ printTime time ++ ['Z' | utc]
  where printDate Date {..} = show' (unYear year)   4 ++
                              show' (unMonth month) 2 ++
                              show' (unDay day) 2
        printTime Time {..} = show' (unHour hour)     2 ++
                              show' (unMinute minute) 2 ++
                              show' (unSecond second) 2
        show' x             = fixLenght (show x)
        fixLenght xs n
          | length' >= n    = xs
          | otherwise       = replicate (n - length') '0' ++ xs
              where length' = length xs

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime DateTime {..} = checkDate date && checkTime time
  where checkDate Date{..} = checkMonth && checkDay
          where checkMonth = unMonth month >= 1 && unMonth month <= 12
                checkDay    = unDay day <= gregorianMonthLength (toInteger (unYear year)) (unMonth month) && unDay day >= 1
        checkTime Time{..} = checkHour && checkMinute && checkSecond
          where checkHour = unHour hour >= 0 && unHour hour <= 23
                checkMinute = unMinute minute >= 0 && unMinute minute <= 59
                checkSecond = unSecond second >= 0 && unSecond second <= 59


-- Exercise 6
