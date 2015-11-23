{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

-- Assigment 1 DateTime
-- Student: Thomas Meijers
-- Student number: 5780314

import ParseLib.Abstract
import Data.List
import Data.Time.Calendar (gregorianMonthLength)
import Data.Char

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
-- Parser combinator for DateTime, gets created by combining parseDate, parseTime and parseUtc
-- Finally DateTime gets contstructed by using the <$> parser combinator
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseUtc

-- Parser combinator for parsing the UTC value, results in Boolean wether the Z is present or absent
parseUtc :: Parser Char Bool
parseUtc = succeed True
           <* symbol 'Z' -- Symbol Z can be ignored and the True bool will be used
           <|> succeed False -- Or when Z is absent the False bool will be used

-- Function to create a Parser combinbator for Date
-- Extracted as seperate function since parsing an individual date might be usefull
parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay
  where
     parseYear  = Year  <$> parseDigits 4
     parseMonth = Month <$> parseTwoDigits
     parseDay   = Day   <$> parseTwoDigits

-- Creates Parser combinator for Time
-- Extracted as seperate function since parsing an individual Time might be usefull
parseTime :: Parser Char Time
parseTime = Time
            <$> (symbol 'T' *> parseHour) -- Symbol T has to be parsed but the result can be ignored
            <*> parseMinute
            <*> parseSecond
    where
       parseHour   = Hour   <$> parseTwoDigits
       parseMinute = Minute <$> parseTwoDigits
       parseSecond = Second <$> parseTwoDigits

-- Helper function for creating a Int parser where 2 digits are present
-- Gets used enought for abstracting as individual function
parseTwoDigits :: Parser Char Int
parseTwoDigits = parseDigits 2

-- Function for creating a Parser that parses n amount of digits
-- By using sequence a Parser combinator gets created from n digit parser combinators
-- [Char] gets conferted to Int using the digitsToNumber function
parseDigits :: Int -> Parser Char Int
parseDigits n = digitsToNumber <$> (ParseLib.Abstract.sequence (replicate n digit))
  where
     digitsToNumber = foldl addDigit 0
     addDigit acc c = 10 * acc + digitToInt c

-- Exercise 2
-- Applies the parser combinator to a list of input
-- Finds the first list that is a complete parse (remaining list of symbols is empty)
-- If there is not a complete parse Nothing will be returned
-- Since Maybe is a Functor, Maybe can be mapped over, only retrieving the result value from the tuple
run :: Parser a b -> [a] -> Maybe b
run p = fmap fst . find (null . snd) . parse p

-- Exercise 3
-- Prints a DateTime to its original form
-- First prints the Date then add the Symbol T for the time followed by the acutal Time
-- A Z symbol gets added wether utc of DateTIme is True using list comprehension
printDateTime :: DateTime -> String
printDateTime DateTime {..} = printDate date ++ "T" ++ printTime time ++ ['Z' | utc]
  where printDate Date {..} = show' (unYear year)   4 ++
                              show' (unMonth month) 2 ++
                              show' (unDay day) 2
        printTime Time {..} = show' (unHour hour)     2 ++
                              show' (unMinute minute) 2 ++
                              show' (unSecond second) 2
        show' x             = fixLenght (show x)
        fixLenght xs n -- fixes the length of a value to its original form; year 1 gets fixed to 0001
          | length' >= n    = xs
          | otherwise       = replicate (n - length') '0' ++ xs
              where length' = length xs

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
-- Verifies wether a DateTime is valid
-- Parser takes care of checking wether a Year is 4 digits, so can be omitted here
-- Checks date followed by time if both are correct True is returned
-- Used the gregorianMonthLength function to get the amount of days in a specific month
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
-- Representaion of a Calendar which must have a prodId and Version and can have 0 or more Events
data Calendar = Calendar { prodId  :: ProdId
                         , version :: Version
                         , events  :: [Event] }

-- Created newtypes for individual parser combinators and typeclass instances
newtype ProdId  = ProdId  {unProdId  :: String}
newtype Version = Version {unversion :: Double}

-- Representation of a Event in Icalendar Maybe values are optional (0 or 1 occurences)
data Event = Event { dtstamp     :: DateTime
                   , uid         :: UID
                   , dtStart     :: DateTime
                   , dtEnd       :: DateTime
                   , description :: Description
                   , summary     :: Summary
                   , location    :: Location }

-- Created newtypes for individual parser combinators and typeclass instances
newtype UID = UID { unUID :: String }

-- Maybe type inside of the newtype so that the optional parser combinator can create these types
newtype Description = Description { unDescription :: Maybe String }
newtype Summary     = Summary     { unSummary     :: Maybe String }
newtype Location    = Location    { unLocation    :: Maybe String }
