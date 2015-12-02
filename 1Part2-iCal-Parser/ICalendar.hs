{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

-- Assigment 2 ICalendar
-- Student: Thomas Meijers
-- Student number: 5780314

module ICalendar where

import ParseLib.Abstract
import Data.Maybe
import Text.PrettyPrint
import Data.Char
import System.IO
import Data.List.Extra
import Data.List.Split

data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord)

instance Show DateTime where
  show = printDateTime
    where printDateTime DateTime {..} = printDate date ++ "T" ++ printTime time ++ ['Z' | utc]
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


data Calendar = Calendar { prodId :: String
                         , events :: [VEvent] }
    deriving Eq

data VEvent = VEvent { dtStamp     :: DateTime
                     , uid         :: String
                     , dtStart     :: DateTime
                     , dtEnd       :: DateTime
                     , description :: Maybe String
                     , summary     :: Maybe String
                     , location    :: Maybe String }
    deriving (Eq, Show)

-- Parser combinator for DateTime, gets created by combining parseDate, parseTime and parseUtc
-- Finally DateTime gets contstructed by the parser using the <$> parser combinator
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

run :: Parser a b -> [a] -> Maybe b
run p s = listToMaybe [p | (p, []) <- parse p s]


-- "Main" block, DO NOT EDIT.
-- If you want to run the parser + pretty-printing, rename this module (first line) to "Main".
-- DO NOT forget to rename the module back to "ICalendar" before submitting to DomJudge.
main = do Just cal <- readCalendar "examples/rooster_infotc.ics"
          putStrLn $ show $ ppMonth (Year 2012) (Month 11) $ cal


-- Exercise 1
parseCalendar :: Parser Char Calendar
parseCalendar = Calendar         <$>
                (parseBeginVCal  *>
                parseVersion     *>
                parseProdId)     <*>
                many parseVEvent <*
                parseEndVCal

parseBeginVEvent :: Parser Char String
parseBeginVEvent = parseTokenColonToken "BEGIN" "VEVENT"

parseEndVEvent :: Parser Char String
parseEndVEvent = parseTokenColonToken "END" "VEVENT"

parseUID :: Parser Char String
parseUID = parseTokenColonIdentifier "UID"

parseVEvent :: Parser Char VEvent
parseVEvent = VEvent             <$>
              (parseBeginVEvent   *>
              parseDateTimeStamp) <*> -- apply all fields to permsParser reorder correctly etc..
              parseUID           <*>
              parseDateTimeStart <*>
              parseDateTimeEnd   <*>
              parseDescription   <*>
              parseSummary       <*>
              parseLocation      <* parseEndVEvent

-- |Parser combinator that parses every possible ordering input for parsing
-- elements that are needed in an ordered manner (for the constructor) where the
-- input is unordered
-- type Parser t r = [t] -> [([t], r)]
permsParser :: Parser a b
permsParser = undefined -- permutations xs or something

-- xs = ['d', 'd', 'a', 'c']
-- permutations xs
-- apply parser to every value from the permutations
-- order the result values
-- return the result values

parseDateTimeStamp :: Parser Char DateTime
parseDateTimeStamp = undefined -- parseTokenColonParser "DTSTAMP" parseDateTime

parseDateTimeStart :: Parser Char DateTime
parseDateTimeStart = undefined --parseTokenColonParser "DTSTART" parseDateTime

parseDateTimeEnd :: Parser Char DateTime
parseDateTimeEnd = undefined --parseTokenColonParser "DTEND" parseDateTime

parseDescription :: Parser Char (Maybe String)
parseDescription = optional $ parseTokenColonIdentifier "DESCRIPTION"

parseSummary :: Parser Char (Maybe String)
parseSummary = optional $ parseTokenColonIdentifier "SUMMARY"

parseLocation :: Parser Char (Maybe String)
parseLocation = optional $ parseTokenColonIdentifier "LOCATION"

parseBeginVCal :: Parser Char String
parseBeginVCal = parseTokenColonToken "BEGIN" "VCALENDAR"

parseEndVCal :: Parser Char String
parseEndVCal = parseTokenColonToken "END" "VCALENDAR"

parseTokenColonToken :: String -> String -> Parser Char String
parseTokenColonToken t t' = token t *> parseColon *> token t'

parseColon :: Parser Char Char
parseColon = symbol ':'

-- | Creates a parser combinator based on a token followed by a colon followed by an identifier
-- Both the token and colon can be ignored only the result of identifier matters
parseTokenColonIdentifier :: String -> Parser Char String
parseTokenColonIdentifier t = parseTokenColonParser t $ many anySymbol

parseTokenColonParser :: String -> Parser Char a -> Parser Char a
parseTokenColonParser t = (token t *> parseColon *>)

parseVersion :: Parser Char String
parseVersion = parseTokenColonIdentifier "VERSION"

parseProdId :: Parser Char String
parseProdId = parseTokenColonIdentifier "PRODID"

-- Exercise 2
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar fp = do
                  handle  <- openFile fp ReadMode
                  _       <- hSetNewlineMode handle universalNewlineMode -- set the mode of parsing to parse \r\n instead of only \n result is IO ()
                  content <- hGetContents handle -- get the contents from the file using the modified handle
                  return $
                    run parseCalendar $
                     content -- Parse the input to a Calendar


-- parseSymbol :: String -> Parser Token String
-- parseSymbol s = symbol s *> anySymbol
--
-- parseSymbolSymbol :: String -> String -> Parser Token String
-- parseSymbolSymbol s s' = symbol s *> symbol s'
--
-- -- parseSymbolDateTime :: String -> Parser Token DateTime
-- -- parseSymbolDateTime s = symbol s *> parseDateTime
--
-- parseBeginVCal' :: Parser Token String
-- parseBeginVCal' = parseSymbolSymbol "BEGIN" "VCALENDAR"
--
-- parseVersion' :: Parser Token String
-- parseVersion' = parseSymbol "VERSION"
--
-- parseProdId' :: Parser Token String
-- parseProdId' = parseSymbol "PRODID"
--
-- parseBeginVEvent' :: Parser Token String
-- parseBeginVEvent' = parseSymbolSymbol "BEGIN" "VEVENT"
--
-- parseDateTimeStamp' :: Parser Token String
-- parseDateTimeStamp' = parseSymbol "DTSTAMP"
--
-- parseDateTimeToken :: Parser Token DateTime
-- parseDateTimeToken = undefined
--
-- parseTest :: Parser Token String
-- parseTest = (((parseBeginVCal' *> parseVersion') *> parseProdId') *> parseBeginVEvent') *> parseDateTimeStamp'

parseData = ["BEGIN","VCALENDAR","VERSION","2.0","PRODID","-//hacksw/handcal//NONSGML v1.0//EN","BEGIN","VEVENT","DTSTAMP","19970610T172345Z","UID","19970610T172345Z-AF23B2@example.com","DTSTART","19970714T170000Z","DTEND","19970715T040000Z","SUMMARY","Bastille Day Party","END","VEVENT","END","VCALENDAR",""]

readTest :: FilePath -> IO [Token]
readTest fp = do
              handle  <- openFile fp ReadMode
              _       <- hSetNewlineMode handle universalNewlineMode
              content <- hGetContents handle
              return $ scanner content

data Token = Identifier | StringToken String | DateTimeToken DateTime -- Convert Parser Type to Token ish
  deriving Show

instance Eq Token where
  (StringToken _) == Identifier = True
  (DateTimeToken _) == Identifier = True

scanner :: String -> [Token]
scanner = map makeToken . splitOneOf ":\n\r"

makeToken :: String -> Token
makeToken xs = if (length xs == 15 || length xs == 16) && isDigit (head xs)
                  then DateTimeToken (makeDateTime xs)
               else StringToken xs

makeDateTime :: String -> DateTime
makeDateTime = parseResult . parse parseDateTime

parseResult :: [(a, b)] -> a
parseResult xs
  | null xs = error "DateTime could not be parsed"
  | otherwise = fst (head xs)

parseDateTime' :: Parser Token DateTime
parseDateTime' = symbol DateTimeToken
  where parseDateTime'' (DateTimeToken x : xs) = [(x, xs)]
        parseDateTime'' _                      = []

-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined

-- Exercise 4
countEvents :: Calendar -> Int
countEvents Calendar{events} = length events

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents dt cal = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined

-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> Doc
ppMonth = undefined
