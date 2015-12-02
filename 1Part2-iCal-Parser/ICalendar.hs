{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module ICalendar where

import ParseLib.Abstract
import Data.Maybe
import Text.PrettyPrint
import Data.Char
import System.IO
import Data.List.Extra(permutations)

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
-- Gets used enought for extracting this function as a specialization
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
                (parseBeginVCal  *> -- Begin and Version can be ignored for constructing Calendar
                parseVersion     *>
                parseProdId)     <*>
                many parseVEvent <* -- parses many events where the parser is specified for a single event
                parseEndVCal        -- EndVCal can ignored for constructing Calendar
      where
        parseBeginVCal = parseTokenColonToken "BEGIN" "VCALENDAR" <* parseNewLine
        parseVersion   = parseTokenColonIdentifier "VERSION"      <* parseNewLine
        parseProdId    = parseTokenColonIdentifier "PRODID"       <* parseNewLine
        parseEndVCal   = parseTokenColonToken "END" "VCALENDAR"   <* parseNewLine

parseVEvent :: Parser Char VEvent
parseVEvent = VEvent             <$>
              (parseBeginVEvent *>
              parseDateTimeStamp) <*>
              parseUID           <*>
              parseDateTimeStart <*>
              parseDateTimeEnd   <*>
              parseDescription   <*>
              parseSummary       <*>
              (parseLocation <* parseEndVEvent)
-- TESTING

data VEventTest = VEventTest {
  datStamp :: DateTime,
  datStart :: DateTime,
  datEnd :: DateTime,
  descr :: Maybe String,
  summ  :: Maybe String,
  loc   :: Maybe String,
  ui    :: String} deriving Show

data VEventResult = DT (String, DateTime)
                  | MS (Maybe (String, String))
                  | S  (String, String) deriving Show

parseDateStamp :: Parser Char VEventResult
parseDateStamp = DT <$> parseTokenColonParserT "DTSTAMP" parseDateTime <* parseNewLine

parseDateStart :: Parser Char VEventResult
parseDateStart = DT <$> parseTokenColonParserT "DTSTART" parseDateTime <* parseNewLine

parseDateEnd :: Parser Char VEventResult
parseDateEnd = DT <$> parseTokenColonParserT "DTEND" parseDateTime <* parseNewLine

parseDesc :: Parser Char VEventResult
parseDesc = MS <$> optional (parseTokenColonParserT "DESCRIPTION" (many1 anySymbol) <* parseNewLine)

parseSumm :: Parser Char VEventResult
parseSumm = MS <$> optional (parseTokenColonParserT "SUMMARY" (many1 anySymbol) <* parseNewLine)

parseLoc :: Parser Char VEventResult
parseLoc = MS <$> optional (parseTokenColonParserT "LOCATION" (many1 anySymbol) <* parseNewLine)

parseUID' :: Parser Char VEventResult
parseUID' = S <$> parseTokenColonParserT "UID" (many anySymbol) <* parseNewLine

-- combiners
parseVEventResults :: Parser Char [VEventResult]
parseVEventResults = unordered [ parseDateStamp
                               , parseDateStart
                               , parseDateEnd
                               , parseDesc
                               , parseLoc
                               , parseSumm
                               , parseUID' ]

-- parseDates :: Parser Char [(String, DateTime)]
-- parseDates = unordered [parseDateStamp, parseDateStart, parseDateEnd]
--
-- parseMaybeStrings :: Parser Char [Maybe (String, String)]
-- parseMaybeStrings = unordered [parseDesc, parseSumm, parseLoc]


-- abstractions
parseTokenColonParserT :: String -> Parser Char a -> Parser Char (String, a)
parseTokenColonParserT xs = combineParser (token xs <* parseColon)

combineParser :: Parser Char a -> Parser Char b -> Parser Char (a, b)
combineParser p = ((,) <$> p <*>)

toEvent :: [VEventResult] -> VEventTest
toEvent = foldr f VEventTest{descr = Nothing, summ = Nothing, loc = Nothing}
  where f (MS (Just ("DESCRIPTION", v))) ve = ve{descr=Just v}
        f (MS (Just ("SUMMARY", v)))     ve = ve{summ=Just v}
        f (MS (Just ("LOCATION", v)))    ve = ve{loc=Just v}
        f (MS Nothing)                   ve = ve
        f (DT ("DTSTAMP", v))            ve = ve{datStamp=v}
        f (DT ("DTSTART", v))            ve = ve{datStart=v}
        f (DT ("DTEND", v))              ve = ve{datEnd=v}
        f (S ("UID", v))                 ve = ve{ui=v}

parseVEventTest :: Parser Char VEventTest
parseVEventTest = toEvent <$> parseVEventResults

readTestVEvent :: FilePath -> IO (Maybe VEventTest)
readTestVEvent fp = do
                handle <- openFile fp ReadMode
                _ <- hSetNewlineMode handle universalNewlineMode
                content <- hGetContents handle
                return $ run parseVEventTest content

-- TESTING


vEventTest :: FilePath -> IO (Maybe VEvent)
vEventTest fp = do
                handle <- openFile fp ReadMode
                _ <- hSetNewlineMode handle universalNewlineMode
                content <- hGetContents handle
                return $ run parseVEvent content

parseBeginVEvent :: Parser Char String
parseBeginVEvent = parseTokenColonToken "BEGIN" "VEVENT" <* parseNewLine

parseUID :: Parser Char String
parseUID = parseTokenColonIdentifier "UID" <* parseNewLine

parseDateTimeStamp :: Parser Char DateTime
parseDateTimeStamp = parseTokenColonParser "DTSTAMP" parseDateTime <* parseNewLine

parseDateTimeStart :: Parser Char DateTime
parseDateTimeStart = parseTokenColonParser "DTSTART" parseDateTime <* parseNewLine

parseDateTimeEnd :: Parser Char DateTime
parseDateTimeEnd = parseTokenColonParser "DTEND" parseDateTime <* parseNewLine

parseDescription :: Parser Char (Maybe String)
parseDescription = optional $ parseTokenColonIdentifier "DESCRIPTION" <* parseNewLine

parseSummary :: Parser Char (Maybe String)
parseSummary = optional $ parseTokenColonIdentifier "SUMMARY" <* parseNewLine

parseLocation :: Parser Char (Maybe String)
parseLocation = optional $ parseTokenColonIdentifier "LOCATION" <* parseNewLine

parseEndVEvent :: Parser Char String
parseEndVEvent = parseTokenColonToken "END" "VEVENT" <* parseNewLine

optionalStringParsers :: Parser Char (Maybe String)
optionalStringParsers = parseDescription <|> parseSummary <|> parseLocation

dateTimeParsers :: Parser Char DateTime
dateTimeParsers = parseDateTimeStamp <|> parseDateTimeStart <|> parseDateTimeEnd

unordered :: [Parser s a] -> Parser s [a]
unordered = choice . map ParseLib.Abstract.sequence . permutations

-- Parser combinator abstractions :

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

parseNewLine :: Parser Char Char
parseNewLine = symbol '\n'

-- Exercise 2
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar fp = do
                  handle  <- openFile fp ReadMode
                  _       <- hSetNewlineMode handle universalNewlineMode
                  content <- hGetContents handle
                  return $ run parseCalendar content

type Token = String -- Convert Parser Type to Token ish

readTest :: FilePath -> IO String
readTest fp = do
              handle  <- openFile fp ReadMode
              _       <- hSetNewlineMode handle universalNewlineMode
              hGetContents handle

-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined

-- Exercise 4
countEvents :: Calendar -> Int
countEvents = undefined

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined

-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> Doc
ppMonth = undefined
