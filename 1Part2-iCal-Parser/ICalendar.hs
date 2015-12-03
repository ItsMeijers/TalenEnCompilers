{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

-- Assigment 2 ICal
-- Student: Thomas Meijers
-- Student number: 5780314

module ICalendar where

import ParseLib.Abstract
import Data.Maybe
import Text.PrettyPrint
import Data.Char
import System.IO
import Data.List(delete)
import Data.Time.Calendar (gregorianMonthLength)
import Data.Map.Strict(Map, insertWith, fromList, toList, elems)

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
    deriving Eq

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

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run parseCalendar s

-- Exercise 1
-- | Parser combinator for pasing a Calendar
parseCalendar :: Parser Char Calendar
parseCalendar = Calendar         <$>
                (parseBeginVCal  *> -- Begin and Version can be ignored for constructing Calendar
                parseProdId      <*
                parseVersion)    <*>
                many parseVEvent <* -- parses many events where the parser is specified for a single event
                parseEndVCal        -- EndVCal can ignored for constructing Calendar
      where
        parseBeginVCal = parseTokenColonToken "BEGIN" "VCALENDAR" <* parseNewLine
        parseVersion   = parseTokenColonIdentifier "VERSION"      <* parseNewLine
        parseProdId    = parseTokenColonIdentifier "PRODID"       <* parseNewLine
        parseEndVCal   = parseTokenColonToken "END" "VCALENDAR"   <* parseNewLine

-- | Parser combinator for parsing an event
-- toEvent maps over the parser combinator to reorder the values for the constructor
parseVEvent :: Parser Char VEvent
parseVEvent =  toEvent <$> (parseBeginVEvent   *> -- Begin can be ignored
                            parseVEventResults <*
                            parseEndVEvent)       -- End can be ignored
    where
      parseBeginVEvent = parseTokenColonToken "BEGIN" "VEVENT" <* parseNewLine
      parseEndVEvent   = parseTokenColonToken "END" "VEVENT" <* parseNewLine

-- | Creates an "empty" evevent for folding the unordered values into VEvent from a list of maps
emptyVEvent :: VEvent
emptyVEvent = VEvent{ description = Nothing
                    , summary     = Nothing
                    , location    = Nothing
                    , dtStamp     = undefined
                    , uid         = undefined
                    , dtStart     = undefined
                    , dtEnd       = undefined }

-- | Creates an VEvent out of a List of VEventResults by folding over the [(Name, Value)]
-- based on the Name the value gets put into the VEvent constructor
toEvent :: [VEventResult] -> VEvent
toEvent = foldr f emptyVEvent
  where f (DT       ("DTSTAMP"    , v))  ve = ve{dtStamp=v}
        f (DT       ("DTSTART"    , v))  ve = ve{dtStart=v}
        f (DT       ("DTEND"      , v))  ve = ve{dtEnd=v}
        f (S        ("UID"        , v))  ve = ve{uid=v}
        f (MS (Just ("DESCRIPTION", v))) ve = ve{description=Just v}
        f (MS (Just ("SUMMARY"    , v))) ve = ve{summary=Just v}
        f (MS (Just ("LOCATION"   , v))) ve = ve{location=Just v}
        f (MS Nothing)                   ve = ve

-- | Values to hold the different parse results of the unordered result of the
-- parseVEventResults parserCombinator
data VEventResult = DT (String, DateTime)
                  | MS (Maybe (String, String)) -- Represents the parse results of the optional values in VEvent
                  | S  (String, String)

-- | Parser combinator that parses all the unordered values of VEvent
-- Every parser combinator get mapped to a VEventResult for re-ordering
-- the unordered paser combinator creates every possible parser for the unordered values
parseVEventResults :: Parser Char [VEventResult]
parseVEventResults = unordered [ parseDateTimeStamp
                               , parseDateTimeStart
                               , parseDateTimeEnd
                               , parseDescription
                               , parseLocation
                               , parseSummary
                               , parseUID ]
    where
      parseDateTimeStamp = DT <$> parseTokenColonParserT "DTSTAMP" parseDateTime <* parseNewLine
      parseDateTimeStart = DT <$> parseTokenColonParserT "DTSTART" parseDateTime <* parseNewLine
      parseDateTimeEnd   = DT <$> parseTokenColonParserT "DTEND"   parseDateTime <* parseNewLine
      parseUID           = S  <$> parseTokenColonParserT "UID"     parseSymbols  <* parseNewLine
      parseDescription   = optionalVEvent "DESCRIPTION"
      parseSummary       = optionalVEvent "SUMMARY"
      parseLocation      = optionalVEvent "LOCATION"

-- | Abstraction to create a parser combinator that parses optional VEvent values of String
optionalVEvent :: String -> Parser Char VEventResult
optionalVEvent xs = MS <$> optional (parseTokenColonParserT xs parseSymbols <* parseNewLine)

-- | Parser combinator that parses everything greedy until it reaches a '\r' symbol (indacating it will reach a newline)
parseSymbols :: Parser Char String
parseSymbols = greedy (satisfy (/= '\r'))

{- Functions defined below are abstractions for creating or modifying parser
   combinators see individual comments for explanation of each function     -}

-- | Abstraction for creating parser combinators that parse two values seperated by a colon
-- both values that are seperated by the colon get saved in a Tuple see combineParser
parseTokenColonParserT :: String -> Parser Char a -> Parser Char (String, a)
parseTokenColonParserT xs = combineParser (token xs <* parseColon)

-- | Combines two parser where the result of both parsers are represented in a Tuple
combineParser :: Parser Char a -> Parser Char b -> Parser Char (a, b)
combineParser p = ((,) <$> p <*>)

-- | By creating all the permutations of a List of parser combinators and sequencing
-- these into a new Parser Combinators where a choice is made for success of one of the
-- parser combinators, unordered values can be parsed
unordered :: [Parser s a] -> Parser s [a]
unordered = choice . map ParseLib.Abstract.sequence . permutations

-- | Creates all the permutations of a List
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap (between x) (permutations xs)
  where between e []     = [[e]]
        between e (y:ys) = (e:y:ys) : map (y:) (between e ys)

-- | Parser combinator for parsing two tokens divided by a colon only the value
-- after the colon gets saved by the parser combinator
parseTokenColonToken :: String -> String -> Parser Char String
parseTokenColonToken t t' = token t *> parseColon *> token t'

-- | Parser combinator for parsing a colon
parseColon :: Parser Char Char
parseColon = symbol ':'

-- | Creates a parser combinator based on a token followed by a colon followed by an identifier
-- Both the token and colon can be ignored only the result of identifier matters
parseTokenColonIdentifier :: String -> Parser Char String
parseTokenColonIdentifier t = parseTokenColonParser t $ many (satisfy (/='\n'))

-- | Abstraction to create a parser combinator that parses a token followed by a
-- colon followed by the parser combinator as snd argument
parseTokenColonParser :: String -> Parser Char a -> Parser Char a
parseTokenColonParser t = (token t *> parseColon *>)

-- | Parser combinator for parsing a new line
parseNewLine :: Parser Char String
parseNewLine = token "\r\n"

-- Exercise 2
-- | Function that based on a FilePath reads the file and parses it to a Calendar
-- Result type is Maybe because parsing can succeed or fail and of course IO since
-- reading a file is an IO action
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar fp = do
                  handle  <- openFile fp ReadMode  -- creates a handle for reading the file based on fp
                  content <- hGetContents handle   -- reads the content as a String
                  return $ run parseCalendar content -- parse the content to a Calendar

-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
-- Function that prints a Calendar to it's original form
printCalendar :: Calendar -> String
printCalendar Calendar {..} = "BEGIN:VCALENDAR"            ++ rn ++
                              "PRODID:" ++ prodId          ++ rn ++
                              "VERSION:2.0"                ++ rn ++
                              concatMap printVEvent events ++
                              "END:VCALENDAR"              ++ rn

-- Function that prints a VEvent to it's original form
printVEvent :: VEvent -> String
printVEvent VEvent {..} = "BEGIN:VEVENT"         ++ rn          ++
                          printMaybe "SUMMARY:"     summary     ++
                          printMaybe "DESCRIPTION:" description ++
                          printMaybe "LOCATION:"    location    ++
                          "DTSTAMP:" ++ printDateTime dtStamp   ++ rn ++
                          "DTSTART:" ++ printDateTime dtStart   ++ rn ++
                          "DTEND:"   ++ printDateTime dtEnd     ++ rn

-- | Helper function to print a maybe value with a preprended string returns
-- a empty string when Maybe is Nothing
printMaybe :: String -> Maybe String -> String
printMaybe xs = maybe "" (\s -> xs ++ s ++ rn)

-- | Small specialization to not having to type \r\n all the time, lazy programming with lazy evaluation ;)
rn :: String
rn = "\r\n"

-- Prints a DateTime to its original form
-- First prints the Date then add the Symbol T for the time followed by the acutal Time
-- A Z symbol gets added wether utc of DateTIme is True using list comprehension
printDateTime :: DateTime -> String
printDateTime DateTime {..} = printDate date ++ "T" ++ printTime time ++ ['Z' | utc]
 where printDate Date {..} = show' (unYear year)   4 ++
                             show' (unMonth month) 2 ++
                             show' (unDay day) 2

-- Exercise 4
-- | Counts the amount (Int) of events of certain Calendar
countEvents :: Calendar -> Int
countEvents Calendar {events} = length events

-- | Finds events that happen during a certain Date and Time
findEvents :: DateTime -> Calendar -> [VEvent]
findEvents dt Calendar {events} = eventsMatchesDateTime dt events

-- | Result is all the events that overlap from two lists of VEvent
eventsMatchesDateTime :: DateTime -> [VEvent] -> [VEvent]
eventsMatchesDateTime dt = filter matchesDateTime
    where matchesDateTime VEvent {dtStart, dtEnd} = dt >= dtStart && dt <= dtEnd

-- | Function that checks wether a Calendar has overlapping VEvents
checkOverlapping :: Calendar -> Bool
checkOverlapping Calendar {events} = any overlaps events
  where overlaps (e@VEvent {dtStart, dtEnd}) = matches dtStart e || matches dtEnd e
        matches dt e' = not (null (eventsMatchesDateTime dt events'))
          where events' = delete e' events -- can be safely done since it's sure e' is part of events

-- | Counts how much time is spent based on a certain summary
timeSpent :: String -> Calendar -> Int
timeSpent s Calendar {events} = sum $ map getTime' events
  where getTime' VEvent {summary, dtStart, dtEnd} = maybe 0 (countTime dtStart dtEnd s) summary

-- | Counts the time for each individual VEvent only >0 if summary equals
countTime :: DateTime -> DateTime -> String -> String -> Int
countTime start end s summ
  | s /= summ = 0
  | otherwise = getTime' end - getTime' start
    where getTime' DateTime {time} = getMinutes time
          getMinutes Time {..}    = unHour hour * 60 - unMinute minute


-- Exercise 5
-- | Sadly didn't have time enough to completely finish this function
-- Didn't have time either to clean up the functions (way too messy atm)
ppMonth :: Year -> Month -> Calendar -> Doc
ppMonth year month Calendar {events} = buildTable (toList dayEvents) columnWidth rows
  where numberOfdays = gregorianMonthLength (toInteger (unYear year)) (unMonth month)
        dayEvents    = foldr addEvent emptyMap events
        emptyMap     = fromList $ zip [1..numberOfdays] $ repeat []
        addEvent VEvent{..} = insertWith (++) (getDay dtStart) [getTime dtStart dtEnd]
        columnWidth  = 15 -- max size of two datetimes seperated by a -
        rows         = maximum $ map length $ elems dayEvents


buildTable :: [(Int, [String])] -> Int -> Int -> Doc
buildTable dayEvents width height = hcat $ map fsep $ splitEvery 7 buildDays
  where buildDays    = map (\(d, xs) -> dayLine d $$ line $$ vcat (map timeLine xs)) dayEvents
        line         = hcat (replicate width (char '-'))
        timeLine s   = ptext s <> space
        dayLine day  = int day <+> hcat (replicate (width - length (show day) - 1) space)
        stripe       = char '|'
        plus         = char '+'

splitEvery :: Int -> [t] -> [[t]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list

getDay :: DateTime -> Int
getDay DateTime{date} = getDay' date
  where getDay' Date{day} = unDay day

getTime :: DateTime -> DateTime -> String
getTime dt dt' = printTime' dt ++ " - " ++ printTime' dt'
  where printTime' DateTime {time} = printTime time

printTime :: Time -> String
printTime Time {..} = show' (unHour hour)     2 ++
                      show' (unMinute minute) 2 ++
                      show' (unSecond second) 2

show' :: Show a => a -> Int -> String
show' x             = fixLenght (show x)

fixLenght :: String -> Int -> String
fixLenght xs n -- fixes the length of a value to its original form; year 1 gets fixed to 0001
  | length' >= n    = xs
  | otherwise       = replicate (n - length') '0' ++ xs
      where length' = length xs
