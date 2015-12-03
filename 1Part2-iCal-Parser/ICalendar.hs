{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module ICalendar where

import ParseLib.Abstract
import Data.Maybe
import Text.PrettyPrint
import Data.Char
import System.IO

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

recognizeCalendar :: [Char] -> Maybe Calendar
recognizeCalendar s = run parseCalendar s

-- Exercise 1
-- |
parseCalendar :: Parser Char Calendar
parseCalendar = Calendar         <$>
                (parseBeginVCal  *> -- Begin and Version can be ignored for constructing Calendar
                parseProdId     <*
                parseVersion)     <*>
                many parseVEvent <* -- parses many events where the parser is specified for a single event
                parseEndVCal        -- EndVCal can ignored for constructing Calendar
      where
        parseBeginVCal = parseTokenColonToken "BEGIN" "VCALENDAR" <* parseNewLine
        parseVersion   = parseTokenColonIdentifier "VERSION"      <* parseNewLine
        parseProdId    = parseTokenColonIdentifier "PRODID"       <* parseNewLine
        parseEndVCal   = parseTokenColonToken "END" "VCALENDAR"   <* parseNewLine

-- |
parseVEvent :: Parser Char VEvent
parseVEvent =  toEvent <$> (parseBeginVEvent   *>
                            parseVEventResults <*
                            parseEndVEvent)
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

-- |
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

-- |
data VEventResult = DT (String, DateTime)
                  | MS (Maybe (String, String))
                  | S  (String, String)

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

-- |
optionalVEvent :: String -> Parser Char VEventResult
optionalVEvent xs = MS <$> optional (parseTokenColonParserT xs parseSymbols <* parseNewLine)

parseSymbols :: Parser Char [Char]
parseSymbols = greedy (satisfy (/= '\r'))

{- Functions defined below are abstractions for creating or modifying parser
   combinators see individual comments for explanation of each function     -}

-- |
parseTokenColonParserT :: String -> Parser Char a -> Parser Char (String, a)
parseTokenColonParserT xs = combineParser (token xs <* parseColon)

-- | Combines two parser where the result of both parsers are represented in a Tuple
combineParser :: Parser Char a -> Parser Char b -> Parser Char (a, b)
combineParser p = ((,) <$> p <*>)

-- |
unordered :: [Parser s a] -> Parser s [a]
unordered = choice . map ParseLib.Abstract.sequence . permutations

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap (between x) (permutations xs)
  where between e []     = [[e]]
        between e (y:ys) = (e:y:ys) : map (y:) (between e ys)

-- |
parseTokenColonToken :: String -> String -> Parser Char String
parseTokenColonToken t t' = token t *> parseColon *> token t'

-- |
parseColon :: Parser Char Char
parseColon = symbol ':'

-- | Creates a parser combinator based on a token followed by a colon followed by an identifier
-- Both the token and colon can be ignored only the result of identifier matters
parseTokenColonIdentifier :: String -> Parser Char String
parseTokenColonIdentifier t = parseTokenColonParser t $ many (satisfy (/='\n'))

-- |
parseTokenColonParser :: String -> Parser Char a -> Parser Char a
parseTokenColonParser t = (token t *> parseColon *>)

-- |
parseNewLine :: Parser Char String
parseNewLine = token "\r\n"

-- Exercise 2
-- |
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar fp = do
                  handle  <- openFile fp ReadMode
                  content <- hGetContents handle
                  return $ run parseCalendar content

-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined

-- Exercise 4
countEvents :: Calendar -> Int
countEvents Calendar{events}= length events

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined

-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> Doc
ppMonth = undefined
