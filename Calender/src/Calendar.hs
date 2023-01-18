module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime

-- the line below was cut from Main.hs file and pasted here 
-- (I was getting errors and could not derive the Show class for my Calendar and Event datatype for some reason)
instance Show DateTime where
    show = printDateTime

-- Exercise 6
-- all the example tests are of version 2.0 
-- so although I have included the field with the "Calender" constructor 
-- we can optionally not include it in our datatype.
-- apart from the terminals, each calender consists of three main non-terminal symbols:
-- 1- prodid. 2-version. 3-event (list of event). So we bascially need three constructors. 
-- or one constructor with three fields. 
-- (in the lecture-notes normally three constructors were definedin such a situation;
-- however, the predefined DataTime in the homework had only one constructor so I am going to do the same). Doesn't matter in the main application.
data Calendar = Calendar { prodid  :: Prodid,
                           version :: Version,
                           events  :: [Event]}
    deriving (Eq, Ord, Show)

type Prodid  = String 
type Version = Float 

-- the optional properties of the event data type have been noted using as Maybe type constructor.
-- if we look at the the Event concrete syntax it has the following non-terminal symbols:
-- 1-dtstamp 2-uid 3-dtstart 4-dtend 5-description 6-summary 7-location
data Event = Event { dstamp      :: DStamp,
                     uid         :: Uid,
                     dtstart     :: DtStart,
                     dtend       :: DTend,
                     description :: Maybe Description,
                     summary     :: Maybe Summary,
                     location    :: Maybe Location}
    deriving (Eq, Ord, Show)

-- dstamp in the concrete syntax has one non-terminal symbol: DateTime. We could have defined:
-- data DStamp = DStamp DateTime deriving (Eq, Ord, Show). However, doing so for every single 
-- data type below would just make my semantic functions more nested.
type DStamp      = DateTime
type Uid         = String
type DtStart     = DateTime
type DTend       = DateTime
type Description = String
type Summary     = String
type Location    = String

-- Exercise 7
-- each token is one of the below data types.
data Token = BEGINCALENDER 
           | PRODID {getProdid :: String} | VERSION {getVersion :: Float} 
           | ENDCALENDER
           | BEGINEVENT    
           | DSTAMP {getDStamp:: DateTime} | UID {getUID :: String} | DTSTART {getStart :: DateTime} | DTEND {getEnd :: DateTime} | DESCRIPTION {getDescription :: String} | SUMMARY {getSummary :: String} | LOCATION {getLocation :: String}
           | ENDEVENT
    deriving (Eq, Ord, Show)

-- a lexer (scanner) is a kind of parser. 
-- The first complete parsing is important for us. so we use greedy. 
-- by doing so, we can parse over the construct multiple times and get the complete one.
scanCalendar :: Parser Char [Token]
scanCalendar =  greedy p

-- this is one parse to get one token.
-- depending on which token it is (look at the "token" data type), we need the option parser combinator (<|>).
-- for the beginning of a calender, we just need to check whether a string is present, if so we throw away the string (because it is always constant)
-- we do the same for the end of a calender, the beginning and the end of event as well.
p :: Parser Char Token
p =   BEGINCALENDER <$ token "BEGIN:VCALENDAR"
  <|> PRODID        <$ token "PRODID:"  <*> parseTillEnd
  <|> VERSION       <$ token "VERSION:" <*> parseforVersion
  <|> ENDCALENDER   <$ token "END:VCALENDAR"
  <|> BEGINEVENT    <$ token "BEGIN:VEVENT"
  <|> DSTAMP        <$ token "DTSTAMP:"     <*> parseDateTime
  <|> UID           <$ token "UID:"         <*> parseTillEnd
  <|> DTSTART       <$ token "DTSTART:"     <*> parseDateTime
  <|> DTEND         <$ token "DTEND:"       <*> parseDateTime
  <|> SUMMARY       <$ token "SUMMARY:"     <*> parseTillEnd
  <|> DESCRIPTION   <$ token "DESCRIPTION:" <*> parseTillEnd
  <|> LOCATION      <$ token "LOCATION:"    <*> parseTillEnd
  <|> ENDEVENT      <$ token "END:VEVENT"

-- it parses the rest of that line until it sees the return carriage symbol
parseTillEnd :: Parser Char String -- String -> [(Char, String)]
parseTillEnd = greedy (satisfy ( \x -> x /= '\r' ))

-- parse the number in front of VERSION: and returns as a Float.
parseforVersion :: Parser Char Float 
parseforVersion = (\x y z -> read [x,y,z]) <$> digit <*> symbol '.' <*> digit

parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$> parseProdid <*> parseVersion <*> parseEvents

-- parse prodid
parseProdid :: Parser Token Prodid
parseProdid = getProdid <$> satisfy isProdid

isProdid :: Token -> Bool
isProdid (PRODID _) = True
isProdid _ = False 

-- parse version
parseVersion :: Parser Token Version
parseVersion = getVersion <$> satisfy isVersion

isVersion :: Token -> Bool
isVersion (VERSION _) = True
isVersion _ = False

-- parse event
parseEve :: Parser Token Event
parseEve = Event <$> parsedStamp <*> parseUID <*> parsedtstart <*> parsedtend <*> (Just <$> parsedescription) <*> (Just <$> parsesummary) <*> (Just <$> parselocation)

parseEvents :: Parser Token [Event]
parseEvents = many parseEve

-- dStamp
parsedStamp :: Parser Token DStamp
parsedStamp = getDStamp <$> satisfy isdstamp

isdstamp :: Token -> Bool
isdstamp (DSTAMP _) = True
isdstamp _ = False

-- UID
parseUID:: Parser Token Uid
parseUID = getUID <$> satisfy isUID

isUID :: Token -> Bool
isUID (UID _) = True
isUID _ = False

-- dtstart
parsedtstart:: Parser Token DtStart
parsedtstart = getStart <$> satisfy isdtstart

isdtstart :: Token -> Bool
isdtstart (DTSTART _) = True
isdtstart _ = False

-- dtend
parsedtend:: Parser Token DTend
parsedtend = getEnd <$> satisfy isdtend

isdtend :: Token -> Bool
isdtend (DTEND _) = True
isdtend _ = False

-- description
parsedescription:: Parser Token Description
parsedescription= getDescription <$> satisfy isdescription

isdescription :: Token -> Bool
isdescription (DESCRIPTION _) = True
isdescription _ = False

-- summary
parsesummary:: Parser Token Summary
parsesummary= getSummary <$> satisfy issummary

issummary :: Token -> Bool
issummary (SUMMARY _) = True
issummary _ = False

-- location
parselocation:: Parser Token Location
parselocation = getLocation <$> satisfy islocation

islocation :: Token -> Bool
islocation (LOCATION _) = True
islocation _ = False

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar c = "BEGIN:VCALENDAR" ++ "\r\n"            ++
                  "PRODID:"         ++ prodid c          ++ "\r\n" ++
                  "VERSION:"        ++ show (version c)  ++ "\r\n" ++
                   printEvents e    ++
                  "END:VCALENDAR"   ++ "\r\n"
                    where e = events c

-- printing the events
printEvents :: [Event] -> String
printEvents xs = concatMap printOneEvent xs
  where printOneEvent x = "BEGIN:VEVENT"     ++ "\r\n"     ++
                          printSummary     (summary x)     ++ 
                          printDescription (description x) ++ 
                          printLocation    (location x)    ++
                          "UID:"         ++ uid     x                 ++ "\r\n" ++
                          "DTSTAMP:"     ++ printDateTime (dstamp x)  ++ "\r\n" ++
                          "DTSTART:"     ++ printDateTime (dtstart x) ++ "\r\n" ++
                          "DTEND:"       ++ printDateTime (dtend x)   ++ "\r\n" ++
                          "END:VEVENT"   ++ "\r\n" 

-- if the summary is not given for the event we should not print anything or in other words an empty string.
-- same goes for description and location.
printSummary :: Maybe Summary -> String
printSummary Nothing  = ""
printSummary (Just s) = "SUMMARY:" ++ s ++ "\r\n"

printDescription :: Maybe Description -> String
printDescription Nothing  = ""
printDescription (Just s) = "DESCRIPTION:" ++ s ++ "\r\n" 

printLocation :: Maybe Location -> String
printLocation Nothing  = ""
printLocation (Just s) = "LOCATION:" ++ s ++ "\r\n" 
