-- |module for parsing RFC2822 time into CalendarTime
-- TODO: Update to use new date\/time libraries in GHC 6.6.
module Text.MIME.Parse.DateTime (readDateTime, dateTime) where

import Text.ParserCombinators.Parsec
import System.Time

import Text.MIME.Parse.MIME

-- |Convert an RFC2822 data\/time string into a CalendarTime
-- NOTE: returns bogus values for tzName, day of year, and isDST
-- components of CalendarTime.
readDateTime :: String -> CalendarTime
readDateTime str =
    case parse dateTime str str of
      Left e -> error (show e)
      Right c -> c

-- |parser for RFC2822 date and time
-- NOTE: returns bogus values for tzName, day of year, and isDST
-- components of CalendarTime.
dateTime :: CharParser st CalendarTime
dateTime =
    do dow <- dayOfWeek
       char ','
       ofws
       d <- day
       ofws
       mon <- month
       ofws 
       y <- year
       ofws
       h <- hour
       char ':'
       mi <- minute
       char ':'
       s <- second
       ofws
       z <- zone
       return (CalendarTime { ctYear = y
                            , ctMonth = mon
                            , ctDay = d
                            , ctHour = h
                            , ctMin = mi
                            , ctSec = s
                            , ctPicosec = 0
                            , ctWDay = dow
                            , ctYDay = 0 -- wrong
                            , ctTZName = "PDT" -- wrong
                            , ctTZ = z
                            , ctIsDST = True -- wrong
                            }
              )

dayOfWeek :: CharParser st Day
dayOfWeek =
    choice [ mon, tue, wed, thu, fri, sat, sun ]
    where
      mon = string "Mon" >> return Monday
      tue = try (string "Tue" >> return Tuesday)
      wed = string "Wed" >> return Wednesday
      thu = try (string "Thu" >> return Thursday)
      fri = string "Fri" >> return Friday
      sat = try (string "Sat" >> return Saturday)
      sun = try (string "Sun" >> return Sunday)

day :: CharParser st Int
day = many1 digit >>= return . read

month :: CharParser st Month
month =
    choice [ jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec ]
    where
      jan = try (string "Jan" >> return January)
      feb = string "Feb" >> return February
      mar = try (string "Mar" >> return March)
      apr = string "Apr" >> return April
      may = try (string "May" >> return May)
      jun = try (string "Jun" >> return June)
      jul = try (string "Jul" >> return July)
      aug = string "Aug" >> return August
      sep = string "Sep" >> return September
      oct = string "Oct" >> return October
      nov = string "Nov" >> return November
      dec = string "Dec" >> return December

year :: CharParser st Int
year = count 4 digit >>= return . read
       
hour :: CharParser st Int
hour = count 2 digit >>= return . read

minute :: CharParser st Int
minute = count 2 digit >>= return . read

second :: CharParser st Int
second = count 2 digit >>= return . read

zone :: CharParser st Int
zone = 
    do pm <- plus <|> minus
       h <- count 2 digit >>= return . read
       m <- count 2 digit >>= return . read
       return (pm (((h * 60) + m) * 60))
    where plus  = char '+' >> return id
          minus = char '-' >> return negate