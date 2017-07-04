{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Export to Org mode for Emacs.

module ICal.Org
  (-- * Handy export functions
   exportFromToFile
  ,parseFromObject
  -- * Conversions
  ,documentParser
  ,buildDocument
  -- * Types
  ,Event (..)
  )
  where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Identity
import           Data.Ord
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.IO as LT
import           Data.Time
import           ICal
import           ICal.Parser
import           ICal.Types

-- | An Org mode section.
data Event =
  Event {eventTitle :: !Text -- ^ Title of the section.
        ,eventStart :: !UTCTime -- ^ Date starts.
        ,eventDescription :: !(Maybe Text) -- ^ Contents of the section.
        ,eventEnd :: !(Maybe UTCTime) -- ^ Date ends.
        ,eventCreated :: !UTCTime -- ^ Date created.
        }
  deriving (Show)

-- | Handy exporting function.
exportFromToFile :: Day -> FilePath -> FilePath -> IO ()
exportFromToFile base from to =
  do obj <- tokenizeObjectFromFile from
     today <- getCurrentTime
     case parseFromObject obj of
       Left er -> error (show er)
       Right es ->
         LT.writeFile to
                      (LT.toLazyText (buildDocument base today es))

-- | Parse an iCalendar object into an Org mode document.
parseFromObject :: Object -> Either ParseError [Event]
parseFromObject s = runIdentity (parseEither s documentParser)

-- | Build an org-mode document.
buildDocument :: Day -> UTCTime -> [Event] -> Builder
buildDocument base today =
  mconcat .
  map build .
  dropWhile (\e ->
               utctDay (fromMaybe (eventStart e)
                                  (eventEnd e)) <
               base) .
  sortBy (comparing eventStart)
  where build event =
          mconcat ["* " <> todo <> LT.fromText (eventTitle event)
                  ,"\n"
                  ,"  SCHEDULED: <" <> formatDate (eventStart event) <> ">"
                  ,if fromMaybe (eventStart event)
                                (eventEnd event) >
                      today
                      then ""
                      else "\n  - State \"DONE\"       from \"TODO\"       [" <>
                           formatDate
                             (fromMaybe (eventStart event)
                                        (eventEnd event)) <>
                           "]\n"
                  ,"\n"]
          where formatDate =
                  LT.fromText .
                  T.pack . formatTime defaultTimeLocale "%Y-%m-%d"
                todo =
                  if fromMaybe (eventStart event)
                               (eventEnd event) >
                     today
                     then "TODO "
                     else "DONE "

-- | Parse an org-mode document from the object.
documentParser :: Parser Identity Object [Event]
documentParser =
  begin "VCALENDAR"
        (do version <- property "VERSION"
            unless (version == "2.0")
                   (parseError (GeneralProblem "Expected document version 2.0."))
            scale <- property "CALSCALE"
            unless (scale == "GREGORIAN")
                   (parseError (GeneralProblem "Need time gregorian scale."))
            timezones <- fmap M.fromList (objects "VTIMEZONE" timeZoneParser)   --mike
            events <- objects "VEVENT" (eventParser timezones)
            return events)

-- | Parse a time zone.
timeZoneParser :: Parser Identity [Object] (Text,TimeZone)
timeZoneParser =
  do key <- property "TZID"
     return (key,utc)

-- | Parse an event.
eventParser :: Map Text TimeZone -> Parser Identity [Object] Event
eventParser timezones =
  do start <- property "DTSTART" >>= utcTimeParser timezones
     end <- optional (property "DTEND" >>= utcTimeParser timezones)
     created <- property "CREATED" >>= utcTimeParser timezones
     description <- optional (property "DESCRIPTION")
     summary <- property "SUMMARY"
     return (Event {eventTitle = summary
                   ,eventStart = start
                   ,eventEnd = end
                   ,eventDescription = description
                   ,eventCreated = created})

-- | Parse a time field into a UTCTime.
utcTimeParser :: Map Text TimeZone -> Text -> Parser Identity s UTCTime
utcTimeParser timezones s =
  case T.stripPrefix "VALUE=DATE:" s of
    Just s' ->
      case justdate s' of
        Nothing ->
          parseError (GeneralProblem ("Unable to parse date from " <> s'))
        Just t -> return t
    Nothing ->
      case T.stripPrefix "TZID=" s of
        Just tzPlusDate ->
          case T.break (== ':') tzPlusDate of
            (tz,T.drop 1 -> date) ->
              case datetime "" date of
                Just t -> return t
                Nothing ->
                  parseError (GeneralProblem ("Couldn't parse: " <> date))
        Nothing ->
          case datetime "Z" s of
            Just t -> return t
            Nothing ->
              parseError (GeneralProblem ("Invalid date property: " <> s))
  where datetime z s' =
          parseTimeM True
                     defaultTimeLocale
                     ("%Y%m%dT%H%M%S" ++ z)
                     (T.unpack s')
        justdate s' =
          fmap (`UTCTime`  0)
               (parseTimeM True
                           defaultTimeLocale
                           "%Y%m%d"
                           (T.unpack s'))
