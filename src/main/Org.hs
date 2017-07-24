-- | Export .org file from an iCalendar file.

import Data.List
import Data.Maybe
import Data.Time
import ICal.Org  
import System.Environment

-- | Export .org file from an iCalendar file.
main :: IO ()
main =
  do args <- getArgs
     let files = filter (not . isPrefixOf "--") args
         opts = filter (isPrefixOf "--") args
     case files of
       [ics,org] ->
         do let base =
                  fromMaybe (fromGregorian 1970 01 01)
                            (do dateString <-
                                  listToMaybe (mapMaybe (stripPrefix "--base=") opts)
                                parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString)
            exportFromToFile base ics org
       _ -> error "expected <input.ics> <output.org> [--base=YYYY-MM-DD (default: 1970-01-01)]"
