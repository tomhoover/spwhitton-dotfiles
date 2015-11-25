import           Control.Monad      (when)
import qualified Data.Map           as Map
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)
import           System.Exit        (die)
import           System.FilePath    ((</>), isAbsolute)

data Time = Time Int Int        -- hour, minute
data Day = Day Int Int          -- day, month

type SunTimes = Map.Map Day (Time, Time) -- date -> (sunrise time, sunset time)

sepLine :: String
sepLine = "     h m  h m   h m  h m   h m  h m   h m  h m   h m  h m   h m  h m   h m  h m   h m  h m   h m  h m   h m  h m   h m  h m   h m  h m"

parseTable :: String -> SunTimes
parseTable = parseTable' . map (tail . words) . drop 1 . dropWhile (/= sepLine) . lines

parseTable' :: [[String]] -> SunTimes
parseTable' (rise:set:days) = undefined
  where
    rise' = readTime rise
    set'  = readTime set

readTime :: String -> Time
readTime s = Time (read . take 2 $ s) (read . drop 2 $ s)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ die "supply exactly one cmdline argument" -- a text file from http://aa.usno.navy.mil/data/docs/RS_OneYear.php
    here <- getCurrentDirectory
    table <- if isAbsolute (head args)
             then readFile (head args)
             else readFile (here </> head args)
    let times = parseTable table
    undefined
