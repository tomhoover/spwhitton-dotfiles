#!/usr/bin/env runhaskell

-- WIP reimplementation of srem in Haskell.  srem is a bit of a hack
-- and the use of datetime is probably over-engineering.  So see how
-- it comes out in Haskell.

import System.Environment (getArgs, getProgName)
import System.IO
import System.Process (readProcess)
import System.Directory (getHomeDirectory)
import Data.List (foldl')
import Text.Regex.Posix

-- hour, minute, text of the reminder
data Reminder = Reminder Int Int String
              deriving (Show)

-- main function definitions

main :: IO ()
main = do
  args <- getArgs
  runProcs $ getProcs args

getProcs :: [String] -> [IO ()]
getProcs args
  | argc == 1 && head args == "cron" = [undefined]
  | otherwise = usageDie "invalid arguments"
  where argc = length args

parseEmacsOutput :: String -> [Reminder]
parseEmacsOutput = foldl' parseLine [] . drop 2 . lines
  where apptRegexp = " ([0-9]{1,2}):([0-9][0-9])[-]{0,1}[0-9:]{0,5}[.]* (.*)$"
        snipRegexp = "[ ]{2,}:.*:*$"

        parseLine rems line
          | lineMatch = rems ++ staggeredReminders hour' min' text'
          | otherwise = rems
          where lineMatch = line =~ apptRegexp :: Bool
                lineMatchStrings = line =~ apptRegexp :: [[String]]
                hour:min:text:[] = drop 1 . concat $ lineMatchStrings

                hour' = read hour
                min' = read min
                (text',_,_) = text =~ snipRegexp :: (String,String,String)

staggeredReminders :: Int -> Int -> String -> [Reminder]
staggeredReminders hour min text = foldr step [] [0, 15, 60]
  where step diff rems = undefined

sampleEmacs :: String
sampleEmacs = "Sean's diary for today\nWednesday   4 February 2015\n  Appt:       17:00...... Dummy event 1                                  :APPT::\n  Appt:       18:00...... Dummy event 2                                  :APPT::"

getEmacsOutput :: IO String
getEmacsOutput = do
  homedir <- getHomeDirectory
  readProcess "emacs" [ "-batch"
                      , "-l", homedir ++ "/.emacs.d/init.el"
                      , "-eval", "(setq org-agenda-sticky nil)",
                        "-eval", "(org-batch-agenda \"D\")"] ""

-- plumbing function definitions

runProcs :: [IO ()] -> IO ()
runProcs (x:xs) = do
  x
  runProcs xs

usageDie :: String -> [IO ()]
usageDie = undefined
-- usageDie msg = do
--   name <- getProgName
--   hPutStrLn stderr $ name ++ ": " ++ msg
