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

-- parseEmacsOutput :: String -> [Reminder]
-- parseEmacsOutput = foldl' parseLine [] . drop 2 . lines
--   where apptRegexp = " ([0-9]{1,2}):([0-9][0-9])[-]{0,1}[0-9:]{0,5}[.]* (.*)$"
--         remindersAt = [60, 15, 0] :: [Int]
--         parseLine rems line
--           | length appt /= 0 = let hours = read $ appt !! 1
--                                    mins  = read $ appt !! 2
--                                    text  = appt !! 3
--                                in (Reminder hours mins text):rems
--           | otherwise        = rems
--           where appt = line =~ apptRegexp :: [String]

parseEmacsOutput :: String -> [Reminder]
parseEmacsOutput = foldl' parseLine [] . drop 2 . lines
  where apptRegexp = " ([0-9]{1,2}):([0-9][0-9])[-]{0,1}[0-9:]{0,5}[.]* (.*)$"
        parseLine rems line = let lineMatch = line =~ apptRegexp :: [[String]]
                                  hour:mins:text:[] = drop 1 . concat $ lineMatch
                                  hour' = read hour
                                  mins' = read mins
                                  (text',_,_) = text =~ "[ ]{2,}:.*:*$" :: (String,String,String)
                              in rems ++ [Reminder hour' mins' text']

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
