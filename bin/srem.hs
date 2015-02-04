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
  | 1 == 1 = [do
      emacsOutput <- getEmacsOutput
      print $ parseEmacsOutput emacsOutput]
  | otherwise = usageDie "invalid arguments"
  where argc = length args

parseEmacsOutput :: String -> [Reminder]
parseEmacsOutput = foldl' parseLine [] . drop 2 . lines
  where apptRegexp = " ([0-9]{1,2}):([0-9][0-9])[-]{0,1}[0-9:]{0,5}[.]* (.*)$"
        snipRegexp = "[ ]{2,}:.*:*$"

        parseLine rems line
          | lineMatch = rems ++ staggeredReminders hour' mins' text'
          | otherwise = rems
          where lineMatch = line =~ apptRegexp :: Bool
                lineMatchStrings = line =~ apptRegexp :: [[String]]
                hour:mins:text:[] = drop 1 . concat $ lineMatchStrings

                hour' = read hour
                mins' = read mins
                (text',_,_) = text =~ snipRegexp :: (String,String,String)

staggeredReminders :: Int -> Int -> String -> [Reminder]
staggeredReminders hour mins text = foldl' step [] [60, 15, 0]
  where step rems diff = let hour'
                               | diff > mins = hour - 1
                               | otherwise = hour
                             -- could do with addition mod 60
                             mins'
                               | mins >= diff = mins - diff
                               | otherwise = 60 + (mins - diff)
                         in rems ++ [Reminder hour' mins' text]

getEmacsOutput :: IO String
getEmacsOutput = do
  homedir <- getHomeDirectory
  readProcess "emacs" [ "-batch"
                      , "-l", homedir ++ "/.emacs.d/init.el"
                      , "-eval", "(setq org-agenda-sticky nil)",
                        "-eval", "(org-batch-agenda \"D\")"] ""

-- plumbing function definitions

runProcs :: [IO ()] -> IO ()
runProcs [] = return ()
runProcs (x:xs) = do
  x
  runProcs xs

usageDie :: String -> [IO ()]
usageDie = undefined
-- usageDie msg = do
--   name <- getProgName
--   hPutStrLn stderr $ name ++ ": " ++ msg
