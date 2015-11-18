-- 0. check for filename collisions and generate .htaccess entry
-- 1. parse first two lines: produce date=, modified= and title= (second one just in case it can be made to take effect)
-- 2. pass remainder through pandoc (with options to use old-style markdown: for example, need a blank line between a paragraph and a list)
-- 3. fix MORE and links in bottom part, and add tags (and imported_pyblosxom tag)
-- 4. put file into entries dir, and images into wikiannex

-- There are no blog post filename conflicts.  There are the following attachment conflicts:

-- ["/home/swhitton/local/big/blog/korea/epik/classroom.jpg"
-- ,"/home/swhitton/local/big/blog/writing/diary/jhcoip/classroom.jpg"
-- ,"/home/swhitton/local/big/blog/korea/epik/classroomthumb.jpg"
-- ,"/home/swhitton/local/big/blog/writing/diary/jhcoip/classroomthumb.jpg"
-- ,"/home/swhitton/local/big/blog/tech/emacs/org-mode/freemindeg.html_files/icons/help.png"
-- ,"/home/swhitton/local/big/blog/oxford/oliscrot/help.png"
-- ,"/home/swhitton/local/big/blog/korea/epik/temple.jpg"
-- ,"/home/swhitton/local/big/blog/writing/diary/jhcoip/temple.jpg"
-- ,"/home/swhitton/local/big/blog/korea/epik/templethumb.jpg"
-- ,"/home/swhitton/local/big/blog/writing/diary/jhcoip/templethumb.jpg"]

import           Data.String.Utils    (replace)
import           System.FilePath.Find

-- import           System.Directory
-- import           System.FilePath

-- import           Control.Monad
-- import           Data.List

convertEntry :: String -> String -> String
convertEntry category source = unlines $ meta ++ tags ++ body
  where
    meta = parseMeta . take 2 . lines $ source
    tags = parseCat category
    body = convertBody . unlines . drop 2 . lines $ source

-- work by replacing '/' with ' ' in strings like "/tech/gnu+linux"
parseCat :: String -> [String]
parseCat category = ("[[!tag" ++ (replace "/" " " category) ++ " ]]"):[]

parseMeta :: [String] -> [String]
parseMeta (title:date:[]) = ("[[!meta title=\"" ++ drop 7 title ++ "\"]]")
                            : ("[[!meta date=\"" ++ drop 19 date ++ "\"]]")
                            : []

convertBody :: String -> [String]

main = do
    let rootDir = "/home/swhitton/doc/www/blog"
    entries <- find (pure True) (extension ==? ".org") rootDir
    attachments <- find (pure True) (not <$> extension ==? ".org") rootDir

    undefined

-- getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
-- getDirectoryContentsRecursive path = do
--     names <- filter (`notElem` [".", "..", ".git"]) <$> getDirectoryContents path
--     liftM concat $ forM names $ \name -> do
--         dir <- doesDirectoryExist (path </> name)
--         if dir
--             then getDirectoryContentsRecursive (path </> name)
--             else return [path </> name]

-- checkForDuplicates :: IO ()
-- checkForDuplicates = do
--     names <- (++)
--              <$> getDirectoryContentsRecursive "/home/swhitton/local/big/blog"
--              <*> getDirectoryContentsRecursive "/home/swhitton/src/wiki/blog/entry"
--     let posts = takeBaseName <$> filter ((`elem` [".org", ".mdwn"]) . takeExtension) names
--     let other = filter ((`notElem` [".org", ".mdwn"]) . takeExtension) names
--     let other' = takeBaseName <$> other
--     -- let otherDupes = remNonDupes . sortOn takeBaseName $ other
--     let otherDupes = sortOn takeBaseName [ x | x <- other, takeBaseName x `elem` (takeBaseName <$> (other `listMinus` x))]
--     if nub posts == posts
--         then putStrLn "no post filenames conflict" -- this gets printed
--         else putStrLn "oh no!  post filenames conflict!"
--     if nub other' == other'
--         then putStrLn "no attachment filenames conflict"
--         else putStrLn "oh no!  attachment filenames conflict!" -- this gets printed!
--     putStrLn . show $ otherDupes

-- remNonDupes :: (Eq a) => [a] -> [a]
-- remNonDupes = remNonDupes' []

-- remNonDupes' :: (Eq a) => [a] -> [a] -> [a]
-- remNonDupes' ys [] = ys
-- remNonDupes' ys (x:xs) = if x `elem` xs || x `elem` ys
--                          then remNonDupes' (ys ++ [x]) xs
--                          else remNonDupes' ys xs

-- main :: IO ()
-- main = checkForDuplicates

-- listMinus :: (Eq a) => [a] -> a -> [a]
-- listMinus (x:xs) y = if x == y then xs else x:listMinus xs y
