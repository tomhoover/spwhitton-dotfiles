{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens         hiding (argument)
import           Control.Monad.Reader
import           Options.Applicative
import           System.Directory     (doesFileExist, getDirectoryContents,
                                       makeAbsolute)
import           System.Exit          (ExitCode (..))
import           System.FilePath      (addExtension, dropExtension,
                                       takeBaseName, takeExtension,
                                       takeFileName, (</>))
import           System.IO            (BufferMode (..), hFlush, hSetBuffering,
                                       stdout)
import           System.Process       (readProcessWithExitCode)

-- #### Definitions

data Opts = Opts { _preset          :: String
                 , _audio           :: String
                 , _subtitle        :: String
                 , _directory       :: String
                 , _inputExtensions :: [String]
                 }
makeLenses ''Opts

-- #### The conversion process

video4iPad :: ReaderT Opts IO ()
video4iPad = do
    liftIO $ hSetBuffering stdout NoBuffering
    opts <- ask >>= liftIO . ensureAbsDirectory
    let extsFilter file = (drop 1 . takeExtension) file `elem` (opts ^. inputExtensions)
    let presetFilter = presetExtensionFilter opts
    files <- filterM (liftIO . doesFileExist)
             =<< (map (opts ^. directory </>))
             <$> (liftIO . getDirectoryContents) (opts ^. directory)
    let filteredFiles = filter (\f -> extsFilter f && f `notElem` [".", ".."]) . presetFilter $ files
    forM_ filteredFiles $ \file -> do
        o <- outputArg file
        let args = [ "-i"
                   , file
                   , "-o"
                   , o
                   , "--preset=" ++ opts ^. preset
                   , "--audio=" ++ opts ^. audio
                   , "--subtitle=" ++ opts ^. subtitle
                   , "--subtitle-burned=1"]
        liftIO . putStr $ "Processing " ++ (takeFileName file) ++ "... "
        liftIO . hFlush $ stdout
        (code, out, err) <- liftIO $ readProcessWithExitCode "HandBrakeCLI" args ""
        if code == ExitSuccess
           then liftIO . putStrLn $ "done"
           else do
                liftIO . putStrLn $ "failed"
                liftIO . putStrLn $ out ++ err

-- | Gives the output file extension of a HandBrake preset, if known.
presetExtension        :: String -> Maybe String
presetExtension "iPad" = Just "m4v"
presetExtension _      = Nothing

outputArg      :: FilePath -> ReaderT Opts IO FilePath
outputArg file = do
    opts <- ask
    return $
        maybe ((dropExtension file) `addExtension` "unknown")
        (\ext -> (dropExtension file) `addExtension` ext)
        $ presetExtension (opts ^. preset)

-- | For command line options, yields a filter to remove input files
-- for which an output file already exists, if the extension of output
-- files of a preset is known.
presetExtensionFilter        :: Opts -> ([FilePath] -> [FilePath])
presetExtensionFilter opts = maybe id extFilter $ presetExtension (opts ^. preset)
  where
    extFilter ext = \files -> filter (\f ->
        (drop 1 . takeExtension) f `notElem` (opts ^. inputExtensions)
        || (takeBaseName f `addExtension` ext) `notElem` (takeFileName <$> files)) files

-- | Ensures directory supplied on command line is absolute.
ensureAbsDirectory      :: Opts -> IO Opts
ensureAbsDirectory opts = do
    absDir <- makeAbsolute (opts ^. directory)
    return (opts & directory .~ absDir)

-- #### Process command line arguments

optsParser :: Parser Opts
optsParser = Opts
             <$> strOption (   long "format-preset"
                            <> short 'p'
                            <> metavar "PRESET"
                            <> value "iPad")
             <*> strOption (   long "audio-track"
                            <> short 'a'
                            <> metavar "AUDIOT"
                            <> value "1")
             <*> strOption (   long "subtitle-track"
                            <> short 's'
                            <> metavar "SUBST"
                            <> value "1")
             <*> argument str (metavar "DIRECTORY")
             <*> (some (argument str (metavar "EXT ..")) <|> pure ["mkv"])

main :: IO ()
main = runReaderT video4iPad
      =<< execParser (info (helper <*> optsParser)
                       (   fullDesc
                        <> progDesc "\nMass convert all videos in DIRECTORY with extension(s) EXT using HandBrake with HandBrake preset PRESET, choosing only one audio track AUDIOT and burning only one subtitle track SUBST.  Defaults to iPad preset, the first audio and subtitle tracks and files with extension 'mkv'."
                        <> header "video4ipad - mass convert videos for consumption"))
