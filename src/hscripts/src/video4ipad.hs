{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens         hiding (argument)
import           Control.Monad.Reader
import           Options.Applicative
import           System.Directory     (getDirectoryContents, doesFileExist, makeAbsolute)
import           System.FilePath      (takeExtension, (</>))

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
    opts <- ask >>= liftIO . ensureAbsDirectory
    let extsFilter file = (tail . takeExtension) file `elem` (opts ^. inputExtensions)
    files <- filter extsFilter
             <$> filter (`notElem` [".", ".."])
             <$> (filterM (liftIO . doesFileExist)
             =<< (liftIO . getDirectoryContents) (opts ^. directory))
    liftIO $ putStrLn $ show files
    forM_ files $ \file -> do
        liftIO $ putStrLn file

-- | Gives the output file extension of a HandBrake preset.
presetExtension        :: String -> Maybe String
presetExtension "iPad" = Just "m4v"
presetExtension _      = Nothing

-- | Ensures directory supplied on command line is absolute.
ensureAbsDirectory      :: Opts -> IO Opts
ensureAbsDirectory opts = directory %~ makeAbsolute $ opts

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

main = runReaderT video4iPad
      =<< execParser (info (helper <*> optsParser)
                       (   fullDesc
                        <> progDesc "\nMass convert all videos in DIRECTORY with extension(s) EXT using HandBrake with HandBrake preset PRESET, choosing only one audio track AUDIOT and burning only one subtitle track SUBST.  Defaults to iPad preset, the first audio and subtitle tracks and files with extension 'mkv'."
                        <> header "video4ipad - mass convert videos for consumption"))
