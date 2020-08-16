{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Options.Applicative
import System.Directory
import System.Environment
import System.FilePath.Posix
import System.Process

monitorsFile :: FilePath
monitorsFile = ".local/share/switch-monitor/monitors.txt"

resolvedFile :: IO FilePath
resolvedFile = getHomeDirectory >>= (return . (</> monitorsFile))

reloadList :: IO ()
reloadList = do
  monitorsRaw <- readProcess "xrandr" ["--listmonitors"] []
  filepath <- resolvedFile
  createDirectoryIfMissing True $ takeDirectory filepath
  writeFile filepath monitorsRaw

moveMouse :: Mode -> IO ()
moveMouse mode = do
  monitorsRaw <- resolvedFile >>= readFile
  mouseRaw <- readProcess "xdotool" ["getmouselocation", "--shell"] []
  let (x, y) = nextMousePosition mode (parseMonitors monitorsRaw) (parseMouse mouseRaw)
  readProcess "xdotool" ["mousemove", show x, show y] "" >> return ()

data Opts = Opts
  { reloadMonitors :: !Bool,
    movePrevious :: !Bool,
    moveNext :: !Bool
  }

main = do
  opts <- execParser optsParser
  if | reloadMonitors opts -> reloadList
     | moveNext opts -> moveMouse Next
     | movePrevious opts -> moveMouse Previous
     | otherwise -> putStrLn "No command given, see --help for details"
  where
    optsParser :: ParserInfo Opts
    optsParser =
      info
        (helper <*> programOptions)
        ( fullDesc <> progDesc "Move mouse between multiple monitors"
            <> header "Switch Monitors"
        )
    programOptions :: Parser Opts
    programOptions =
      Opts <$> switch (long "reload" <> short 'r' <> help "Reload monitor configuration")
        <*> switch (long "previous" <> short 'p' <> help "Move mouse to previous monitor")
        <*> switch (long "next" <> short 'n' <> help "Move mouse to next monitor")
