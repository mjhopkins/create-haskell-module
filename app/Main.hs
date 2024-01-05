module Main where

import           System.Environment (getArgs)
import           System.IO
import           System.Exit

import CreateModule

main :: IO ()
main = do
  args <- getArgs
  case args of
    [s]               -> newModule "src" s
    ["-d", dir, s]    -> newModule dir s
    ["--dir", dir, s] -> newModule dir s
    [s, "-d", dir]    -> newModule dir s
    [s, "--dir", dir] -> newModule dir s
    _ -> do
      hPutStrLn stderr usage
      exitWith (ExitFailure 2)

usage :: String
usage = unlines
  [ "Usage: new-module [options] FQN"
  , ""
  , "Create a new empty Haskell module, creating directories if needed."
  , ""
  , "Command options:"
  , "  -d, --dir directory        Directory in which to create module (defaults to src);"
  ]
