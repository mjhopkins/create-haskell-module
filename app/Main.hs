{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Char (isSpace)
import qualified Data.List as L
import           Data.List (isPrefixOf)
import           Data.Maybe
import           Data.Tuple.Extra
import           System.Environment (getArgs)
import           System.Exit

import           CreateModule

main :: IO ()
main = getArgs >>= maybe exitUsage (uncurry3 run) . processArgs
  where
    exitUsage = logStr "ERROR: could not parse arguments." >> logStr usage >> exitWith (ExitFailure 2)

run :: FilePath -> Bool -> String -> IO ()
run dir force = either handleErr return <=< runExceptT . newModule dir force . trim
  where
    handleErr e = do
      logStr $ "ERROR: " ++ e
      exitFailure

usage :: String
usage = unlines
  [ "Usage: new-hs-module [options] FQN"
  , ""
  , "Create a new empty Haskell module, creating directories as required."
  , ""
  , "Command options:"
  , "  -d, --dir directory        Directory in which to create module (defaults to src)."
  , "  -f, --force                Overwrite the file if it already exists."
  ]

-- | Returns (directory, force, module name)
processArgs :: [String] -> Maybe (String, Bool, String)
processArgs = go Nothing Nothing Nothing
  where
  go md mf (Just n) []                       = Just (fromMaybe "src" md, fromMaybe False mf, n)
  go md mf Nothing  (NotOpt n:ss)            = go md mf (Just n) ss
  go md mf mn       (Long "dir":NotOpt d:ss) = maybe (go (Just d) mf mn ss) (const Nothing) md
  go md mf mn       (Short "d" :NotOpt d:ss) = maybe (go (Just d) mf mn ss) (const Nothing) md
  go md mf mn       (Long "force":ss)        = maybe (go md (Just True) mn ss) (const Nothing) mf
  go md mf mn       (Short "f":ss)           = maybe (go md (Just True) mn ss) (const Nothing) mf
  go  _  _  _       _                        = Nothing

pattern Long :: String -> String
-- pattern Long s <- (long -> Just s) where Long = ("--" ++ )
pattern Long s <- (long -> Just s) where Long = ("--" ++ )

pattern Short :: String -> String
pattern Short s <- (short -> Just s) where Short = ("-" ++)

pattern NotOpt :: String -> String
pattern NotOpt s <- (notOpt -> Just s) where NotOpt = id

long :: String -> Maybe String
long s
  | "--" `isPrefixOf` s = Just $ L.drop 2 s
  | otherwise           = Nothing

short :: String -> Maybe String
short s
  | "-" `isPrefixOf` s = Just $ L.drop 1 s
  | otherwise          = Nothing

notOpt :: String -> Maybe String
notOpt s
  | "--" `isPrefixOf` s = Nothing
  | "-" `isPrefixOf` s  = Nothing
  | otherwise           = Just s

trim :: String -> String
trim = L.dropWhile isSpace . L.dropWhileEnd isSpace
