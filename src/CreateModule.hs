module CreateModule where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Data.List as L
import           System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, doesPathExist)
import           System.FilePath
import           System.IO
import           Text.Regex.TDFA

newModule :: FilePath -> Bool -> String -> ExceptT String IO ()
newModule dir force modName = do
  checkValidName modName
  let path = nameToPath dir modName
  checkIfPathExists path force
  logStr . unwords $ ["Creating new module", modName, "in directory", dir ++ "."]
  liftIO $ createDirectoryIfMissing True (parent path)
  liftIO $ writeFile path (moduleContents modName)

checkValidName :: Monad m => [Char] -> ExceptT String m ()
checkValidName name
  = unless (isValidModuleName name)
  . throwE
  . unwords
  $ ["\"" ++ name ++ "\"", "is not a valid module name."]

nameToPath :: FilePath -> String -> FilePath
nameToPath dir name = dir </> map tr name ++ ".hs"
  where
    tr '.' = '/'
    tr ch  = ch

checkIfPathExists :: MonadIO m => FilePath -> Bool -> ExceptT String m ()
checkIfPathExists path force = do
  pathExists <- liftIO $ doesPathExist path
  when (pathExists && not force) $ do
    dirExists  <- liftIO $ doesDirectoryExist path
    fileExists <- liftIO $ doesFileExist path
    let ptype = case (dirExists, fileExists) of
          (True, _)      -> "Directory"
          (_, True)      -> "File"
          (False, False) -> "Path"
    let msg = [ptype, path, "already exists. Not overwriting without --force."]
    throwE . unwords $ msg

moduleContents :: String -> String
moduleContents nm = unwords ["module", nm, "where"] ++ "\n\n"

{- | NB. Unicode not currently supported. -}
isValidModuleName :: String -> Bool
isValidModuleName s = s =~ anchor modRegex

anchor :: String -> String
anchor r = "\\`" ++ r ++ "\\'"

modRegex :: String
modRegex = cmpRegex ++ "(\\." ++ cmpRegex ++ ")*"

cmpRegex :: String
cmpRegex = "([A-Z][A-Za-z0-9_']*)"

{-
From the Haskell 2010 Report:

conid    → large {small | large | digit | '}
modid    → {conid.}conid
large    → ascLarge | uniLarge
ascLarge → A|B|..|Z
uniLarge → any uppercase or titlecase Unicode letter
small    → ascSmall | uniSmall | _
ascSmall → a|b|..|z
uniSmall → any Unicode lowercase letter
digit    → ascDigit | uniDigit
ascDigit → 0 | 1 | . . . | 9
uniDigit → any Unicode decimal digit

Not currently making any attempt to support Unicode.
-}

pathComponents :: FilePath -> [String]
pathComponents = go []
  where
    go acc "" = L.reverse acc
    go acc s =
      case L.break (== '/') s of
        (a,[])   -> go (a:acc) []
        (a,_:cs) -> go (a:acc) cs

parent :: FilePath -> FilePath
parent fp = case pathComponents fp of
  l@(_:_) -> L.intercalate "/" $ init l
  _       -> error $ "Can't get parent of " ++ fp

logStr :: MonadIO m => String -> m ()
logStr = liftIO . hPutStrLn stderr
