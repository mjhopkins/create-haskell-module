module CreateModule where

import qualified Data.List as L
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath
import           System.IO

newModule :: FilePath -> String -> IO ()
newModule dir nm = do
  hPutStrLn stderr . unwords $ ["Creating new module", nm, "in directory", dir ++ "."]
  let
    contents =
      unwords ["module", nm, "where"] ++ "\n\n"
    path = dir </> map tr nm ++ ".hs"
    tr '.' = '/'
    tr ch  = ch
  createDirectoryIfMissing True (parent path)
  writeFile path contents

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
