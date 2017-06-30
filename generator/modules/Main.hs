module Main where

import qualified GHC.IO.Encoding as E
import System.Directory
import System.Environment
import System.Exit
import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Data.Either
import System.FilePath.Posix
import Text.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Walk


moduleQuery :: Pandoc -> [(String, FilePath)]
moduleQuery = query go
  where
    go (CodeBlock (ident0, classes, attrs0) code) =
      let attrs     = M.fromList attrs0
          lang      = M.lookup "rundoc-language" attrs
          module_   = M.lookup "rundoc-module" attrs
          file      = M.lookup "rundoc-file" attrs
          f         = ((,) code) <$> file
          m         = mkML code <$> module_ <*> lang
      in maybe [] (:[]) (f <|> m)
    go _ = []

    mkML c m l = (c, m ++ "." ++ l)

writeModule :: FilePath -> (String, FilePath) -> IO ()
writeModule root (s, p) = do
  createDirectoryIfMissing True dir
  writeFile path s
  where
    path = root </> p
    dir  = takeDirectory path

fromRight :: (Show a) => Either a b -> b
fromRight (Left a)  = error (show a)
fromRight (Right b) = b

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  [org, root] <- getArgs
  s <- readFile org
  let doc = fromRight (readOrg def s)
  let modules = moduleQuery doc
  mapM_ (writeModule root) modules
