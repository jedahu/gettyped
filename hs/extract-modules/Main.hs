module Main where

import qualified GHC.IO.Encoding as E
import System.Directory
import System.Environment
import System.Exit
import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Data.Either
import Data.Semigroup
import System.FilePath.Posix
import Text.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Walk

data ErrorKind = None | Runtime | Static

moduleQuery :: Pandoc -> [(String, FilePath, ErrorKind)]
moduleQuery = query go
  where
    go (CodeBlock (ident0, classes, attrs0) code) =
      let attrs     = M.fromList attrs0
          isStatic  = M.member "rundoc-static" attrs
      in
        if isStatic
        then []
        else
          let lang      = M.lookup "rundoc-language" attrs
              module_   = M.lookup "rundoc-module" attrs
              file      = M.lookup "rundoc-file" attrs
              err       = errKind (M.lookup "rundoc-error" attrs)
              f         = ((,,) code) <$> file <*> Just err
              m         = mkML code <$> module_ <*> (suffix <$> lang) <*> Just err
          in maybe [] (:[]) (f <|> m)
    go _ = []

    mkML c m s e = (c, m ++ "." ++ s, e)

    suffix "ts" = "ts"
    suffix "check" = "ts.check"

    errKind (Just "runtime") = Runtime
    errKind (Just "static")  = Static
    errKind (Just e)         = error ("Invalid error kind " <> e)
    errKind Nothing          = None

writeModule :: FilePath -> (String, FilePath, ErrorKind) -> IO ()
writeModule root (s, p, e) = do
  createDirectoryIfMissing True dir
  writeFile path s
  touchErrFile e
  where
    path = root </> p
    dir  = takeDirectory path
    touchErrFile None    = return ()
    touchErrFile Runtime = writeFile (path <> ".re") ""
    touchErrFile Static  = writeFile (path <> ".se") ""

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
