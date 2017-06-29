{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified GHC.IO.Encoding as E
import System.Environment
import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Map as M
import System.FilePath.Posix
import Hakyll
import Hakyll.Web.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Walk


conf :: Configuration
conf = defaultConfiguration

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      if takeBaseName p == "index"
      then takeDirectory p </> "index.html"
      else takeDirectory p </> takeBaseName p </> "index.html"
      where
        p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
  where
    pattern = "/index.html"
    replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
  | idx `isSuffixOf` url = take (length url - length idx) url
  | otherwise            = url
  where
    idx = "index.html"

docContext :: Context String
docContext = defaultContext

moduleFilter :: Pandoc -> Pandoc
moduleFilter = walk go
  where
    go (CodeBlock (ident0, classes, attrs0) code) =
      let attrs     = M.fromList attrs0
          lang      = M.lookup "rundoc-language" attrs
          name      = M.member "rundoc-name" attrs
          module_   = M.lookup "rundoc-module" attrs
          ident     = if ident0 == "" then Nothing else Just ident0
          head      = maybe [] (mkHead name) module_
          codeId    = fromMaybe "" (ident <|> (("module:" ++) <$> module_))
          codeBlock = CodeBlock (codeId, classes, attrs0) code
      in Div ("", [], []) (head ++ [codeBlock])
    go x = x

    mkHead name m = [ Header 5
                      ("", ["gt-module-section"], [])
                      (if name then [(Str ("\"" ++ m ++ "\""))] else [])
                    ]

pandocReaderOpts :: ReaderOptions
pandocReaderOpts = defaultHakyllReaderOptions

pandocWriterOpts :: WriterOptions
pandocWriterOpts = defaultHakyllWriterOptions

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyllWith conf $ do

    match "template/*.html" $ compile templateCompiler

    match "doc/**.org" $ do
      route cleanRoute
      compile $ pandocCompilerWithTransform pandocReaderOpts pandocWriterOpts moduleFilter
        >>= loadAndApplyTemplate "template/doc.html" docContext
        >>= relativizeUrls
        >>= cleanIndexUrls

    version "redirects" $ createRedirects
      [ ("index.html", "/doc/")
      ]

    match "static/**" $ do
      route $ gsubRoute "static/" (const "")
      compile copyFileCompiler
