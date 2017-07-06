{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Control.Applicative ((<*>))
import Control.Applicative ((<|>))
import Control.Category ((.))
import Data.Bool (Bool(..))
import Data.Default (def)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Eq ((==))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.String (String)
import Prelude (error)
import System.IO (IO)
import Text.Pandoc (readOrg, writeHtmlString)
import Text.Pandoc.Definition (Block(..), Inline(..), Pandoc)
import Text.Pandoc.JSON (toJSONFilter)
import Text.Show (Show, show)
import qualified Data.Map as M
import qualified GHC.IO.Encoding as E


mkHead :: String -> [Block]
mkHead m = [ Header 5
             ("", ["gt-module-section"], [])
             [(Str ("\"" <> m <> "\""))]
           ]

moduleDecorator :: Block -> Block
moduleDecorator (CodeBlock (ident0, classes, attrs0) code) =
  let attrs     = M.fromList attrs0
      lang      = M.lookup "rundoc-language" attrs
      module_   = M.lookup "rundoc-module" attrs
      ident     = if ident0 == "" then Nothing else Just ident0
      head      = maybe [] mkHead module_
      codeId    = fromMaybe "" (ident <|> (("module:" <>) <$> module_))
      codeBlock = CodeBlock (codeId, classes, attrs0) code
  in Div ("", [], []) (head <> [codeBlock])
moduleDecorator x = x

fromRight :: (Show a) => Either a b -> b
fromRight (Left a)  = error (show a)
fromRight (Right b) = b

readIn :: String -> Pandoc
readIn = fromRight . readOrg def

writeOut :: Pandoc -> String
writeOut = writeHtmlString def

main :: IO ()
main = toJSONFilter moduleDecorator
