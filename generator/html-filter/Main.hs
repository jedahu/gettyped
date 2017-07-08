{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified GHC.IO.Encoding as E
import Control.Applicative ((<*>), (*>))
import Control.Applicative ((<|>))
import Control.Category ((.))
import Data.Bool (Bool(..))
import Data.Default (def)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Eq ((==))
import Data.Foldable (elem)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Text.Lazy (Text, words)
import Data.Text.Lazy.IO (interact)
import Prelude (error)
import System.IO (IO)
import Text.HTML.TagSoup (Tag(..))
import Text.HTML.TagSoup.Tree (TagTree(..), parseTree, renderTree, transformTree)
import Text.Pandoc (readOrg, writeHtmlString)
import Text.Pandoc.Definition (Block(..), Inline(..), Pandoc)
import Text.Pandoc.JSON (toJSONFilter)
import Text.Show (Show, show)
import qualified Data.Map as M
import qualified GHC.IO.Encoding as E

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  interact (filterHtmlString convert)

filterHtmlString :: (TagTree Text -> [TagTree Text]) -> Text -> Text
filterHtmlString f = renderTree . transformTree f . parseTree

getAttrs :: TagTree Text -> M.Map Text Text
getAttrs (TagBranch _ attrs _)       = M.fromList attrs
getAttrs (TagLeaf (TagOpen _ attrs)) = M.fromList attrs
getAttrs _                           = M.fromList []

hasClass :: Text -> TagTree Text -> Bool
hasClass name tree = fromMaybe False val
  where
    val = elem name . words <$> M.lookup "class" (getAttrs tree)

convert :: TagTree Text -> [TagTree Text]
convert t@(TagBranch "pre" attrs0 children) =
  maybe
  [t]
  (\mod -> [convertRundocBlock mod attrs children])
  (if hasClass "rundoc-block" t
    then M.lookup "rundoc-module" attrs
    else Nothing)
  where
    attrs = M.fromList attrs0
convert t = [t]

convertRundocBlock :: Text -> M.Map Text Text -> [TagTree Text] -> TagTree Text
convertRundocBlock mod attrs children =
  TagBranch
  "details"
  [ ("open", if hide then "" else "open")
  , ("id", codeId)
  , ("class", "gt-module-section")
  ]
  [head, rest]
  where
    hide = M.member "rundoc-hide" attrs
    ident = M.lookup "id" attrs
    codeId = fromMaybe ("gt-module:" <> mod) ident
    head = summary mod
    foot = TagBranch "div" [("class", "gt-module-tools")] []
    summary s = TagBranch "summary" [] [TagLeaf (TagText s)]
    code = TagBranch "pre" (M.toList attrs) children
    rest = TagBranch "div" [] [code, foot]
