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
  case (M.lookup "rundoc-language" attrs, M.lookup "rundoc-module" attrs) of
    (Just "ts", Just m) -> [convertModule m attrs children]
    (Just "check", _)   -> []
    (_, _)              -> [t]
  where
    attrs = M.fromList attrs0
convert t = [t]

convertModule :: Text -> M.Map Text Text -> [TagTree Text] -> TagTree Text
convertModule mod attrs children =
  TagBranch "details" detailAttrs [head, rest]
  where
    hide = M.member "rundoc-hide" attrs
    invisible = M.member "rundoc-invisible" attrs
    static = M.member "rundoc-static" attrs
    ident = M.lookup "id" attrs
    codeId = fromMaybe ("gt-module:" <> mod) ident
    head = summary mod
    openAttr = if hide then [] else [("open", "open")]
    displayAttr = if invisible then [("style", "display: none")] else []
    detailAttrs = openAttr <> displayAttr <> [("id", codeId), ("class", "gt-module-section")]
    tools = TagBranch "div" [("class", "gt-module-tools")] []
    output = TagBranch "ul" [("class", "gt-module-output")] []
    summary s = TagBranch "summary" [("data-gt-module", mod)]
                [ TagBranch "span" [("class", "gt-module-title")]
                  [ TagLeaf (TagText (s <> ".ts")) ]
                , TagBranch "i" [("class", "gt-less-more material-icons md-24 md-dark")] []
                , TagBranch "span" [("class", "gt-editor-load-spinner")]
                  [ TagBranch "i" [] []
                  , TagBranch "i" [] []
                  , TagBranch "i" [] []
                  ]
                ]
    code = TagBranch "pre" (M.toList attrs) children
    rest = TagBranch "div" [] (code : (if static then [] else [tools, output]))
