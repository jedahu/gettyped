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
  case (lang, run, check) of
    (Just "ts", Just m, _)   -> [convertModule m attrs children]
    (Just "yaml", _, Just _) -> []
    _                        -> [t]
  where
    attrs = M.fromList attrs0
    lang  = M.lookup "rundoc-language" attrs
    run   = M.lookup "rundoc-module" attrs
    check = M.lookup "rundoc-check-module" attrs
convert t = [t]

convertModule :: Text -> M.Map Text Text -> [TagTree Text] -> TagTree Text
convertModule mod attrs children =
  TagBranch "section" detailAttrs [head, code]
  where
    hide = M.member "rundoc-hide" attrs
    invisible = M.member "rundoc-invisible" attrs
    static = M.member "rundoc-static" attrs
    ident = M.lookup "id" attrs
    codeId = fromMaybe ("gt-module:" <> mod) ident
    head = summary mod
    closed = if hide then " gt-expander-closed" else ""
    displayAttr = if invisible then [("style", "display: none")] else []
    detailAttrs = displayAttr
                  <>
                  [ ("id", codeId)
                  , ("class", "gt-module-section gt-expander" <> closed)
                  ]
    summary s = TagBranch "h6"
                [ ("class", "gt-module-title gt-expander-toggle")
                , ("data-gt-module", mod)
                ]
                [ TagBranch "span" [] [TagLeaf (TagText (s <> ".ts"))] ]
    code = TagBranch "div" [("class", "gt-module-insides gt-expander-content")]
           [ TagBranch "pre" (M.toList attrs) children ]
