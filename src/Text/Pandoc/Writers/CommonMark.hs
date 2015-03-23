{-
Copyright (C) 2015 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.CommonMark
   Copyright   : Copyright (C) 2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to CommonMark.

CommonMark:  <http://commonmark.org>
-}
module Text.Pandoc.Writers.CommonMark (writeCommonMark) where
import Text.Pandoc.Definition
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Options
import Text.Pandoc.Readers.TeXMath (texMathToInlines)
import CMark
import qualified Data.Text as T
import Control.Monad.Identity (runIdentity, Identity)

-- | Convert Pandoc to CommonMark.
writeCommonMark :: WriterOptions -> Pandoc -> String
writeCommonMark opts (Pandoc meta blocks) = rendered
  where main = runIdentity $ blocksToCommonMark opts blocks
        metadata = runIdentity $ metaToJSON opts
                     (blocksToCommonMark opts)
                     (inlinesToCommonMark opts)
                     meta
        context = defField "body" main $ metadata
        rendered = if writerStandalone opts
                      then renderTemplate' (writerTemplate opts) context
                      else main

node :: NodeType -> [Node] -> Node
node = Node Nothing

blocksToCommonMark :: WriterOptions -> [Block] -> Identity String
blocksToCommonMark opts bs = return $
  T.unpack $ nodeToCommonmark cmarkOpts colwidth
           $ node DOCUMENT (blocksToNodes bs)
   where cmarkOpts = [optHardBreaks | isEnabled Ext_hard_line_breaks opts]
         colwidth = if writerWrapText opts
                       then writerColumns opts
                       else 0

inlinesToCommonMark :: WriterOptions -> [Inline] -> Identity String
inlinesToCommonMark opts ils = return $
  T.unpack $ nodeToCommonmark cmarkOpts colwidth
           $ node PARAGRAPH (inlinesToNodes ils)
   where cmarkOpts = [optHardBreaks | isEnabled Ext_hard_line_breaks opts]
         colwidth = if writerWrapText opts
                       then writerColumns opts
                       else 0

blocksToNodes :: [Block] -> [Node]
blocksToNodes = foldr blockToNodes []

blockToNodes :: Block -> [Node] -> [Node]
blockToNodes (Plain xs) = (node PARAGRAPH (inlinesToNodes xs) :)
blockToNodes (Para xs) = (node PARAGRAPH (inlinesToNodes xs) :)
blockToNodes (CodeBlock (_,classes,_) xs) =
  (node (CODE_BLOCK (T.pack (unwords classes)) (T.pack xs)) [] :)
blockToNodes (RawBlock fmt xs)
  | fmt == Format "html" = (node (HTML (T.pack xs)) [] :)
  | otherwise = id
blockToNodes (BlockQuote bs) =
  (node BLOCK_QUOTE (blocksToNodes bs) :)
blockToNodes (BulletList items) =
  (node (LIST ListAttributes{
               listType = BULLET_LIST,
               listDelim = PERIOD_DELIM,
               listTight = case items of
                                ((Plain _ : _):_) -> True
                                _                 -> False,
               listStart = 1 }) (map (node ITEM . blocksToNodes) items) :)
blockToNodes (OrderedList (start, _sty, delim) items) =
  (node (LIST ListAttributes{
               listType = ORDERED_LIST,
               listDelim = case delim of
                                OneParen  -> PAREN_DELIM
                                TwoParens -> PAREN_DELIM
                                _         -> PERIOD_DELIM,
               listTight = case items of
                                ((Plain _ : _):_) -> True
                                _                 -> False,
               listStart = start }) (map (node ITEM . blocksToNodes) items) :)
blockToNodes HorizontalRule = (node HRULE [] :)
blockToNodes (Header lev _ ils) = (node (HEADER lev) (inlinesToNodes ils) :)
blockToNodes (Div _ bs) = (blocksToNodes bs ++)
blockToNodes Null = id


{-
    | DefinitionList [([Inline],[[Block]])]  -- ^ Definition list
                            -- Each list item is a pair consisting of a
                            -- term (a list of inlines) and one or more
                            -- definitions (each a list of blocks)
    | Table [Inline] [Alignment] [Double] [TableCell] [[TableCell]]  -- ^ Table\
,
                            -- with caption, column alignments (required),
                            -- relative column widths (0 = default),
                            -- column headers (each a list of blocks), and
                            -- rows (each a list of lists of blocks)
-}

inlinesToNodes :: [Inline] -> [Node]
inlinesToNodes  = foldr inlineToNodes []

inlineToNodes :: Inline -> [Node] -> [Node]
inlineToNodes (Str s) = (node (TEXT (T.pack s)) [] :)
inlineToNodes Space   = (node (TEXT (T.pack " ")) [] :)
inlineToNodes LineBreak = (node LINEBREAK [] :)
inlineToNodes (Emph xs) = (node EMPH (inlinesToNodes xs) :)
inlineToNodes (Strong xs) = (node STRONG (inlinesToNodes xs) :)
inlineToNodes (Strikeout xs) =
  ((node (HTML (T.pack "<s>")) [] : inlinesToNodes xs ++
   [node (HTML (T.pack "</s>")) []]) ++ )
inlineToNodes (Superscript xs) =
  ((node (HTML (T.pack "<sub>")) [] : inlinesToNodes xs ++
   [node (HTML (T.pack "</sub>")) []]) ++ )
inlineToNodes (Subscript xs) =
  ((node (HTML (T.pack "<sup>")) [] : inlinesToNodes xs ++
   [node (HTML (T.pack "</sup>")) []]) ++ )
inlineToNodes (SmallCaps xs) =
  ((node (HTML (T.pack "<span style=\"font-variant:small-caps;\">")) [] :
    inlinesToNodes xs ++
    [node (HTML (T.pack "</span>")) []]) ++ )
inlineToNodes (Link ils (url,tit)) =
  (node (LINK (T.pack url) (T.pack tit)) (inlinesToNodes ils) :)
inlineToNodes (Image ils (url,tit)) =
  (node (IMAGE (T.pack url) (T.pack tit)) (inlinesToNodes ils) :)
inlineToNodes (RawInline fmt xs)
  | fmt == Format "html" = (node (INLINE_HTML (T.pack xs)) [] :)
  | otherwise = id
inlineToNodes (Span _ ils) = (inlinesToNodes ils ++)


{-
    | Quoted QuoteType [Inline] -- ^ Quoted text (list of inlines)
    | Cite [Citation]  [Inline] -- ^ Citation (list of inlines)
    | Code Attr String      -- ^ Inline code (literal)
    | Math MathType String  -- ^ TeX math (literal)
    | Note [Block]          -- ^ Footnote or endnote
-}
