{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.UniocdeTable
-- Copyright   :  (C) 2019 Torsten Kemps-Benedix
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Torsten Kemps-Benedix (tkx68@icloud.com)
-- Stability   :  experimental
-- Portability :  portable
--
-- This package allows you to print simple text tables with suitable unicode characters for borders and edges.
--
-- @example
--
-- @
-- {-\# LANGUAGE OverloadedStrings, FlexibleContexts \#-}
-- module Main where
--'
-- import Data.Array.IArray
-- import Data.Text.Lazy.Builder
-- import Text.UnicodeTable
--'
-- main = do
--     let content = array ((0,0),(5,5)) [((r,c), fromString . show $ (r,c)) | r <- [0..5], c <- [0..5]]
--     let ws = listArray (0,5) (replicate 6 20)
--     let hs = listArray (0,6) (replicate 7 SingleLine)
--     let vs = listArray (0,6) (replicate 7 SingleLine)
--     let tbl = mkTable content ws hs vs
--     print tbl
-- @
--
-- This gives the following output:
--
-- @
-- ┌────────────────────┬────────────────────┬────────────────────┬────────────────────┬────────────────────┬────────────────────┐
-- │               (0,0)│               (0,1)│               (0,2)│               (0,3)│               (0,4)│               (0,5)│
-- ├────────────────────┼────────────────────┼────────────────────┼────────────────────┼────────────────────┼────────────────────┤
-- │               (1,0)│               (1,1)│               (1,2)│               (1,3)│               (1,4)│               (1,5)│
-- ├────────────────────┼────────────────────┼────────────────────┼────────────────────┼────────────────────┼────────────────────┤
-- │               (2,0)│               (2,1)│               (2,2)│               (2,3)│               (2,4)│               (2,5)│
-- ├────────────────────┼────────────────────┼────────────────────┼────────────────────┼────────────────────┼────────────────────┤
-- │               (3,0)│               (3,1)│               (3,2)│               (3,3)│               (3,4)│               (3,5)│
-- ├────────────────────┼────────────────────┼────────────────────┼────────────────────┼────────────────────┼────────────────────┤
-- │               (4,0)│               (4,1)│               (4,2)│               (4,3)│               (4,4)│               (4,5)│
-- ├────────────────────┼────────────────────┼────────────────────┼────────────────────┼────────────────────┼────────────────────┤
-- │               (5,0)│               (5,1)│               (5,2)│               (5,3)│               (5,4)│               (5,5)│
-- └────────────────────┴────────────────────┴────────────────────┴────────────────────┴────────────────────┴────────────────────┘
-- @
----------------------------------------------------------------------------
module Text.UnicodeTable (mkTable, Table(..), LineType(..), fromTable) where

import           Prelude                     hiding (Left, Right)
import           Data.Array.IArray           (Array)
import qualified Data.Array.IArray           as A
import qualified Data.List                   as L
import qualified Data.Text.Lazy              as T
import qualified Data.Text.Lazy.Builder      as TB
import qualified Formatting                  as F

data LineType = NoLine | SingleLine | DoubleLine deriving (Show, Eq)

data Table = Table {
    content :: Array (Int, Int) TB.Builder,
    columnWidth :: Array Int Int,
    hLines :: Array Int LineType,
    vLines :: Array Int LineType}

-- |The 'mkTable' function safely constructs a table and checks the correctness of all dimensions.
--  All lower bounds must be 0. The number of 'columnWidths' must be euqal to the number of columns
--  in the 'content'. The 'hLines' and 'vLines' must contain one element more than the rows resp.
--  columns of the 'content'.
mkTable ::
    Array (Int, Int) TB.Builder -- ^ The first argument 'content' is a two dimensional 'Array' of 'Data.Text.Lazy.Builder.Builder's and specifies what shall go into the cells of the table.
    -> Array Int Int -- ^ The second argument 'columnWidth' specifies the widths of the columns as the number of characters they may contain.
    -> Array Int LineType -- ^ The third argument 'hLines' specifies the 'LineType's of the horizontal lines.
    -> Array Int LineType -- ^ The fourth argument 'vLines' specifies the 'LineType's of the vertical lines.
    -> Table
mkTable content columnWidths hLines vLines =
    let ((n0,n1),(m0,m1)) = A.bounds content
        (nW0,nW1) = A.bounds columnWidths
        (nH0,nH1) = A.bounds hLines
        (nV0,nV1) = A.bounds vLines
    in if n0/=0 || m0/=0 || nW0/=0 || nH0/=0 || nV0/=0
            then error "All lower bounds shall be 0!"
            else if m1/=nW1
                then error "Number of column widths must be equal to number of columns of content!"
                else if m1/=nV1+1
                    then error "Number of columns of content must be one less than number of vLines!"
                    else if n1/=nH1+1
                    then error "Number of rows of content must be one less than number of hLines!"
                    else Table content columnWidths hLines vLines

data LineDirection = Vertical | Horizontal | Cross | VerticalAndLeft |
                     VerticalAndRight | HorizontalAndDown | HorizontalAndUp deriving Show

lineType2Char :: LineDirection -> LineType -> Char
lineType2Char _ NoLine = ' '
lineType2Char Vertical SingleLine = '│'
lineType2Char Vertical DoubleLine = '║'
lineType2Char Horizontal SingleLine = '─'
lineType2Char Horizontal DoubleLine = '═'
lineType2Char _ _ = '*'

data EdgeType = Inside | Top | TopLeft | TopRight |
                Bottom | BottomLeft | BottomRight | Left | Right deriving Show

edge :: LineType -> LineType -> EdgeType -> Char
edge DoubleLine DoubleLine Bottom = '╩'
edge DoubleLine DoubleLine BottomLeft = '╚'
edge DoubleLine DoubleLine BottomRight = '╝'
edge DoubleLine DoubleLine Inside = '╬'
edge DoubleLine DoubleLine Left = '╠'
edge DoubleLine DoubleLine Right = '╣'
edge DoubleLine DoubleLine Top = '╦'
edge DoubleLine DoubleLine TopLeft = '╔'
edge DoubleLine DoubleLine TopRight = '╗'
edge DoubleLine NoLine _ = '═'
edge DoubleLine SingleLine Bottom = '╧'
edge DoubleLine SingleLine BottomLeft = '╘'
edge DoubleLine SingleLine BottomRight = '╛'
edge DoubleLine SingleLine Inside = '╪'
edge DoubleLine SingleLine Left = '╞'
edge DoubleLine SingleLine Right = '╡'
edge DoubleLine SingleLine Top = '╤'
edge DoubleLine SingleLine TopLeft = '╒'
edge DoubleLine SingleLine TopRight = '╕'
edge NoLine DoubleLine _ = '║'
edge NoLine NoLine _ = ' '
edge NoLine SingleLine _ = '│'
edge SingleLine DoubleLine Bottom = '╨'
edge SingleLine DoubleLine BottomLeft = '╙'
edge SingleLine DoubleLine BottomRight = '╜'
edge SingleLine DoubleLine Inside = '╫'
edge SingleLine DoubleLine Left = '╟'
edge SingleLine DoubleLine Right = '╢'
edge SingleLine DoubleLine Top = '╥'
edge SingleLine DoubleLine TopLeft = '╓'
edge SingleLine DoubleLine TopRight = '╖'
edge SingleLine NoLine _ = '─'
edge SingleLine SingleLine Bottom = '┴'
edge SingleLine SingleLine BottomLeft = '└'
edge SingleLine SingleLine BottomRight = '┘'
edge SingleLine SingleLine Inside = '┼'
edge SingleLine SingleLine Left = '├'
edge SingleLine SingleLine Right = '┤'
edge SingleLine SingleLine Top = '┬'
edge SingleLine SingleLine TopLeft = '┌'
edge SingleLine SingleLine TopRight = '┐'

-- |The 'fromTable' function constructs a 'Data.Text.Lazy.Builder.Builder' from the 'content' and the
-- other parts of the table specification. You can further convert this
-- 'Data.Text.Lazy.Builder.Builder' to 'Data.Text.Lazy.Text' with
-- 'Data.Text.Lazy.Builder.toLazyText'. You may also simply 'show' a 'Table' and get it as a 'String'.
fromTable :: Table -> TB.Builder
fromTable Table{..} =
    let as :: [((Int, Int), TB.Builder)]
        as = A.assocs content
        ((_,_),(r1,c1)) = A.bounds content
        edgeType r c | r==0 && c==0 = TopLeft
                     | r==0 && c==c1+1 = TopRight
                     | r==0 = Top
                     | r<=r1 && c==0 = Left
                     | r<=r1 && c==c1+1 = Right
                     | r<=r1 = Inside
                     | r==r1+1 && c==0 = BottomLeft
                     | r==r1+1 && c==c1+1 = BottomRight
                     | r==r1+1 = Bottom
                     | otherwise = error "There is no edge here."
        hBar r = if hLines A.! r == NoLine
                    then mempty
                    else foldl (\b (c, w) -> b `mappend` (TB.fromString $ (L.replicate w $
                                    lineType2Char Horizontal (hLines A.! r)))
                                  `mappend` (TB.singleton $
                                    edge (hLines A.! r) (vLines A.! (c+1)) (edgeType r (c+1))))
                       (TB.singleton $ edge (hLines A.! r) (vLines A.! 0) (edgeType r 0))
                       (A.assocs columnWidth)
                    `mappend` TB.fromLazyText "\n"
        vBar c = TB.singleton (lineType2Char Vertical (vLines A.! c))
    in (foldl (\b ((r,c), t) -> b `mappend` (if c==0 then hBar r `mappend`vBar 0 else mempty)
                                  `mappend` F.bprint (F.left (columnWidth A.! c) ' ') t
                                  `mappend` vBar (c+1)
                                  `mappend` (if c==c1
                                                then (TB.fromLazyText "\n")
                                                else mempty))
             mempty
             as) `mappend` hBar (r1+1)

instance Show Table where
    show = T.unpack . TB.toLazyText . fromTable
