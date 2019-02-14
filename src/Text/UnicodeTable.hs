{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
module Text.UnicodeTable (Table(..), LineType(..), fromTable) where

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
