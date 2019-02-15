{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where

import Data.Array.IArray
import Data.Text.Lazy.Builder
import Text.UnicodeTable

main = do
    let content = array ((0,0),(5,5)) [((r,c), fromString . show $ (r,c)) | r <- [0..5], c <- [0..5]]
    let ws = listArray (0,5) (replicate 6 20)
    let hs = listArray (0,6) (replicate 7 SingleLine)
    let vs = listArray (0,6) (replicate 7 SingleLine)
    let tbl = mkTable content ws hs vs
    print tbl
