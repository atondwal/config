{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances, DeriveDataTypeable #-}
module MyAccordion (
    MyAccordion(MyAccordion)) where

import XMonad
import qualified XMonad.StackSet as W

import GHC.Word (Word32)

data MyAccordion a = MyAccordion deriving ( Read, Show )

instance LayoutClass MyAccordion Window where
    pureLayout _ sc ws = zip ups tops ++ [(W.focus ws, mainPane)] ++ zip dns bottoms
     where
       ups    = reverse $ W.up ws
       dns    = W.down ws
       (tops, allButTop) = splitVerticallyWU 20 (length ups) sc
       (center, bottoms) = splitVerticallyWD 20 (length dns) allButTop
       mainPane = center

splitVerticallyWU :: (Ord a, Num a) => Dimension -> a -> Rectangle -> ([Rectangle], Rectangle)
splitVerticallyWU _ n r | n < 1 = ([],r)
splitVerticallyWU w n (Rectangle sx sy sw sh) = (Rectangle sx sy sw w : rs, sc)
  where (rs, sc) = splitVerticallyWU w (n-1) (Rectangle sx (sy+fromIntegral w) sw (sh-w))

splitVerticallyWD :: (Ord a, Num a) => Word32 -> a -> Rectangle -> (Rectangle, [Rectangle])
splitVerticallyWD _ n r | n < 1 = (r,[])
splitVerticallyWD w n (Rectangle sx sy sw sh) = (sc, rs ++ [bot])
  where main = Rectangle sx sy sw (sh-w)
        bot = Rectangle sx (sy + (fromIntegral $ sh-w)) sw w
        (sc, rs) = splitVerticallyWD w (n-1) main
