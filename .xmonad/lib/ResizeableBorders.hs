{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
module ResizeableBorders (BorderMessage (..), borderWithWidth) where
import XMonad
import XMonad.Layout.LayoutModifier
import Data.Set (toList)

-- increase or decrease border thickness
data BorderMessage = IncBorder | DecBorder deriving (Read, Show, Typeable)
instance Message BorderMessage

data WideBorder a = WideBorder Dimension deriving (Read, Show)
instance LayoutModifier WideBorder Window where
  unhook (WideBorder _) = do
    width <- asks (borderWidth . config)
    ws <- toList <$> gets mapped
    setBorders width ws

  redoLayout (WideBorder n) _ _ wrs = do
    setBorders n ws
    return (wrs, Just $ WideBorder n)
   where
    ws = map fst wrs

  pureMess (WideBorder n) m = doinc <$> fromMessage m
   where
    doinc IncBorder = WideBorder $ n + 1
    doinc DecBorder = WideBorder $ if n == 0 then 0 else n - 1

-- copied wholesale from NoBorders.hs (it doesn't export this fn)
setBorders :: Dimension -> [Window] -> X ()
setBorders bw ws = withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w bw) ws

borderWithWidth = ModifiedLayout . WideBorder
