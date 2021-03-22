{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ResizeableBorders (BorderMessage (..), borderWithWidth, colorized, ColorMessage (..)) where
import XMonad
import XMonad.Layout.LayoutModifier
import Data.Set (toList)

-- increase or decrease border thickness
data BorderMessage = IncBorder | DecBorder deriving (Read, Show, Typeable)
instance Message BorderMessage

-- the state of the WideBorder modifier
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

borderWithWidth :: Dimension -> l a -> ModifiedLayout WideBorder l a
borderWithWidth = ModifiedLayout . WideBorder

-- let's try recoloring them?
data ColorMessage = SetFocusedColor Pixel | SetNormalColor Pixel | ResetColor
    deriving (Read, Show, Typeable)
instance Message ColorMessage

data ColorBorder a = ColorBorder Pixel Pixel deriving (Read, Show)
instance LayoutModifier ColorBorder Window where

  redoLayout (ColorBorder nbc fbc) _ _ wrs = do
    mapM_ (setBorderColor nbc) (fst <$> wrs)
    withFocused (setBorderColor fbc)
    return (wrs, Just $ ColorBorder nbc fbc)

  handleMess (ColorBorder n f) m
     | Just _ :: Maybe Event     <- fromMessage m = withFocused (setBorderColor f) >> pure Nothing
     | Just (SetFocusedColor f') <- fromMessage m = pure (Just $ ColorBorder n f')
     | Just (SetNormalColor n')  <- fromMessage m = pure (Just $ ColorBorder n' f)
  handleMess _ _ = pure Nothing

setBorderColor :: Pixel -> Window -> X ()
setBorderColor c w = withDisplay $ \d -> io $ setWindowBorder d w c

colorized :: l a -> ModifiedLayout ColorBorder l a
colorized = ModifiedLayout (ColorBorder 0 0)
