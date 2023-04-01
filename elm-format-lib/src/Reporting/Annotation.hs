-- This module is copied from the Elm compiler with small changes
-- https://github.com/elm/compiler/blob/94715a520f499591ac6901c8c822bc87cd1af24f/compiler/src/Reporting/Annotation.hs

{-# OPTIONS_GHC -Wall #-}
module Reporting.Annotation
  ( Located(..)
  , Position(..)
  , Region(..)
  , toValue
  , merge
  , at
  , toRegion
  , mergeRegions
  , zero
  , one
  , packRegion
  , unpackRegion
  )
  where


import Control.Monad (liftM2)
import Data.Coapplicative
import Data.Binary (Binary, get, put)
import Data.Word (Word16, Word64)
import Data.String (unwords)
import Data.Bits ((.&.), shiftR, shiftL)



-- LOCATED


data Located a =
  At {-# UNPACK #-} !Word64 a  -- PERF see if unpacking region is helpful
  deriving Eq


instance (Show a) => Show (Located a) where
    showsPrec p (At ann a) = showParen (p > 10) $
        showString $ unwords
            [ show ann
            , showsPrec 99 a ""
            ]

instance Functor Located where
  fmap f (At region a) =
    At region (f a)


instance Foldable Located where
    foldMap f (At _ a) = f a


instance Traversable Located where
    traverse f (At region a) = fmap (At region) $ f a


instance Coapplicative Located where
    extract (At _ x) = x
    {-# INLINE extract #-}


traverse :: (Functor f) => (a -> f b) -> Located a -> f (Located b)
traverse func (At region value) =
  At region <$> func value


toValue :: Located a -> a
toValue (At _ value) =
  value


merge :: Located a -> Located b -> value -> Located value
merge (At r1 _) (At r2 _) value =
  At (packRegion $ mergeRegions (unpackRegion r1) (unpackRegion r2)) value



-- POSITION


data Position =
  Position
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word16
  deriving Eq


at :: Position -> Position -> a -> Located a
at start end a =
  At (packRegion $ Region start end) a



-- REGION


data Region = Region Position Position
  deriving Eq


instance Show Region where
    showsPrec p (Region (Position r1 c1) (Position r2 c2)) = showParen (p > 10) $
        showString $ unwords
            [ "at"
            , show r1
            , show c1
            , show r2
            , show c2
            ]


toRegion :: Located a -> Region
toRegion (At region _) =
  unpackRegion region


packRegion :: Region -> Word64
packRegion (Region (Position r1 c1) (Position r2 c2)) =
    ((fromIntegral r1) `shiftL` (16 * 0)) +
    ((fromIntegral c1) `shiftL` (16 * 1)) +
    ((fromIntegral r2) `shiftL` (16 * 2)) +
    ((fromIntegral c2) `shiftL` (16 * 3))


unpackRegion :: Word64 -> Region
unpackRegion packed =
    let
        row1 = fromIntegral ((packed `shiftR` (16 * 3)) .&. 0xFF)
        col1 = fromIntegral ((packed `shiftR` (16 * 2)) .&. 0xFF)
        row2 = fromIntegral ((packed `shiftR` (16 * 1)) .&. 0xFF)
        col2 = fromIntegral ((packed `shiftR` (16 * 0)) .&. 0xFF)
    in
        Region (Position row1 col1) (Position row2 col2)


mergeRegions :: Region -> Region -> Region
mergeRegions (Region start _) (Region _ end) =
  Region start end


zero :: Region
zero =
  Region (Position 0 0) (Position 0 0)


one :: Region
one =
  Region (Position 1 1) (Position 1 1)


instance Binary Region where
  put (Region a b) = put a >> put b
  get = liftM2 Region get get


instance Binary Position where
  put (Position a b) = put a >> put b
  get = liftM2 Position get get

