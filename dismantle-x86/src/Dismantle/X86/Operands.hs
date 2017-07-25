{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
module Dismantle.X86.Operands (
  GPR,
  ControlReg,
  DebugReg,
  MMXR,
  XMMR,
  YMMR,
  ZMMR,
  Displacement,
  Segment,
  MemRef(..),
  disp8,
  disp32,
  noDisplacement,
  -- * Register definitions
  pattern RAX,
  pattern EAX,
  -- * Segments
  pattern ES,
  pattern CS,
  pattern SS,
  pattern DS,
  pattern FS,
  pattern GS
  ) where

import GHC.TypeLits ( Nat )

import Data.Int ( Int8, Int32 )
import Data.Word ( Word8, Word64 )

newtype GPR (w :: Nat) = GPR Word8
  deriving (Eq, Ord)

newtype Displacement (w :: Nat) = Displacement Int32
  deriving (Eq, Ord)

disp8 :: Int8 -> Displacement 8
disp8 = Displacement . fromIntegral

disp32 :: Int32 -> Displacement 32
disp32 = Displacement

noDisplacement :: Displacement 0
noDisplacement = Displacement 0

newtype Segment = Segment Word8
  deriving (Eq, Ord)

pattern ES :: Segment
pattern ES = Segment 0

pattern CS :: Segment
pattern CS = Segment 1

pattern SS :: Segment
pattern SS = Segment 2

pattern DS :: Segment
pattern DS = Segment 3

pattern FS :: Segment
pattern FS = Segment 4

pattern GS :: Segment
pattern GS = Segment 5

data MemRef (rw :: Nat) (dw :: Nat) = IPRelative Segment (Displacement dw)
                                    | MemRef Segment (Maybe (GPR rw)) (Maybe (Int, GPR rw)) (Displacement dw)
                                    | Offset Segment Word64

pattern RAX :: GPR 64
pattern RAX = GPR 0

pattern EAX :: GPR 32
pattern EAX = GPR 0

newtype ControlReg = CR Word8
  deriving (Eq, Ord)

newtype DebugReg = DR Word8
  deriving (Eq, Ord)

newtype MMXR = MMXR Word8
  deriving (Eq, Ord)

newtype XMMR = XMMR Word8
  deriving (Eq, Ord)

newtype YMMR = YMMR Word8
  deriving (Eq, Ord)

newtype ZMMR = ZMMR Word8
  deriving (Eq, Ord)
