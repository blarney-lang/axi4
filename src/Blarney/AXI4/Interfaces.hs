{-# LANGUAGE UndecidableInstances #-}

module Blarney.AXI4.Interfaces (
  AXI4_RFlit (..)
, AXI4_Params_Container (..)
, type IdWidth
, type AddrWidth
, type DataWidth
, type AWUserWidth
, type WUserWidth
, type BUserWidth
, type ARUserWidth
, type RUserWidth
, AXI4_Manager (..)
, AXI4_Subordinate (..)
, AXI4_Shim (..)
, KnownNat_AXI4_Params
) where

-- Blarney imports
import Blarney
import Blarney.Vector
import Blarney.SourceSink
import Blarney.Connectable

-- AXI4 imports
import Blarney.AXI4.Flits
import Blarney.AXI4CommonTypes

-- | AXI4 parameter container, primarily for use as a kind rather than a type
data AXI4_Params_Container =
  AXI4_Params
    Nat -- ^ Number of bits for the AXI4 id fields
    Nat -- ^ Number of bits for the AXI4 address
    Nat -- ^ Number of *bytes* for the AXI4 data
    Nat -- ^ Number of bits for the AW user field
    Nat -- ^ Number of bits for the W user field
    Nat -- ^ Number of bits for the B user field
    Nat -- ^ Number of bits for the AR user field
    Nat -- ^ Number of bits for the R user field

-- | Select width (bits) of id from `AXI4_Params`
type family IdWidth params where
  IdWidth (AXI4_Params id a d awu wu bu aru ru) = id

-- | Select width (bits) of address from `AXI4_Params`
type family AddrWidth params where
  AddrWidth (AXI4_Params id a d awu wu bu aru ru) = a

-- | Select width (bytes) of data from `AXI4_Params`
type family DataWidth params where
  DataWidth (AXI4_Params id a d awu wu bu aru ru) = d

-- | Select width (bits) of AW user field from `AXI4_Params`
type family AWUserWidth params where
  AWUserWidth (AXI4_Params id a d awu wu bu aru ru) = awu

-- | Select width (bits) of W user field from `AXI4_Params`
type family WUserWidth params where
  WUserWidth (AXI4_Params id a d awu wu bu aru ru) = wu

-- | Select width (bits) of B user field from `AXI4_Params`
type family BUserWidth params where
  BUserWidth (AXI4_Params id a d awu wu bu aru ru) = bu

-- | Select width (bits) of AR user field from `AXI4_Params`
type family ARUserWidth params where
  ARUserWidth (AXI4_Params id a d awu wu bu aru ru) = aru

-- | Select width (bits) of R user field from `AXI4_Params`
type family RUserWidth params where
  RUserWidth (AXI4_Params id a d awu wu bu aru ru) = ru

-- | Constraint synonym for 'KnownNat' over fields of 'AXI4_Params'
type KnownNat_AXI4_Params (p :: AXI4_Params_Container) =
  ( KnownNat (IdWidth p)
  , KnownNat (AddrWidth p)
  , KnownNat (DataWidth p)
  , KnownNat (DataWidth p * 8)
  , KnownNat (AWUserWidth p)
  , KnownNat (WUserWidth p)
  , KnownNat (BUserWidth p)
  , KnownNat (ARUserWidth p)
  , KnownNat (RUserWidth p)
  )

-- | AXI4 manager
data AXI4_Manager (p :: AXI4_Params_Container) =
  AXI4_Manager {
    aw :: Source (AXI4_AWFlit (IdWidth p) (AddrWidth p) (AWUserWidth p))
  , w  :: Source (AXI4_WFlit (DataWidth p) (WUserWidth p))
  , b  :: Sink   (AXI4_BFlit (IdWidth p) (BUserWidth p))
  , ar :: Source (AXI4_ARFlit (IdWidth p) (AddrWidth p) (ARUserWidth p))
  , r  :: Sink   (AXI4_RFlit (IdWidth p) (DataWidth p) (RUserWidth p))
  } deriving Generic

-- | AXI4 subordinate
data AXI4_Subordinate (p :: AXI4_Params_Container) =
  AXI4_Subordinate {
    aw :: Sink   (AXI4_AWFlit (IdWidth p) (AddrWidth p) (AWUserWidth p))
  , w  :: Sink   (AXI4_WFlit (DataWidth p) (WUserWidth p))
  , b  :: Source (AXI4_BFlit (IdWidth p) (BUserWidth p))
  , ar :: Sink   (AXI4_ARFlit (IdWidth p) (AddrWidth p) (ARUserWidth p))
  , r  :: Source (AXI4_RFlit (IdWidth p) (DataWidth p) (RUserWidth p))
  } deriving Generic

-- | AXI4 shim
data AXI4_Shim ps pm =
  AXI4_Shim {
    subordinate :: AXI4_Subordinate ps
  , manager     :: AXI4_Manager pm
  } deriving Generic

-- Interface instances

-- | 'AXI4_Manager' is an 'Interface'
instance (KnownNat_AXI4_Params p, KnownNat (DataWidth p * 8)) =>
           Interface (AXI4_Manager p) where
  toIfc m = toPorts
    (portSkipName, m.aw)
    (portSkipName, m.w)
    (portSkipName, m.b)
    (portSkipName, m.ar)
    (portSkipName, m.r)

  fromIfc ifc = fromPorts ifc \aw w b ar r -> AXI4_Manager {..}

-- | 'AXI4_Subordinate' is an 'Interface'
instance (KnownNat_AXI4_Params p, KnownNat (DataWidth p * 8)) =>
           Interface (AXI4_Subordinate p) where
  toIfc m = toPorts
    (portSkipName, m.aw)
    (portSkipName, m.w)
    (portSkipName, m.b)
    (portSkipName, m.ar)
    (portSkipName, m.r)

  fromIfc ifc = fromPorts ifc \aw w b ar r -> AXI4_Subordinate {..}

-- | 'AXI4_Shim' is an 'Interface'
instance ( KnownNat_AXI4_Params ps
         , KnownNat_AXI4_Params pm ) => Interface (AXI4_Shim ps pm)

-- Connectable instances

-- Can connect 'AXI4_Manager' to 'AXI4_Subordinate'
instance Connectable (AXI4_Manager p) (AXI4_Subordinate p) where
  makeConnection man sub = do
    makeConnection man.aw sub.aw
    makeConnection man.w  sub.w
    makeConnection sub.b  man.b
    makeConnection man.ar sub.ar
    makeConnection sub.r  man.r
