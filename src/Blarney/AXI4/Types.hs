{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Blarney.AXI4.Types (
  AXI4_AWFlit (..)
, AXI4_WFlit (..)
, AXI4_BFlit (..)
, AXI4_ARFlit (..)
, AXI4_RFlit (..)
, AXI4_Params (..)
, AXI4_Manager (..)
, AXI4_Subordinate (..)
, AXI4_Shim (..)
, IdBits
, AddrBits
, DataBytes
, AWUserBits
, WUserBits
, BUserBits
, ARUserBits
, RUserBits
, KnownNat_AXI4_Params
) where

import Blarney
import Blarney.Vector
import Blarney.SourceSink
import Blarney.AXI4TypesCommon

-- AW channel (write request address)
--------------------------------------------------------------------------------

-- | AXI4 "AW" write address channel
data AXI4_AWFlit id_bits addr_bits awuser_bits =
  AXI4_AWFlit {
    awid     :: Bit id_bits
  , awaddr   :: Bit addr_bits
  , awlen    :: AXI4_Len
  , awsize   :: AXI4_Size
  , awburst  :: AXI4_Burst
  , awlock   :: AXI4_Lock
  , awcache  :: AXI4_Cache
  , awprot   :: AXI4_Prot
  , awqos    :: AXI4_QoS
  , awregion :: AXI4_Region
  , awuser   :: Bit awuser_bits
  } deriving (Generic, Bits)

-- | Flatten 'Source's of 'AWFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  (KnownNat id_bits, KnownNat addr_bits, KnownNat awuser_bits)
  => Interface (Source (AXI4_AWFlit id_bits addr_bits awuser_bits)) where

  toIfc src = toPorts
    (portName "awvalid",           src.canPeek)
    (portMethodEn "awready" "" [], when src.canPeek do src.consume)
    (portName "awid",              src.peek.awid)
    (portName "awaddr",            src.peek.awaddr)
    (portName "awlen",             src.peek.awlen)
    (portName "awsize",            src.peek.awsize)
    (portName "awburst",           src.peek.awburst)
    (portName "awlock",            src.peek.awlock)
    (portName "awcache",           src.peek.awcache)
    (portName "awprot",            src.peek.awprot)
    (portName "awqos",             src.peek.awqos)
    (portName "awregion",          src.peek.awregion)
    (portName "awuser",            src.peek.awuser)

  fromIfc ifc =
    fromPorts ifc \canPeekVal consumeAct
                   awid awaddr awlen awsize awburst awlock
                   awcache awprot awqos awregion awuser ->
      Source {
        canPeek = canPeekVal
      , consume = consumeAct
      , peek = AXI4_AWFlit {..}
      }

-- | Flatten 'Sink's of 'AWFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  (KnownNat id_bits, KnownNat addr_bits, KnownNat awuser_bits)
  => Interface (Sink (AXI4_AWFlit id_bits addr_bits awuser_bits)) where

  toIfc snk = toPorts
    (portName "awready", snk.canPut)
    ( portMethodEn "" "awvalid"
        [ "awid", "awaddr", "awlen", "awsize", "awburst", "awlock"
        , "awcache", "awprot", "awqos", "awregion", "awuser" ]
    , \awid awaddr awlen awsize awburst awlock
       awcache awprot awqos awregion awuser ->
         when snk.canPut do snk.put AXI4_AWFlit{..} )

  fromIfc ifc =
    fromPorts ifc \canPutVal putAct ->
      Sink {
        canPut = canPutVal
      , put = \f -> putAct f.awid f.awaddr f.awlen f.awsize f.awburst f.awlock
                           f.awcache f.awprot f.awqos f.awregion f.awuser
      }

-- W channel (write request data)
--------------------------------------------------------------------------------

-- | AXI4 "W" write data channel
data AXI4_WFlit data_bytes wuser_bits =
  AXI4_WFlit {
    wdata :: Vec data_bytes (Bit 8)
  , wstrb :: Bit data_bytes
  , wlast :: Bit 1
  , wuser :: Bit wuser_bits
  } deriving (Generic, Bits)

-- | Flatten 'Source's of 'WFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  (KnownNat data_bytes, KnownNat wuser_bits)
  => Interface (Source (AXI4_WFlit data_bytes wuser_bits)) where

  toIfc src = toPorts
    (portName "wvalid",           src.canPeek)
    (portMethodEn "wready" "" [], when src.canPeek do src.consume)
    (portName "wdata",            src.peek.wdata)
    (portName "wstrb",            src.peek.wstrb)
    (portName "wlast",            src.peek.wlast)
    (portName "wuser",            src.peek.wuser)

  fromIfc ifc =
    fromPorts ifc \canPeekVal consumeAct wdata wstrb wlast wuser ->
      Source {
        canPeek = canPeekVal
      , consume = consumeAct
      , peek = AXI4_WFlit {..}
      }

-- | Flatten 'Sink's of 'WFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  (KnownNat data_bytes, KnownNat wuser_bits)
  => Interface (Sink (AXI4_WFlit data_bytes wuser_bits)) where

  toIfc snk = toPorts
    (portName "wready", snk.canPut)
    ( portMethodEn "" "wvalid" [ "wdata", "wstrb", "wlast", "wuser" ]
    , \wdata wstrb wlast wuser -> when snk.canPut do snk.put AXI4_WFlit{..} )

  fromIfc ifc =
    fromPorts ifc \canPutVal putAct ->
      Sink {
        canPut = canPutVal
      , put = \f -> putAct f.wdata f.wstrb f.wlast f.wuser
      }

-- B channel (write response)
--------------------------------------------------------------------------------

-- | AXI4 "B" write response channel
data AXI4_BFlit id_bits buser_bits =
  AXI4_BFlit {
    bid   :: Bit id_bits
  , bresp :: AXI4_Resp
  , buser :: Bit buser_bits
  } deriving (Generic, Bits)

-- | Flatten 'Source's of 'BFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  (KnownNat id_bits, KnownNat buser_bits)
  => Interface (Source (AXI4_BFlit id_bits buser_bits)) where

  toIfc src = toPorts
    (portName "bvalid",           src.canPeek)
    (portMethodEn "bready" "" [], when src.canPeek do src.consume)
    (portName "bid",              src.peek.bid)
    (portName "bresp",            src.peek.bresp)
    (portName "buser",            src.peek.buser)

  fromIfc ifc =
    fromPorts ifc \canPeekVal consumeAct bid bresp buser ->
      Source {
        canPeek = canPeekVal
      , consume = consumeAct
      , peek = AXI4_BFlit {..}
      }

-- | Flatten 'Sink's of 'BFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  (KnownNat id_bits, KnownNat buser_bits)
  => Interface (Sink (AXI4_BFlit id_bits buser_bits)) where

  toIfc snk = toPorts
    (portName "bready", snk.canPut)
    ( portMethodEn "" "bvalid" [ "bid", "bresp", "buser" ]
    , \bid bresp buser -> when snk.canPut do snk.put AXI4_BFlit{..} )

  fromIfc ifc =
    fromPorts ifc \canPutVal putAct ->
      Sink {
        canPut = canPutVal
      , put = \f -> putAct f.bid f.bresp f.buser
      }

-- AR channel (read request address)
--------------------------------------------------------------------------------

-- | AXI4 "AR" read address channel
data AXI4_ARFlit id_bits addr_bits aruser_bits =
  AXI4_ARFlit {
    arid     :: Bit id_bits
  , araddr   :: Bit addr_bits
  , arlen    :: AXI4_Len
  , arsize   :: AXI4_Size
  , arburst  :: AXI4_Burst
  , arlock   :: AXI4_Lock
  , arcache  :: AXI4_Cache
  , arprot   :: AXI4_Prot
  , arqos    :: AXI4_QoS
  , arregion :: AXI4_Region
  , aruser   :: Bit aruser_bits
  } deriving (Generic, Bits)

-- | Flatten 'Source's of 'ARFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  (KnownNat id_bits, KnownNat addr_bits, KnownNat aruser_bits)
  => Interface (Source (AXI4_ARFlit id_bits addr_bits aruser_bits)) where

  toIfc src = toPorts
    (portName "arvalid",           src.canPeek)
    (portMethodEn "arready" "" [], when src.canPeek do src.consume)
    (portName "arid",              src.peek.arid)
    (portName "araddr",            src.peek.araddr)
    (portName "arlen",             src.peek.arlen)
    (portName "arsize",            src.peek.arsize)
    (portName "arburst",           src.peek.arburst)
    (portName "arlock",            src.peek.arlock)
    (portName "arcache",           src.peek.arcache)
    (portName "arprot",            src.peek.arprot)
    (portName "arqos",             src.peek.arqos)
    (portName "arregion",          src.peek.arregion)
    (portName "aruser",            src.peek.aruser)

  fromIfc ifc =
    fromPorts ifc \canPeekVal consumeAct
                   arid araddr arlen arsize arburst arlock
                   arcache arprot arqos arregion aruser ->
      Source {
        canPeek = canPeekVal
      , consume = consumeAct
      , peek = AXI4_ARFlit {..}
      }

-- | Flatten 'Sink's of 'ARFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  (KnownNat id_bits, KnownNat addr_bits, KnownNat aruser_bits)
  => Interface (Sink (AXI4_ARFlit id_bits addr_bits aruser_bits)) where

  toIfc snk = toPorts
    (portName "arready", snk.canPut)
    ( portMethodEn "" "arvalid"
        [ "arid", "araddr", "arlen", "arsize", "arburst", "arlock"
        , "arcache", "arprot", "arqos", "arregion", "aruser" ]
    , \arid araddr arlen arsize arburst arlock
       arcache arprot arqos arregion aruser ->
         when snk.canPut do snk.put AXI4_ARFlit{..} )

  fromIfc ifc =
    fromPorts ifc \canPutVal putAct ->
      Sink {
        canPut = canPutVal
      , put = \f -> putAct f.arid f.araddr f.arlen f.arsize f.arburst f.arlock
                           f.arcache f.arprot f.arqos f.arregion f.aruser
      }

-- R channel (read response)
--------------------------------------------------------------------------------

-- | AXI4 "R" read response channel
data AXI4_RFlit id_bits data_bytes ruser_bits =
  AXI4_RFlit {
    rid   :: Bit id_bits
  , rdata :: Vec data_bytes (Bit 8)
  , rresp :: AXI4_Resp
  , rlast :: Bit 1
  , ruser :: Bit ruser_bits
  } deriving (Generic, Bits)

-- | Flatten 'Source's of 'RFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  (KnownNat id_bits, KnownNat data_bytes, KnownNat ruser_bits)
  => Interface (Source (AXI4_RFlit id_bits data_bytes ruser_bits)) where

  toIfc src = toPorts
    (portName "rvalid",           src.canPeek)
    (portMethodEn "rready" "" [], when src.canPeek do src.consume)
    (portName "rid",              src.peek.rid)
    (portName "rdata",            src.peek.rdata)
    (portName "rresp",            src.peek.rresp)
    (portName "rlast",            src.peek.rlast)
    (portName "ruser",            src.peek.ruser)

  fromIfc ifc =
    fromPorts ifc \canPeekVal consumeAct rid rdata rresp rlast ruser ->
      Source {
        canPeek = canPeekVal
      , consume = consumeAct
      , peek = AXI4_RFlit {..}
      }

-- | Flatten 'Sink's of 'RFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  (KnownNat id_bits, KnownNat data_bytes, KnownNat ruser_bits)
  => Interface (Sink (AXI4_RFlit id_bits data_bytes ruser_bits)) where

  toIfc snk = toPorts
    (portName "rready", snk.canPut)
    ( portMethodEn "" "rvalid" [ "rid", "rdata", "rresp", "rlast", "ruser" ]
    , \rid rdata rresp rlast ruser ->
         when snk.canPut do snk.put AXI4_RFlit{..} )

  fromIfc ifc =
    fromPorts ifc \canPutVal putAct ->
      Sink {
        canPut = canPutVal
      , put = \f -> putAct f.rid f.rdata f.rresp f.rlast f.ruser
      }

-- AXI4 Manager / Subordinates
--------------------------------------------------------------------------------

-- | AXI4 parameter container
data AXI4_Params -- | number of bits for the AXI4 ID fiels
                 (id_bits :: Nat)
                 -- | number of bits for the AXI4 address
                 (addr_bits :: Nat)
                 -- | number of *bytes* for the AXI4 data
                 (data_bytes :: Nat)
                 -- | number of buts for the AW user field
                 (awuser_bits :: Nat)
                 -- | number of buts for the W user field
                 (wuser_bits :: Nat)
                 -- | number of buts for the B user field
                 (buser_bits :: Nat)
                 -- | number of buts for the AR user field
                 (aruser_bits :: Nat)
                 -- | number of buts for the R user field
                 (ruser_bits :: Nat)

-- | Select 'id_bits' field of 'AXI4_Params'
type IdBits (params :: AXI4_Params id a d awu wu bu aru ru) = id

-- | Select 'addr_bits' field of 'AXI4_Params'
type AddrBits (params :: AXI4_Params id a d awu wu bu aru ru) = a

-- | Select 'data_bytes' field of 'AXI4_Params'
type DataBytes (params :: AXI4_Params id a d awu wu bu aru ru) = d

-- | Select 'awuser_bits' field of 'AXI4_Params'
type AWUserBits (params :: AXI4_Params id a d awu wu bu aru ru) = awu

-- | Select 'wuser_bits' field of 'AXI4_Params'
type WUserBits (params :: AXI4_Params id a d awu wu bu aru ru) = wu

-- | Select 'buser_bits' field of 'AXI4_Params'
type BUserBits (params :: AXI4_Params id a d awu wu bu aru ru) = bu

-- | Select 'aruser_bits' field of 'AXI4_Params'
type ARUserBits (params :: AXI4_Params id a d awu wu bu aru ru) = aru

-- | Select 'ruser_bits' field of 'AXI4_Params'
type RUserBits (params :: AXI4_Params id a d awu wu bu aru ru) = ru

-- | Constraint synonym for 'KnownNat' over fields of 'AXI4_Params'
type KnownNat_AXI4_Params (params :: AXI4_Params id a d awu wu bu aru ru) =
  (KnownNat id, KnownNat a, KnownNat d, KnownNat awu,
   KnownNat wu, KnownNat bu, KnownNat aru, KnownNat ru)

-- | AXI4 manager
data AXI4_Manager (params :: AXI4_Params id_bits addr_bits data_bytes
                                         awuser_bits wuser_bits buser_bits
                                         aruser_bits ruser_bits) =
  AXI4_Manager {
    aw :: Source (AXI4_AWFlit id_bits addr_bits awuser_bits)
  , w  :: Source (AXI4_WFlit data_bytes wuser_bits)
  , b  :: Sink   (AXI4_BFlit id_bits buser_bits)
  , ar :: Source (AXI4_ARFlit id_bits addr_bits aruser_bits)
  , r  :: Sink   (AXI4_RFlit id_bits data_bytes ruser_bits)
  } deriving (Generic, Interface)

-- | AXI4 subordinate
data AXI4_Subordinate (params :: AXI4_Params id_bits addr_bits data_bytes
                                             awuser_bits wuser_bits buser_bits
                                             aruser_bits ruser_bits) =
  AXI4_Subordinate {
    aw :: Sink   (AXI4_AWFlit id_bits addr_bits awuser_bits)
  , w  :: Sink   (AXI4_WFlit data_bytes wuser_bits)
  , b  :: Source (AXI4_BFlit id_bits buser_bits)
  , ar :: Sink   (AXI4_ARFlit id_bits addr_bits aruser_bits)
  , r  :: Source (AXI4_RFlit id_bits data_bytes ruser_bits)
  } deriving (Generic, Interface)

-- | AXI4 shim
data AXI4_Shim params_sub params_mgr =
  AXI4_Shim {
    subordinate :: AXI4_Subordinate params_sub
  , manager     :: AXI4_Manager params_mgr
  } deriving (Generic, Interface)
