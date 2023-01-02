{-# LANGUAGE UndecidableInstances #-}

module Blarney.AXI4.Flits (
  AXI4_AWFlit (..)
, AXI4_WFlit (..)
, AXI4_BFlit (..)
, AXI4_ARFlit (..)
, AXI4_RFlit (..)
) where

import Blarney
import Blarney.Vector
import Blarney.SourceSink
import Blarney.AXI4.Fields

-- AXI4 flit types
------------------

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

-- | AXI4 "W" write data channel
data AXI4_WFlit data_bytes wuser_bits =
  AXI4_WFlit {
    wdata :: Vec data_bytes (Bit 8)
  , wstrb :: Bit data_bytes
  , wlast :: Bit 1
  , wuser :: Bit wuser_bits
  } deriving (Generic, Bits)

-- | AXI4 "B" write response channel
data AXI4_BFlit id_bits buser_bits =
  AXI4_BFlit {
    bid   :: Bit id_bits
  , bresp :: AXI4_Resp
  , buser :: Bit buser_bits
  } deriving (Generic, Bits)

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

-- | AXI4 "R" read response channel
data AXI4_RFlit id_bits data_bytes ruser_bits =
  AXI4_RFlit {
    rid   :: Bit id_bits
  , rdata :: Vec data_bytes (Bit 8)
  , rresp :: AXI4_Resp
  , rlast :: Bit 1
  , ruser :: Bit ruser_bits
  } deriving (Generic, Bits)

-- Interface instances for sources/sinks of AXI flits
-----------------------------------------------------

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

-- | Flatten 'Source's of 'WFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  (KnownNat data_bytes, KnownNat (data_bytes*8), KnownNat wuser_bits)
  => Interface (Source (AXI4_WFlit data_bytes wuser_bits)) where

  toIfc src = toPorts
    (portName "wvalid",           src.canPeek)
    (portMethodEn "wready" "" [], when src.canPeek do src.consume)
    (portName "wdata",            pack src.peek.wdata)
    (portName "wstrb",            src.peek.wstrb)
    (portName "wlast",            src.peek.wlast)
    (portName "wuser",            src.peek.wuser)

  fromIfc ifc =
    fromPorts ifc \canPeekVal consumeAct wdata wstrb wlast wuser ->
      Source {
        canPeek = canPeekVal
      , consume = consumeAct
      , peek = AXI4_WFlit { wdata = unpack wdata, .. }
      }

-- | Flatten 'Sink's of 'WFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  (KnownNat data_bytes, KnownNat (data_bytes*8), KnownNat wuser_bits)
  => Interface (Sink (AXI4_WFlit data_bytes wuser_bits)) where

  toIfc snk = toPorts
    (portName "wready", snk.canPut)
    ( portMethodEn "" "wvalid" [ "wdata", "wstrb", "wlast", "wuser" ]
    , \wdata wstrb wlast wuser ->
        when snk.canPut do snk.put AXI4_WFlit{ wdata = unpack wdata, ..} )

  fromIfc ifc =
    fromPorts ifc \canPutVal putAct ->
      Sink {
        canPut = canPutVal
      , put = \f -> putAct (pack f.wdata) f.wstrb f.wlast f.wuser
      }

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
