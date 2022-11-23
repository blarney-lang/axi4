{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Blarney.AXI4.Types (
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
     (KnownNat id_bits, KnownNat addr_bits, KnownNat awuser_bits) =>
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
    fromPorts ifc \awvalid awready awid awaddr awlen awsize
                   awburst awlock awcache awprot awqos
                   awregion awuser ->
      Source {
        canPeek = awvalid
      , consume = awready
      , peek = AXI4_AWFlit {..}
      }

-- | Flatten 'Sink's of 'AWFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
     (KnownNat id_bits, KnownNat addr_bits, KnownNat awuser_bits) =>
  => Interface (Sink (AXI4_AWFlit id_bits addr_bits awuser_bits)) where

  toIfc snk = toPorts
    (portName "awready", snk.canPut)
    (portMethodAlwaysEn "" ["awid", "awaddr", "awlen", "awsize",
                            "awburst", "awlock", "awcache", "awprot",
                            "awqos" "awregion" "awuser"],
       \awid awaddr awlen awsize awburst awlock
        awcache awprot awqos awregion awuser ->
          snk.put (AXI4_AWFlit {..}))

  fromIfc ifc =
    fromPorts ifc \awready bigPut ->
      Sink {
        canPut = awready
      , put f = bigPut f.awid f.awaddr f.awlen f.awsize f.awburst f.awlock
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
  Interface (Source (AXI4_WFlit data_bytes wuser_bits)) where
  toIfc src = toPorts ("wvalid", src.canPeek)
                      ("wready", when src.canPeek do src.consume)
                      ("wdata", src.peek.wdata)
                      ("wstrb", src.peek.wstrb)
                      ("wlast", src.peek.wlast)
                      ("wuser", src.peek.wuser)
  fromIfc ifc = fromPorts ifc \wvalid wready wdata wstrb wlast wuser ->
                  Source {
                    canPeek = wvalid
                  , consume = wready
                  , peek = AXI4_WFlit {..}
                  }

-- | Flatten 'Sink's of 'WFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  Interface (Sink (AXI4_WFlit data_bytes wuser_bits)) where
  toIfc snk = error "TODO"
  fromIfc ifc = error "TODO"

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
  Interface (Source (AXI4_BFlit id_bits buser_bits)) where
  toIfc src = toPorts ("bvalid", src.canPeek)
                      ("bready", when src.canPeek do src.consume)
                      ("bid", src.peek.bid)
                      ("bresp", src.peek.bresp)
                      ("buser", src.peek.buser)
  fromIfc ifc = fromPorts ifc \bvalid bready bid bresp buser ->
                  Source {
                    canPeek = bvalid
                  , consume = bready
                  , peek = AXI4_BFlit {..}
                  }

-- | Flatten 'Sink's of 'BFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  Interface (Sink (AXI4_BFlit id_bits buser_bits)) where
  toIfc snk = error "TODO"
  fromIfc ifc = error "TODO"

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
  Interface (Source (AXI4_ARFlit id_bits addr_bits aruser_bits)) where
  toIfc src = toPorts ("arvalid", src.canPeek)
                      ("arready", when src.canPeek do src.consume)
                      ("arid", src.peek.arid)
                      ("araddr", src.peek.araddr)
                      ("arlen", src.peek.arlen)
                      ("arsize", src.peek.arsize)
                      ("arburst", src.peek.arburst)
                      ("arlock", src.peek.arlock)
                      ("arcache", src.peek.arcache)
                      ("arprot", src.peek.arprot)
                      ("arqos", src.peek.arqos)
                      ("arregion", src.peek.arregion)
                      ("aruser", src.peek.aruser)
  fromIfc ifc = fromPorts ifc \arvalid arready arid araddr arlen arsize
                               arburst arlock arcache arprot arqos
                               arregion aruser ->
                  Source {
                    canPeek = arvalid
                  , consume = arready
                  , peek = AXI4_ARFlit {..}
                  }

-- | Flatten 'Sink's of 'ARFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  Interface (Sink (AXI4_ARFlit id_bits addr_bits aruser_bits)) where
  toIfc snk = error "TODO"
  fromIfc ifc = error "TODO"

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
  Interface (Source (AXI4_RFlit id_bits data_bytes ruser_bits)) where
  toIfc src = toPorts ("rvalid", src.canPeek)
                      ("rready", when src.canPeek do src.consume)
                      ("rid", src.peek.rid)
                      ("rdata", src.peek.rdata)
                      ("rresp", src.peek.rresp)
                      ("rlast", src.peek.rlast)
                      ("ruser", src.peek.ruser)
  fromIfc ifc = fromPorts ifc \rvalid rready rid rdata rresp rlast ruser ->
                  Source {
                    canPeek = rvalid
                  , consume = rready
                  , peek = AXI4_RFlit {..}
                  }

-- | Flatten 'Sink's of 'RFlit's for AXI4 compliant interface
instance {-# OVERLAPPING #-}
  Interface (Sink (AXI4_RFlit id_bits data_bytes ruser_bits)) where
  toIfc snk = error "TODO"
  fromIfc ifc = error "TODO"

-- AXI4 Manager / Subordinates
--------------------------------------------------------------------------------

-- | AXI4 parameter container
data AXI4_Params -- | number of bits for the AXI4 ID fiels
                 id_bits
                 -- | number of bits for the AXI4 address
                 addr_bits
                 -- | number of *bytes* for the AXI4 data
                 data_bytes
                 -- | number of buts for the AW user field
                 awuser_bits
                 -- | number of buts for the W user field
                 wuser_bits
                 -- | number of buts for the B user field
                 buser_bits
                 -- | number of buts for the AR user field
                 aruser_bits
                 -- | number of buts for the R user field
                 ruser_bits

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
