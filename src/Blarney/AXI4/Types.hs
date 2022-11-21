{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Blarney.AXI4.Types (
) where

import Blarney
import Blarney.Vector
import Blarney.AXI4TypesCommon

-- | AXI4 "AW" write address channel
data AXI4_AWFlit id_bits addr_bits user_bits = AXI4_AWFlit {
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
  , awuser   :: Bit user_bits
  } deriving (Generic, Bits)

-- | AXI4 "W" write data channel
data AXI4_WFlit data_bytes user_bits = AXI4_WFlit {
    wdata :: Vector data_bytes (Bit 8)
  , wstrb :: Bit data_bytes
  , wlast :: Bit 1
  , wuser :: Bit user_bits
  } deriving (Generic, Bits)

-- | AXI4 "B" write response channel
data AXI4_BFlit id_bits user_bits = AXI4_BFlit {
    bid   :: Bit id_bits
  , bresp :: AXI4_Resp
  , buser :: Bit user_bits
  } deriving (Generic, Bits)

-- | AXI4 "AR" read address channel
data AXI4_ARFlit id_sz addr_sz user_sz = AXI4_ARFlit {
    arid     :: Bit id_sz
  , araddr   :: Bit addr_sz
  , arlen    :: AXI4_Len
  , arsize   :: AXI4_Size
  , arburst  :: AXI4_Burst
  , arlock   :: AXI4_Lock
  , arcache  :: AXI4_Cache
  , arprot   :: AXI4_Prot
  , arqos    :: AXI4_QoS
  , arregion :: AXI4_Region
  , aruser   :: Bit user_sz
  } deriving (Generic, Bits)

-- | AXI4 "R" read response channel
data AXI4_RFlit id_bits data_bytes user_bits = AXI4_RFlit {
    rid   :: Bit id_bits
  , rdata :: Vector data_bytes (Bit 8)
  , rresp :: AXI4_Resp
  , rlast :: Bit 1
  , ruser :: Bit user_bits
  } deriving (Generic, Bits)

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
data AXI4_Manager (AXI4_Params id_bits addr_bits data_bytes
                               awuser_bits wuser_bits buser_bits
                               aruser_bits ruser_bits) = AXI4_Manager {
    aw :: Source (AXI4_AWFlit id_bits addr_bits awuser_bits)
  , w  :: Source (AXI4_WFlit data_bytes wuser_bits)
  , b  :: Sink   (AXI4_BFlit id_bits buser_bits)
  , ar :: Source (AXI4_ARFlit id_bits addr_bits aruser_bits)
  , r  :: Sink   (AXI4_RFlit id_bits data_bytes ruser_bits)
  } deriving (Generic, Bits)

-- | AXI4 subordinate
data AXI4_Subordinate (AXI4_Params id_bits addr_bits data_bytes
                                   awuser_bits wuser_bits buser_bits
                                   aruser_bits ruser_bits) = AXI4_Subordinate {
    aw :: Sink   (AXI4_AWFlit id_bits addr_bits awuser_bits)
  , w  :: Sink   (AXI4_WFlit data_bytes wuser_bits)
  , b  :: Source (AXI4_BFlit id_bits buser_bits)
  , ar :: Sink   (AXI4_ARFlit id_bits addr_bits aruser_bits)
  , r  :: Source (AXI4_RFlit id_bits data_bytes ruser_bits)
  } deriving (Generic, Bits)

-- | AXI4 shim
data AXI4_Shim pSub pMngr = AXI4_Shim {
    subordinate :: AXI4_Subordinate pSub
  , manager     :: AXI4_Manager pMngr
  } deriving (Generic, Bits)
