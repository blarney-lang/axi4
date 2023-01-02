module Blarney.AXI4.Utils.BufferShim (
  makeAXI4BufferShim_Core
, makeAXI4BufferShim
) where

import Blarney
import Blarney.Queue
import Blarney.SourceSink
import Blarney.AXI4.Flits
import Blarney.AXI4.Interfaces

-- | AXI4 buffer shim (core module)
makeAXI4BufferShim_Core ::
  -- type aliases and constraints
  ( awflit ~ AXI4_AWFlit (IdWidth p) (AddrWidth p) (AWUserWidth p)
  , wflit  ~ AXI4_WFlit  (DataWidth p) (WUserWidth p)
  , bflit  ~ AXI4_BFlit  (IdWidth p) (BUserWidth p)
  , arflit ~ AXI4_ARFlit (IdWidth p) (AddrWidth p) (ARUserWidth p)
  , rflit  ~ AXI4_RFlit  (IdWidth p) (DataWidth p) (RUserWidth p)
  , ToSource aw_buff awflit, ToSink aw_buff awflit
  , ToSource w_buff wflit, ToSink w_buff wflit
  , ToSource b_buff bflit, ToSink b_buff bflit
  , ToSource ar_buff arflit, ToSink ar_buff arflit
  , ToSource r_buff rflit, ToSink r_buff rflit )
  -- argument types and return type
  => Module aw_buff -> Module w_buff -> Module b_buff
  -> Module ar_buff -> Module r_buff
  -> Module (AXI4_Shim p p)
makeAXI4BufferShim_Core mkAWBuff mkWBuff mkBBuff mkARBuff mkRBuff = do
  awbuff <- mkAWBuff
  wbuff  <- mkWBuff
  bbuff  <- mkBBuff
  arbuff <- mkARBuff
  rbuff  <- mkRBuff
  return
    AXI4_Shim {
      manager =
        AXI4_Manager {
          aw = toSource awbuff
        , w  = toSource wbuff
        , b  = toSink bbuff
        , ar = toSource arbuff
        , r  = toSink rbuff
        }
    , subordinate =
        AXI4_Subordinate {
          aw = toSink awbuff
        , w  = toSink wbuff
        , b  = toSource bbuff
        , ar = toSink arbuff
        , r  = toSource rbuff
        }
    }

-- | AXI4 buffer shim
makeAXI4BufferShim :: KnownNat_AXI4_Params p =>
     -- | Type of buffer to use
     (forall a. Bits a => Module (Queue a))
     -- | Shim with buffer between each side
  -> Module (AXI4_Shim p p)
makeAXI4BufferShim makeBuffer =
  makeAXI4BufferShim_Core makeBuffer
                          makeBuffer
                          makeBuffer
                          makeBuffer
                          makeBuffer
