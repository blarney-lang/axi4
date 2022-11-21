{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Blarney.AXI4.BasicUtils (
) where

import Blarney

-- | AXI4 buffer shim

mkAXI4BufferShim_Core ::
  -- AXI4 parameters
  forall (params :: AXI4_Params id_bits addr_bits data_bytes
                                awuser_bits wuser_bits buser_bits
                                aruser_bits ruser_bits).
  -- type aliases and constraints
  ( awflit ~ AXI4_AWFlit id_bits addr_bits awuser_bits
  , wflit ~ AXI4_WFlit data_bytes wuser_bits
  , bflit ~ AXI4_BFlit id_bits buser_bits
  , arflit ~ AXI4_ARFlit id_bits addr_bits aruser_bits
  , rflit ~ AXI4_RFlit id_bits data_bytes ruser_bits
  , ToSource aw_buff awflit, ToSink aw_buff awflit
  , ToSource w_buff wflit, ToSink w_buff wflit
  , ToSource b_buff bflit, ToSink b_buff bflit
  , ToSource ar_buff arflit, ToSink ar_buff arflit
  , ToSource r_buff rflit, ToSink r_buff rflit )
  -- argument types and return type
  => Module (aw_buff awflit)
  -> Module (w_buff wflit)
  -> Module (b_buff bflit)
  -> Module (ar_buff arflit)
  -> Module (r_buff rflit)
  -> Module (AXI4_Shim params params)
mkAXI4BufferShim_Core mkAWBuff mkWBuff mkBBuff mkARBuff mkRBuff = do
  let awbuff <- mkAWBuff
  let  wbuff <- mkWBuff
  let  bbuff <- mkBBuff
  let arbuff <- mkARBuff
  let  rbuff <- mkRBuff
  return AXI4_Shim { manager = AXI4_Manager { aw = toSource awbuff
                                            ,  w = toSource wbuff
                                            ,  b = toSink bbuff
                                            , ar = toSource arbuff
                                            ,  r = toSink rbuff }
                   , subordinate = AXI4_Subordinate { aw = toSink awbuff
                                                    ,  w = toSink wbuff
                                                    ,  b = toSource bbuff
                                                    , ar = toSink arbuff
                                                    ,  r = toSource rbuff } }

mkAXI4BufferShimFF :: Module (AXI4_Shim params params)
mkAXI4BufferShimFF = mkAXI4BufferShim_Core mkFIFO mkFIFO mkFIFO mkFIFO mkFIFO
