{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Blarney.AXI4.Utils.Basics (
  mkAXI4BufferShimFF
) where

import Blarney
import Blarney.Queue
import Blarney.SourceSink
import Blarney.AXI4.Types

-- | AXI4 buffer shim

mkAXI4BufferShim_Core ::
  -- type aliases and constraints
  ( awflit ~ AXI4_AWFlit id_bits addr_bits awuser_bits
  , wflit ~ AXI4_WFlit data_bytes wuser_bits
  , bflit ~ AXI4_BFlit id_bits buser_bits
  , arflit ~ AXI4_ARFlit id_bits addr_bits aruser_bits
  , rflit ~ AXI4_RFlit id_bits data_bytes ruser_bits
  , params ~ (_dummy :: AXI4_Params id_bits addr_bits data_bytes
                                    awuser_bits wuser_bits buser_bits
                                    aruser_bits ruser_bits)
  , ToSource aw_buff awflit, ToSink aw_buff awflit
  , ToSource w_buff wflit, ToSink w_buff wflit
  , ToSource b_buff bflit, ToSink b_buff bflit
  , ToSource ar_buff arflit, ToSink ar_buff arflit
  , ToSource r_buff rflit, ToSink r_buff rflit )
  -- argument types and return type
  => Module aw_buff -> Module w_buff -> Module b_buff
  -> Module ar_buff -> Module r_buff
  -> Module (AXI4_Shim params params)
mkAXI4BufferShim_Core mkAWBuff mkWBuff mkBBuff mkARBuff mkRBuff = do
  awbuff <- mkAWBuff
  wbuff  <- mkWBuff
  bbuff  <- mkBBuff
  arbuff <- mkARBuff
  rbuff  <- mkRBuff
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

mkAXI4BufferShimFF :: _ => Module (AXI4_Shim params params)
mkAXI4BufferShimFF = mkAXI4BufferShim_Core makeQueue
                                           makeQueue
                                           makeQueue
                                           makeQueue
                                           makeQueue
