module Blarney.AXI4.Tests.PIO (test) where

-- Blarney imports
import Blarney
import Blarney.Stmt
import Blarney.Queue
import Blarney.Vector qualified as V
import Blarney.SourceSink
import Blarney.Connectable

-- AXI4 imports
import Blarney.AXI4
import Blarney.AXI4.Utils.BufferShim

-- A PIO (parallel I/O) is a memory-mapped register of a desired width
type PIO addr_bits data_bytes =
  AXI4_Subordinate (AXI4_Params 0 addr_bits data_bytes 0 0 0 0 0)

makePIO :: _ => Module (PIO addr_bits data_bytes)
makePIO = do
  -- Declare a shim to implement the interface in a simple manner
  shim <- makeAXI4BufferShim (makePipelineQueue 1)

  -- Data registers
  dataRegs :: V.Vec (2^addr_bits) (Reg (Bit 8)) <-
    V.replicateM (makeReg dontCare)

  -- Read-related registers
  arid    <- makeReg dontCare
  araddr  <- makeReg dontCare
  arsize  <- makeReg dontCare
  arcount <- makeReg dontCare

  -- Write-related registers
  awid   <- makeReg dontCare
  awaddr <- makeReg dontCare
  awsize <- makeReg dontCare

  -- Read/write in progress?
  read  <- makeReg false
  write <- makeReg false

  always do
    -- Receive write request
    when (inv write.val .&&. shim.manager.aw.canPeek) do
      let awflit = shim.manager.aw.peek
      awid <== awflit.awid
      awaddr <== awflit.awaddr
      awsize <== (1 :: Bit addr_bits) .<<. awflit.awsize
      shim.manager.aw.consume
      write <== true

    -- Receive write data
    when (write.val .&&. shim.manager.w.canPeek) do
      let wflit = shim.manager.w.peek
      when shim.manager.b.canPut do
        shim.manager.w.consume
        -- Issue write response at end of burst
        when wflit.wlast do
          write <== false
          shim.manager.b.put
            AXI4_BFlit {
              bid = awid.val
            , bresp = resp_okay
            , buser = dontCare
            }
        -- Update data registers
        sequence_
          [ when (fromInteger i .>=. awaddr.val .&&.
                  fromInteger i .<. awaddr.val + awsize.val) do
              let j = fromInteger i - awaddr.val
              when (wflit.wstrb ! j) do
                r <== (wflit.wdata ! j)
          | (r, i) <- zip (V.toList dataRegs) [0..] ]
        -- Increment address
        awaddr <== awaddr.val + awsize.val

    -- Receive read request
    when (inv read.val .&&. shim.manager.ar.canPeek) do
      let arflit = shim.manager.ar.peek
      arcount <== arflit.arlen
      arsize <== (1 :: Bit addr_bits) .<<. arflit.arsize
      araddr <== arflit.araddr
      arid <== arflit.arid
      shim.manager.ar.consume
      read <== true

    -- Issue read response
    when (read.val .&&. shim.manager.r.canPut) do
      -- Respond with requested bytes
      let getByte i = (dataRegs ! (araddr.val + fromIntegral i)).val
      shim.manager.r.put
        AXI4_RFlit {
          rid = arid.val
        , rdata = V.map getByte V.genVec
        , rresp = resp_okay
        , rlast = arcount.val .==. 0
        , ruser = dontCare
        }
      araddr <== araddr.val + arsize.val
      arcount <== arcount.val - 1
      -- Finished reading?
      when (arcount.val .==. 0) do
        read <== false

  -- Return the subordinate side of the shim as the external interface
  return shim.subordinate

makePIOTestBench :: Module ()
makePIOTestBench = do
  -- 64 bit PIO with 32-bit data bus
  pio :: PIO 3 4 <- makePIO

  runStmt do
    -- Issue write request for 6 bytes starting at address 1
    wait pio.aw.canPut 
    action do
      pio.aw.put
        AXI4_AWFlit {
          awid     = dontCare
        , awaddr   = 1
        , awlen    = 2
        , awsize   = 1
        , awburst  = dontCare
        , awlock   = dontCare
        , awcache  = dontCare
        , awprot   = dontCare
        , awqos    = dontCare
        , awregion = dontCare
        , awuser   = dontCare
        } 

    -- Write 6 bytes
    forM_ [0..2] \i -> do
      wait pio.w.canPut
      action do
        pio.w.put
          AXI4_WFlit {
            wdata = V.fromList (map fromInteger [2*i, 2*i+1, 0, 0])
          , wstrb = fromBitList [1, 1, 0, 0]
          , wlast = if i == 2 then true else false
          , wuser = dontCare
          }

    -- Wait for write response
    wait pio.b.canPeek
    action do
      pio.b.consume

    -- Issue read request for 4 bytes starting at address 3
    wait pio.ar.canPut
    action do
      pio.ar.put
        AXI4_ARFlit {
          arid     = dontCare
        , araddr   = 3
        , arlen    = 3
        , arsize   = 0
        , arburst  = dontCare
        , arlock   = dontCare
        , arcache  = dontCare
        , arprot   = dontCare
        , arqos    = dontCare
        , arregion = dontCare
        , aruser   = dontCare
        }

    -- Read 4 bytes
    forM_ [0..3] \i -> do
      wait pio.r.canPeek
      action do
        display (V.head pio.r.peek.rdata)
        pio.r.consume

    action do
      finish
 
-- Cabal test
test :: IO ()
test = simulate makePIOTestBench
