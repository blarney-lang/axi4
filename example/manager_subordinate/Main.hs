import Blarney
import Blarney.AXI4
import Blarney.SourceSink

--------------------------------------------------------------------------------

mkTestManager :: KnownNat_AXI4_Params params
              => Module (AXI4_Manager params)
mkTestManager = do
  -- declare a shim to implement the interface in a simple manner
  shim <- mkAXI4BufferShimFF
  -- use the subordinate side of the shim internally
  always do
    -- typical write request send
    when (shim.subordinate.aw.canPut .&&. shim.subordinate.w.canPut) do
      shim.subordinate.aw.put dontCare
      shim.subordinate.w.put dontCare
    -- typical write response receive
    when shim.subordinate.b.canPeek do
      shim.subordinate.b.consume
    -- typical read request send
    when shim.subordinate.ar.canPut do
      shim.subordinate.ar.put dontCare
    -- typical read response receive
    when shim.subordinate.r.canPeek do
      shim.subordinate.r.consume
  -- return the manager side of the shim as the external interface
  return shim.manager

--------------------------------------------------------------------------------

mkTestSubordinate :: KnownNat_AXI4_Params params
                  => Module (AXI4_Subordinate params)
mkTestSubordinate = do
  -- declare a shim to implement the interface in a simple manner
  shim <- mkAXI4BufferShimFF
  -- use the manager side of the shim internally
  always do
    -- typical write request receive
    when (shim.manager.aw.canPeek .&&. shim.manager.w.canPeek) do
      shim.manager.aw.consume
      shim.manager.w.consume
    -- typical write response send
    when shim.manager.b.canPut do
      shim.manager.b.put dontCare
    -- typical read request receive
    when shim.manager.ar.canPeek do
      shim.manager.ar.consume
    -- typical read response send
    when shim.manager.r.canPut do
      shim.manager.r.put dontCare
  -- return the subordinate side of the shim as the external interface
  return shim.subordinate

--------------------------------------------------------------------------------

type Test_AXI4_Params = AXI4_Params 2 32 8 0 0 0 0 0

--testBench :: Module ()
testBench =
  makeBoundary "Mgr" (mkTestManager @(_ :: Test_AXI4_Params))
  --mgr <- makeBoundary "Mgr" mkTestManager
  --sub <- makeBoundary "Sub" mkTestSubordinate
  --mkConnection mgr sub

--------------------------------------------------------------------------------

main :: IO ()
main = do
  writeVerilogModule testBench "testBench" "testBench-Verilog/"
