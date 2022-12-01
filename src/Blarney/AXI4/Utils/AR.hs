module Blarney.AXI4.Utils.AR (
) where

import Blarney
import Blarney.AXI4.Types

-- map over flit type
--------------------------------------------------------------------------------

mapAXI4_ARFlit_arid :: (Bit id_in -> Bit id_out)
                    -> AXI4_ARFlit  id_in addr_bits aruser_bits
                    -> AXI4_ARFlit id_out addr_bits aruser_bits
mapAXI4_ARFlit_arid f x = x { arid = f x.arid }

mapAXI4_ARFlit_araddr :: (Bit addr_in -> Bit addr_out)
                      -> AXI4_ARFlit id_bits  addr_in aruser_bits
                      -> AXI4_ARFlit id_bits addr_out aruser_bits
mapAXI4_ARFlit_araddr f x = x { araddr = f x.araddr }

mapAXI4_ARFlit_aruser :: (Bit aruser_in -> Bit aruser_out)
                      -> AXI4_ARFlit id_bits addr_bits  aruser_in
                      -> AXI4_ARFlit id_bits addr_bits aruser_out
mapAXI4_ARFlit_aruser f x = x { aruser = f x.aruser }
