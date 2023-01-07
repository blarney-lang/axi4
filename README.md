# AXI4 library

This is a library for working with the AXI4 protocol in
[Blarney](https://github.com/blarney-lang/blarney).  It supports
development of AXI4 components (managers, subordinates, and stream
processors), interfacing with existing AXI4 components written in
other languages, and connecting AXI4 components together.  *It is in
the early stages of development.*

## Usage

To use the library, add `blarney` and `blarney-axi4` as package
dependenices to your project's `.cabal` file.  Also tell cabal where
to find these packages in your project's `cabal.project` file. See the
[AXI4 memory-mapped register
component](https://github.com/blarney-lang/mmreg) for a complete
example of how to do this.

## Type overview

AXI4 interface parameters shared by managers and subordinates are
captured by the `AXI4_Params` type constructor of kind:

```haskell
AXI4_Params
  :: Nat     -- Id width (bits)
  -> Nat     -- Address width (bits)
  -> Nat     -- Data width (bytes)
  -> Nat     -- AW user width (bits)
  -> Nat     -- B user width (bits)
  -> Nat     -- AR user width (bits)
  -> Nat     -- R user width (bits)
  -> AXI4_Params_Container
```

Individual parameters can be selected from a type of kind
`AXI4_Params_Container` using the following type families.

```haskell
  IdWidth     (AXI4_Params id a d awu wu bu aru ru) = id
  AddrWidth   (AXI4_Params id a d awu wu bu aru ru) = a
  DataWidth   (AXI4_Params id a d awu wu bu aru ru) = d
  AWUserWidth (AXI4_Params id a d awu wu bu aru ru) = awu
  WUserWidth  (AXI4_Params id a d awu wu bu aru ru) = wu
  BUserWidth  (AXI4_Params id a d awu wu bu aru ru) = bu
  ARUserWidth (AXI4_Params id a d awu wu bu aru ru) = aru
  RUserWidth  (AXI4_Params id a d awu wu bu aru ru) = ru
```

AXI4 managers, subordinates, and shims are then defined as collections
of AXI4 channels in various directions:

```haskell
-- | AXI4 manager
data AXI4_Manager (p :: AXI4_Params_Container) =
  AXI4_Manager {
    aw :: Source (AXI4_AWFlit (IdWidth p) (AddrWidth p) (AWUserWidth p))
  , w  :: Source (AXI4_WFlit (DataWidth p) (WUserWidth p))
  , b  :: Sink   (AXI4_BFlit (IdWidth p) (BUserWidth p))
  , ar :: Source (AXI4_ARFlit (IdWidth p) (AddrWidth p) (ARUserWidth p))
  , r  :: Sink   (AXI4_RFlit (IdWidth p) (DataWidth p) (RUserWidth p))
  }

-- | AXI4 subordinate
data AXI4_Subordinate (p :: AXI4_Params_Container) =
  AXI4_Subordinate {
    aw :: Sink   (AXI4_AWFlit (IdWidth p) (AddrWidth p) (AWUserWidth p))
  , w  :: Sink   (AXI4_WFlit (DataWidth p) (WUserWidth p))
  , b  :: Source (AXI4_BFlit (IdWidth p) (BUserWidth p))
  , ar :: Sink   (AXI4_ARFlit (IdWidth p) (AddrWidth p) (ARUserWidth p))
  , r  :: Source (AXI4_RFlit (IdWidth p) (DataWidth p) (RUserWidth p))
  }

-- | AXI4 shim
data AXI4_Shim (ps :: AXI4_Params_Container)
               (pm :: AXI4_Params_Container) =
  AXI4_Shim {
    subordinate :: AXI4_Subordinate ps
  , manager     :: AXI4_Manager pm
  }
```

Finally, the individual flits carried by the channels are defined as:

```haskell
-- | Flits carried by AXI4 "AW" write address channel
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

-- | Flits carried by AXI4 "W" write data channel
data AXI4_WFlit data_bytes wuser_bits =
  AXI4_WFlit {
    wdata :: Vec data_bytes (Bit 8)
  , wstrb :: Bit data_bytes
  , wlast :: Bit 1
  , wuser :: Bit wuser_bits
  } deriving (Generic, Bits)

-- | Flits carried by AXI4 "B" write response channel
data AXI4_BFlit id_bits buser_bits =
  AXI4_BFlit {
    bid   :: Bit id_bits
  , bresp :: AXI4_Resp
  , buser :: Bit buser_bits
  } deriving (Generic, Bits)

-- | Flits carried by AXI4 "AR" read address channel
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

-- | Flits carried by AXI4 "R" read response channel
data AXI4_RFlit id_bits data_bytes ruser_bits =
  AXI4_RFlit {
    rid   :: Bit id_bits
  , rdata :: Vec data_bytes (Bit 8)
  , rresp :: AXI4_Resp
  , rlast :: Bit 1
  , ruser :: Bit ruser_bits
  } deriving (Generic, Bits)
```
