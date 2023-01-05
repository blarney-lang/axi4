module Blarney.AXI4CommonTypes (
  AXI4_Len
, AXI4_Size
, AXI4_Burst, burst_fixed, burst_incr, burst_wrap, burst_reserved
, AXI4_Lock, lock_normal, lock_exclusive
, AXI4_Cache
, arcache_dev_nonbuf
, arcache_dev_buf
, arcache_norm_noncache_nonbuf
, arcache_norm_noncache_buf
, arcache_wthru_no_alloc
, arcache_wthru_r_alloc
, arcache_wthru_w_alloc
, arcache_wthru_r_w_alloc
, arcache_wback_no_alloc
, arcache_wback_r_alloc
, arcache_wback_w_alloc
, arcache_wback_r_w_alloc
, awcache_dev_nonbuf
, awcache_dev_buf
, awcache_norm_noncache_nonbuf
, awcache_norm_noncache_buf
, awcache_wthru_no_alloc
, awcache_wthru_r_alloc
, awcache_wthru_w_alloc
, awcache_wthru_r_w_alloc
, awcache_wback_no_alloc
, awcache_wback_r_alloc
, awcache_wback_w_alloc
, awcache_wback_r_w_alloc
, AXI4_Prot
, AXI4_Prot_2, prot_2_data, prot_2_inst
, AXI4_Prot_1, prot_1_secure, prot_1_nonsecure
, AXI4_Prot_0, prot_0_unpriv, prot_0_priv
, axi4Prot
, AXI4_QoS
, AXI4_Region
, AXI4_Resp, resp_okay, resp_exokay, resp_slverr, resp_decerr
) where

import Blarney

-- | AXI4 burst length (number of flits - 1)
type AXI4_Len = Bit 8

-- | AXI4 flit size, in bytes (2^size byte(s) in a flit, from 1 to 128)
type AXI4_Size = Bit 3

-- | AXI4 burst type
data AXI4_Burst = AXI4_Burst (Bit 2)
  deriving (Generic, Cmp, Bits, Interface, FShow)

burst_fixed    = AXI4_Burst 0b00
burst_incr     = AXI4_Burst 0b01
burst_wrap     = AXI4_Burst 0b10
burst_reserved = AXI4_Burst 0b11

-- | AXI4 locked accesses type
data AXI4_Lock = AXI4_Lock (Bit 1)
  deriving (Generic, Cmp, Bits, Interface, FShow)

lock_normal    = AXI4_Lock 0b0
lock_exclusive = AXI4_Lock 0b1

-- | AXI4 cache memory attributes
data AXI4_Cache = AXI4_Cache (Bit 4)
  deriving (Generic, Cmp, Bits, Interface, FShow)

arcache_dev_nonbuf           = AXI4_Cache 0b0000
arcache_dev_buf              = AXI4_Cache 0b0001
arcache_norm_noncache_nonbuf = AXI4_Cache 0b0010
arcache_norm_noncache_buf    = AXI4_Cache 0b0011
arcache_wthru_no_alloc       = AXI4_Cache 0b1010
arcache_wthru_r_alloc        = AXI4_Cache 0b1110
arcache_wthru_w_alloc        = AXI4_Cache 0b1010
arcache_wthru_r_w_alloc      = AXI4_Cache 0b1110
arcache_wback_no_alloc       = AXI4_Cache 0b1011
arcache_wback_r_alloc        = AXI4_Cache 0b1111
arcache_wback_w_alloc        = AXI4_Cache 0b1011
arcache_wback_r_w_alloc      = AXI4_Cache 0b1111

awcache_dev_nonbuf           = AXI4_Cache 0b0000
awcache_dev_buf              = AXI4_Cache 0b0001
awcache_norm_noncache_nonbuf = AXI4_Cache 0b0010
awcache_norm_noncache_buf    = AXI4_Cache 0b0011
awcache_wthru_no_alloc       = AXI4_Cache 0b0110
awcache_wthru_r_alloc        = AXI4_Cache 0b0110
awcache_wthru_w_alloc        = AXI4_Cache 0b1110
awcache_wthru_r_w_alloc      = AXI4_Cache 0b1110
awcache_wback_no_alloc       = AXI4_Cache 0b0111
awcache_wback_r_alloc        = AXI4_Cache 0b0111
awcache_wback_w_alloc        = AXI4_Cache 0b1111
awcache_wback_r_w_alloc      = AXI4_Cache 0b1111

-- | AXI4 access permissions
data AXI4_Prot = AXI4_Prot (Bit 3)
  deriving (Generic, Cmp, Bits, Interface, FShow)

data AXI4_Prot_2 = AXI4_Prot_2 (Bit 1)
  deriving (Generic, Cmp, Bits, Interface, FShow)

prot_2_data = AXI4_Prot_2 0b0
prot_2_inst = AXI4_Prot_2 0b1

data AXI4_Prot_1 = AXI4_Prot_1 (Bit 1)
  deriving (Generic, Cmp, Bits, Interface, FShow)

prot_1_secure    = AXI4_Prot_1 0b0
prot_1_nonsecure = AXI4_Prot_1 0b1

data AXI4_Prot_0 = AXI4_Prot_0 (Bit 1)
  deriving (Generic, Cmp, Bits, Interface, FShow)

prot_0_unpriv = AXI4_Prot_0 0b0
prot_0_priv   = AXI4_Prot_0 0b1

axi4Prot :: AXI4_Prot_2 -> AXI4_Prot_1 -> AXI4_Prot_0 -> AXI4_Prot
axi4Prot x y z = AXI4_Prot (pack x # pack y # pack z)

-- | AXI4 QoS signaling
type AXI4_QoS = Bit 4

-- | AXI4 multiple region signaling
type AXI4_Region = Bit 4

-- | AXI4 response values
data AXI4_Resp = AXI4_Resp (Bit 2)
  deriving (Generic, Cmp, Bits, Interface, FShow)

resp_okay   = AXI4_Resp 0b00
resp_exokay = AXI4_Resp 0b01
resp_slverr = AXI4_Resp 0b10
resp_decerr = AXI4_Resp 0b11
