{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blarney.AXI4TypesCommon (
  AXI4_Len
, AXI4_Size
, AXI4_Burst, fixed, incr, wrap, reserved
, AXI4_Lock, normal, exclusive
, AXI4_Cache
, AXI4_Prot
, AXI4_Prot_2, prot_2_data, prot_2_inst
, AXI4_Prot_1, prot_1_secure, prot_1_nonsecure
, AXI4_Prot_0, prot_0_unpriv, prot_0_priv
, axi4Prot
, AXI4_QoS
, AXI4_Region
, AXI4_Resp, okay, exokay, slverr, decerr
) where

import Blarney

-- | AXI4 burst length (number of flits - 1)
type AXI4_Len = Bit 8

-- | AXI4 flit size, in bytes (2^size byte(s) in a flit, from 1 to 128)
type AXI4_Size = Bit 3

-- | AXI4 burst type
newtype AXI4_Burst = AXI4_Burst (Bit 2) deriving (Generic, Cmp, Bits, Interface)
fixed = AXI4_Burst 0b00
incr = AXI4_Burst 0b01
wrap = AXI4_Burst 0b10
reserved = AXI4_Burst 0b11

-- | AXI4 locked accesses type
newtype AXI4_Lock = AXI4_Lock (Bit 1) deriving (Generic, Cmp, Bits, Interface)
normal = AXI4_Lock 0b0
exclusive = AXI4_Lock 0b1

-- | AXI4 "cache" memory attributes
type AXI4_Cache = Bit 4

-- arcache_dev_nonbuf           = 4'b0000;
-- arcache_dev_buf              = 4'b0001;
-- arcache_norm_noncache_nonbuf = 4'b0010;
-- arcache_norm_noncache_buf    = 4'b0011;
-- arcache_wthru_no_alloc       = 4'b1010;
-- arcache_wthru_r_alloc        = 4'b1110;
-- arcache_wthru_w_alloc        = 4'b1010;
-- arcache_wthru_r_w_alloc      = 4'b1110;
-- arcache_wback_no_alloc       = 4'b1011;
-- arcache_wback_r_alloc        = 4'b1111;
-- arcache_wback_w_alloc        = 4'b1011;
-- arcache_wback_r_w_alloc      = 4'b1111;

-- awcache_dev_nonbuf           = 4'b0000;
-- awcache_dev_buf              = 4'b0001;
-- awcache_norm_noncache_nonbuf = 4'b0010;
-- awcache_norm_noncache_buf    = 4'b0011;
-- awcache_wthru_no_alloc       = 4'b0110;
-- awcache_wthru_r_alloc        = 4'b0110;
-- awcache_wthru_w_alloc        = 4'b1110;
-- awcache_wthru_r_w_alloc      = 4'b1110;
-- awcache_wback_no_alloc       = 4'b0111;
-- awcache_wback_r_alloc        = 4'b0111;
-- awcache_wback_w_alloc        = 4'b1111;
-- awcache_wback_r_w_alloc      = 4'b1111;

-- | AXI4 access permissions
type AXI4_Prot = Bit 3
newtype AXI4_Prot_2 = AXI4_Prot_2 (Bit 1) deriving (Generic, Cmp, Bits)
prot_2_data = AXI4_Prot_2 0b0
prot_2_inst = AXI4_Prot_2 0b1
newtype AXI4_Prot_1 = AXI4_Prot_1 (Bit 1) deriving (Generic, Cmp, Bits)
prot_1_secure = AXI4_Prot_1 0b0
prot_1_nonsecure = AXI4_Prot_1 0b1
newtype AXI4_Prot_0 = AXI4_Prot_0 (Bit 1) deriving (Generic, Cmp, Bits)
prot_0_unpriv = AXI4_Prot_0 0b0
prot_0_priv = AXI4_Prot_0 0b1

axi4Prot :: AXI4_Prot_2 -> AXI4_Prot_1 -> AXI4_Prot_0 -> AXI4_Prot
axi4Prot x y z = pack x # pack y # pack z

-- | AXI4 QoS signaling
type AXI4_QoS = Bit 4

-- | AXI4 multiple region signaling
type AXI4_Region = Bit 4

-- | AXI4 response values
newtype AXI4_Resp = AXI4_Resp (Bit 2) deriving (Generic, Cmp, Bits, Interface)
okay = AXI4_Resp 0b00
exokay = AXI4_Resp 0b01
slverr = AXI4_Resp 0b10
decerr = AXI4_Resp 0b11
