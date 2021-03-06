﻿#load "LittleEndian.fs"
#load "BigEndian.fs"

open System.Diagnostics
open Roma.Cli

let check x msg =
    if not(x) then
        failwithf "test %s failed" msg

check ((LittleEndian.u16 [| 0x67uy; 0x45uy; 0x23uy; 0x01uy |] 0) = 0x4567us) "le u16 0"
check ((LittleEndian.u16 [| 0x67uy; 0x45uy; 0x23uy; 0x01uy |] 1) = 0x2345us) "le u16 1"
check ((LittleEndian.u16 [| 0x67uy; 0x45uy; 0x23uy; 0x01uy |] 2) = 0x0123us) "le u16 2"
check ((LittleEndian.u32 [| 0x67uy; 0x45uy; 0x23uy; 0x01uy |] 0) = 0x01234567u) "le u32 0"
check ((LittleEndian.u64 [| 0xefuy; 0xcduy; 0xabuy; 0x89uy; 0x67uy; 0x45uy; 0x23uy; 0x01uy |] 0) = 0x0123456789abcdefUL) "le u64 0"
check ((BigEndian.u16 [| 0x67uy; 0x45uy; 0x23uy; 0x01uy |] 0) = 0x6745us) "be u16 0"
check ((BigEndian.u16 [| 0x67uy; 0x45uy; 0x23uy; 0x01uy |] 1) = 0x4523us) "be u16 1"
check ((BigEndian.u16 [| 0x67uy; 0x45uy; 0x23uy; 0x01uy |] 2) = 0x2301us) "be u16 2"
check ((BigEndian.u32 [| 0x67uy; 0x45uy; 0x23uy; 0x01uy |] 0) = 0x67452301u) "be u32 1"
check ((BigEndian.u64 [| 0xefuy; 0xcduy; 0xabuy; 0x89uy; 0x67uy; 0x45uy; 0x23uy; 0x01uy |] 0) = 0xefcdab8967452301UL) "be u64 1"
