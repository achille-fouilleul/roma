namespace Roma.Cli

type Instruction =
    | Nop
    | Break
    | Ldarg_0
    | Ldarg_1
    | Ldarg_2
    | Ldarg_3
    | Ldloc_0
    | Ldloc_1
    | Ldloc_2
    | Ldloc_3
    | Stloc_0
    | Stloc_1
    | Stloc_2
    | Stloc_3
    | Ldarg_s of int
    | Ldarga_s of int
    | Starg_s of int
    | Ldloc_s of int
    | Ldloca_s of int
    | Stloc_s of int
    | Ldnull
    | Ldc_i4_m1
    | Ldc_i4_0
    | Ldc_i4_1
    | Ldc_i4_2
    | Ldc_i4_3
    | Ldc_i4_4
    | Ldc_i4_5
    | Ldc_i4_6
    | Ldc_i4_7
    | Ldc_i4_8
    | Ldc_i4_s of int32
    | Ldc_i4 of int32
    | Ldc_i8 of int64
    | Ldc_r4 of uint32
    | Ldc_r8 of uint64
    | Dup
    | Pop
    | Jmp of MethodSpec
    | Call of MethodSpec
    | Calli of MethodSig
    | Ret
    | Br_s of int
    | Brfalse_s of int
    | Brtrue_s of int
    | Beq_s of int
    | Bge_s of int
    | Bgt_s of int
    | Ble_s of int
    | Blt_s of int
    | Bne_un_s of int
    | Bge_un_s of int
    | Bgt_un_s of int
    | Ble_un_s of int
    | Blt_un_s of int
    | Br of int
    | Brfalse of int
    | Brtrue of int
    | Beq of int
    | Bge of int
    | Bgt of int
    | Ble of int
    | Blt of int
    | Bne_un of int
    | Bge_un of int
    | Bgt_un of int
    | Ble_un of int
    | Blt_un of int
    | Switch of int array
    | Ldind_i1
    | Ldind_u1
    | Ldind_i2
    | Ldind_u2
    | Ldind_i4
    | Ldind_u4
    | Ldind_i8
    | Ldind_i
    | Ldind_r4
    | Ldind_r8
    | Ldind_ref
    | Stind_ref
    | Stind_i1
    | Stind_i2
    | Stind_i4
    | Stind_i8
    | Stind_r4
    | Stind_r8
    | Add
    | Sub
    | Mul
    | Div
    | Div_un
    | Rem
    | Rem_un
    | And
    | Or
    | Xor
    | Shl
    | Shr
    | Shr_un
    | Neg
    | Not
    | Conv_i1
    | Conv_i2
    | Conv_i4
    | Conv_i8
    | Conv_r4
    | Conv_r8
    | Conv_u4
    | Conv_u8
    | Callvirt of MethodSpec
    | Cpobj of TypeSpec
    | Ldobj of TypeSpec
    | Ldstr of string
    | Newobj of MethodSpec
    | Castclass of TypeSpec
    | Isinst of TypeSpec
    | Conv_r_un
    | Unbox of TypeSpec
    | Throw
    | Ldfld of FieldRef
    | Ldflda of FieldRef
    | Stfld of FieldRef
    | Ldsfld of FieldRef
    | Ldsflda of FieldRef
    | Stsfld of FieldRef
    | Stobj of TypeSpec
    | Conv_ovf_i1_un
    | Conv_ovf_i2_un
    | Conv_ovf_i4_un
    | Conv_ovf_i8_un
    | Conv_ovf_u1_un
    | Conv_ovf_u2_un
    | Conv_ovf_u4_un
    | Conv_ovf_u8_un
    | Conv_ovf_i_un
    | Conv_ovf_u_un
    | Box of TypeSpec
    | Newarr of TypeSpec
    | Ldlen
    | Ldelema of TypeSpec
    | Ldelem_i1
    | Ldelem_u1
    | Ldelem_i2
    | Ldelem_u2
    | Ldelem_i4
    | Ldelem_u4
    | Ldelem_i8
    | Ldelem_i
    | Ldelem_r4
    | Ldelem_r8
    | Ldelem_ref
    | Stelem_i
    | Stelem_i1
    | Stelem_i2
    | Stelem_i4
    | Stelem_i8
    | Stelem_r4
    | Stelem_r8
    | Stelem_ref
    | Ldelem of TypeSpec
    | Stelem of TypeSpec
    | Unbox_any of TypeSpec
    | Conv_ovf_i1
    | Conv_ovf_u1
    | Conv_ovf_i2
    | Conv_ovf_u2
    | Conv_ovf_i4
    | Conv_ovf_u4
    | Conv_ovf_i8
    | Conv_ovf_u8
    | Refanyval of TypeSpec
    | Ckfinite
    | Mkrefany of TypeSpec
    | Ldtoken_type of TypeSpec
    | Ldtoken_method of MethodSpec
    | Ldtoken_field of FieldRef
    | Conv_u2
    | Conv_u1
    | Conv_i
    | Conv_ovf_i
    | Conv_ovf_u
    | Add_ovf
    | Add_ovf_un
    | Mul_ovf
    | Mul_ovf_un
    | Sub_ovf
    | Sub_ovf_un
    | Endfinally // also endfault
    | Leave of int
    | Leave_s of int
    | Stind_i
    | Conv_u

    // two-byte opcodes
    | Arglist
    | Ceq
    | Cgt
    | Cgt_un
    | Clt
    | Clt_un
    | Ldftn of MethodSpec
    | Ldvirtftn of MethodSpec
    | Ldarg of int
    | Ldarga of int
    | Starg of int
    | Ldloc of int
    | Ldloca of int
    | Stloc of int
    | Localloc
    | Endfilter
    | Unaligned // prefix
    | Volatile // prefix
    | Tail // prefix
    | Initobj of TypeSpec
    | Constrained of TypeSpec // prefix
    | Cpblk
    | Initblk
    // TODO: no.
    | Rethrow
    | Sizeof of TypeSpec
    | Refanytype
    | Readonly // prefix


