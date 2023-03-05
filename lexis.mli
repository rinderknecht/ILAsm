type prefix =
  Unaligned
| Constrained
| No
| ReadOnly
| Tail
| Volatile

type meta =
  AddOn
| Assembly
| CCtor
| Class
| CorFlags
| Ctor
| Custom
| Data
| EmitByte
| EntryPoint
| Event
| Export
| Field
| File
| Fire
| Get
| Hash
| ImageBase
| Import
| Language
| Line
| SharpLine
| Locale
| Localized
| Locals
| ManifestRes
| MaxStack
| Method
| Module
| MResource
| Namespace
| Other
| Override
| Pack
| Param
| PDirect
| Permission
| PermissionSet
| Property
| PublicKey
| PublicKeyToken
| RemoveOn
| Set
| Size
| Subsystem
| Try
| Ver
| VTable
| VTEntry
| VTFixup
| ZeroInit

type instr =
  Abstract
| Add
| Add_ovf
| Add_ovf_un
| Algorithm
| Alignment
| And
| ANSI
| Any
| ArgList
| Array
| As
| Assembly
| Assert
| At
| Auto
| AutoChar
| BeforeFieldInit
| Beq
| Beq_s
| Bge
| Bge_s
| Bge_un
| Bge_un_s
| Bgt
| Bgt_s
| Bgt_un
| Bgt_un_s
| Ble
| Ble_s
| Ble_un
| Ble_un_s
| Blob
| Blob_object
| Blt
| Blt_s
| Blt_un
| Blt_un_s
| Bne_un
| Bne_un_s
| Bool
| Box
| Br
| Br_s
| Break
| BrFalse
| BrFalse_s
| BrInst
| BrInst_s
| BrNull
| BrNull_s
| BrTrue
| BrTrue_s
| BrZero
| BrZero_s
| Bstr
| ByteArray
| ByValStr
| Call
| CallI
| CallMostDerived
| CallVirt
| CArray
| CastClass
| Catch
| CDecl
| Ceq
| Cf
| Cgt
| Cgt_un
| Char
| CIL
| CKfinite
| Class
| CLSid
| Clt
| Clt_un
| Const
| Conv_i
| Conv_i1
| Conv_i2
| Conv_i4
| Conv_i8
| Conv_ovf_i
| Conv_ovf_i_un
| Conv_ovf_i1
| Conv_ovf_i1_un
| Conv_ovf_i2
| Conv_ovf_i2_un
| Conv_ovf_i4
| Conv_ovf_i4_un
| Conv_ovf_i8
| Conv_ovf_i8_un
| Conv_ovf_u
| Conv_ovf_u_un
| Conv_ovf_u1
| Conv_ovf_u1_un
| Conv_ovf_u2
| Conv_ovf_u2_un
| Conv_ovf_u4
| Conv_ovf_u4_un
| Conv_ovf_u8
| Conv_ovf_u8_un
| Conv_r_un
| Conv_r4
| Conv_r8
| Conv_u
| Conv_u1
| Conv_u2
| Conv_u4
| Conv_u8
| CpBlk
| CpObj
| Currency
| Custom
| Date
| Decimal
| Default
| Demand
| Deny
| Div
| Div_un
| Dup
| EndFault
| EndFilter
| EndFinally
| EndMac
| Enum
| Error
| Explicit
| Extends
| Extern
| False
| FamAndAssem
| Family
| FamOrAssem
| FastCall
| Fault
| Field
| FileTime
| Filter
| Final
| Finally
| Fixed
| Float
| Float32
| Float64
| ForwardRef
| FromUnmanaged
| Handler
| HideBySig
| HResult
| IDispatch
| IL
| Illegal
| Implements
| ImplicitCom
| ImplicitRes
| Import
| In
| InheritCheck
| Init
| InitBlk
| InitObj
| InitOnly
| Instance
| Int
| Int16
| Int32
| Int64
| Int8
| Interface
| InternalCall
| ISinst
| Iunknown
| Jmp
| LastErr
| LCid
| LdArg
| LdArg_0
| LdArg_1
| LdArg_2
| LdArg_3
| LdArg_s
| LdArgA
| LdArgA_s
| LdC_i4
| LdC_i4_0
| LdC_i4_1
| LdC_i4_2
| LdC_i4_3
| LdC_i4_4
| LdC_i4_5
| LdC_i4_6
| LdC_i4_7
| LdC_i4_8
| LdC_i4_M1
| LdC_i4_m1
| LdC_i4_s
| LdC_i8
| LdC_r4
| LdC_r8
| LdElem
| LdElem_i
| LdElem_i1
| LdElem_i2
| LdElem_i4
| LdElem_i8
| LdElem_r4
| LdElem_r8
| LdElem_ref
| LdElem_u1
| LdElem_u2
| LdElem_u4
| LdElem_u8
| LdElemA
| LdFld
| LdFldA
| LdFtn
| LdInd_i
| LdInd_i1
| LdInd_i2
| LdInd_i4
| LdInd_i8
| LdInd_r4
| LdInd_r8
| LdInd_ref
| LdInd_u1
| LdInd_u2
| LdInd_u4
| LdInd_u8
| LdLen
| LdLoc
| LdLoc_0
| LdLoc_1
| LdLoc_2
| LdLoc_3
| LdLoc_s
| LdLocA
| LdLocA_s
| LdNull
| LdObj
| LdSFld
| LdSFldA
| LdStr
| LdToken
| LdVirtFtn
| Leave
| Leave_s
| LinkCheck
| Literal
| LocalLoc
| LpStr
| LpStruct
| LpTStr
| LpVoid
| LpWStr
| Managed
| Marshal
| Method
| MkRefAny
| ModOpt
| ModReq
| Mul
| Mul_ovf
| Mul_ovf_un
| Native
| Neg
| Nested
| NewArr
| NewObj
| NewSlot
| NoAppDomain
| NoInlining
| NoMachine
| NoMangle
| NoMetadata
| NonCasDemand
| NonCasInheritance
| NonCasLinkDemand
| Nop
| NoProcess
| Not
| Not_in_GC_heap
| NotRemotable
| NotSerialized
| Null
| NullRef
| Object
| ObjectRef
| Opt
| OptIL
| Or
| Out
| PermitOnly
| Pinned
| PInvokeImpl
| Pop
| Prefix1
| Prefix2
| Prefix3
| Prefix4
| Prefix5
| Prefix6
| Prefix7
| PrefixRef
| PreJITdeny
| PreJITgrant
| PreserveSig
| Private
| PrivateScope
| Protected
| Public
| Record
| RefAny
| RefAnyType
| RefAnyVal
| Rem
| Rem_un
| ReqMin
| ReqOpt
| ReqRefuse
| ReqSecObj
| Request
| Ret
| Rethrow
| RetVal
| RTspecialName
| Runtime
| SafeArray
| Sealed
| Sequential
| Serializable
| ShL
| ShR
| ShR_un
| SizeOf
| Special
| SpecialName
| StArg
| StArg_s
| Static
| StdCall
| StElem
| StElem_i
| StElem_i1
| StElem_i2
| StElem_i4
| StElem_i8
| StElem_r4
| StElem_r8
| StElem_ref
| StFld
| StInd_i
| StInd_i1
| StInd_i2
| StInd_i4
| StInd_i8
| StInd_r4
| StInd_r8
| StInd_ref
| StLoc
| StLoc_0
| StLoc_1
| StLoc_2
| StLoc_3
| StLoc_s
| StObj
| Storage
| Stored_Object
| Stream
| Streamed_Object
| String
| Struct
| StSFld
| Sub
| Sub_ovf
| Sub_ovf_un
| Switch
| Synchronized
| SysChar
| SysString
| TbStr
| ThisCall
| Throw
| TLS
| TO
| True
| TypedRef
| UnBox
| Unbox_Any
| Unicode
| Unmanaged
| UnmanagedExp
| Unsigned
| Unused
| UserDefined
| Value
| ValueType
| VarArg
| Variant
| Vector
| Virtual
| Void
| WChar
| WInAPI
| With
| Wrapper
| Xor