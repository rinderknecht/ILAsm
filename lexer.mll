{
(* Lexer specification of ILAsm for [ocamllex] *)

(* String processing *)

module StringMap = Map.Make(String)

let add map (key,value) = StringMap.add key value map

let mk_str (len:int) (p: char list) : string =
  let s = Bytes.make len ' ' in 
  let rec fill i =
    function [] -> s | c::l -> Bytes.set s i c; fill (i-1) l
in assert (len = List.length p); Bytes.to_string (fill (len-1) p)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i-1) (s.[i]::l)
in exp (String.length s - 1)

(* Virtual line number (according to #line) and end of lines *)

let virt_lnum = ref 1

let handle_nl buffer = Lexing.new_line buffer; incr virt_lnum

(* Error handling *)

let fail msg buffer =
  raise (Error.(Lexer (msg, mk_seg buffer, !virt_lnum)))

let halt msg buffer =
  let pos = buffer.Lexing.lex_curr_p
in raise (Error.(Lexer (msg, (pos, pos), !virt_lnum)))

exception Local_err of Error.message

let handle_err scan buffer =
  try scan buffer with Local_err msg -> fail msg buffer

(* List of metadata *)

let metadata_list =
  let open Lexis in [
           "addon", AddOn;
        "assembly", Assembly;
           "cctor", CCtor;
           "class", Class;
        "corflags", CorFlags;
            "ctor", Ctor;
          "custom", Custom;
            "data", Data;
        "emitbyte", EmitByte;
      "entrypoint", EntryPoint;
           "event", Event;
          "export", Export;
           "field", Field;
            "file", File;
            "fire", Fire;
             "get", Get;
            "hash", Hash;
       "imagebase", ImageBase;
          "import", Import;
        "language", Language;
            "line", Line;
          "locale", Locale;
       "localized", Localized;
          "locals", Locals;
     "manifestres", ManifestRes;
        "maxstack", MaxStack;
          "method", Method;
          "module", Module;
       "mresource", MResource;
       "namespace", Namespace;
           "other", Other;
        "override", Override;
            "pack", Pack;
           "param", Param;
         "pdirect", PDirect;
      "permission", Permission;
   "permissionset", PermissionSet;
        "property", Property;
       "publickey", PublicKey;
  "publickeytoken", PublicKeyToken;
        "removeon", RemoveOn;
             "set", Set;
            "size", Size;
       "subsystem", Subsystem;
             "try", Try;
             "ver", Ver;
          "vtable", VTable;
         "vtentry", VTEntry;
         "vtfixup", VTFixup;
        "zeroinit", ZeroInit
]

let metadata_map = List.fold_left add StringMap.empty metadata_list


(* List of prefixes *)

let prefix_list =
  let open Lexis in [
    "unaligned", Unaligned;
  "constrained", Constrained;
           "no", No;
     "readonly", ReadOnly;
     "volatile", Volatile
]

let prefix_map = List.fold_left add StringMap.empty prefix_list


(* List of instructions *)

let instr_list =
  let open Lexis in [
           "abstract", Abstract;
                "add", Add;
            "add.ovf", Add_ovf;
         "add.ovf.un", Add_ovf_un;
          "algorithm", Algorithm;
          "alignment", Alignment;
                "and", And;
               "ansi", ANSI;
                "any", Any;
            "arglist", ArgList;
              "array", Array;
                 "as", As;
           "assembly", Assembly;
             "assert", Assert;
                 "at", At;
               "auto", Auto;
           "autochar", AutoChar;
    "beforefieldinit", BeforeFieldInit;
                "beq", Beq;
              "beq.s", Beq_s;
                "bge", Bge;
              "bge.s", Bge_s;
             "bge.un", Bge_un;
           "bge.un.s", Bge_un_s;
                "bgt", Bgt;
              "bgt.s", Bgt_s;
             "bgt.un", Bgt_un;
           "bgt.un.s", Bgt_un_s;
                "ble", Ble;
              "ble.s", Ble_s;
             "ble.un", Ble_un;
           "ble.un.s", Ble_un_s;
               "blob", Blob;
        "blob_object", Blob_object;
                "blt", Blt;
              "blt.s", Blt_s;
             "blt.un", Blt_un;
           "blt.un.s", Blt_un_s;
             "bne.un", Bne_un;
           "bne.un.s", Bne_un_s;
               "bool", Bool;
                "box", Box;
                 "br", Br;
               "br.s", Br_s;
              "break", Break;
            "brfalse", BrFalse;
          "brfalse.s", BrFalse_s;
             "brinst", BrInst;
           "brinst.s", BrInst_s;
             "brnull", BrNull;
           "brnull.s", BrNull_s;
             "brtrue", BrTrue;
           "brtrue.s", BrTrue_s;
             "brzero", BrZero;
           "brzero.s", BrZero_s;
               "bstr", Bstr;
          "bytearray", ByteArray;
           "byvalstr", ByValStr;
               "call", Call;
              "calli", CallI;
    "callmostderived", CallMostDerived;
           "callvirt", CallVirt;
             "carray", CArray;
          "castclass", CastClass;
              "catch", Catch;
              "cdecl", CDecl;
                "ceq", Ceq;
                 "cf", Cf;
                "cgt", Cgt;
             "cgt.un", Cgt_un;
               "char", Char;
                "cil", CIL;
           "ckfinite", CKfinite;
              "class", Class;
              "clsid", CLSid;
                "clt", Clt;
             "clt.un", Clt_un;
              "const", Const;
             "conv.i", Conv_i;
            "conv.i1", Conv_i1;
            "conv.i2", Conv_i2;
            "conv.i4", Conv_i4;
            "conv.i8", Conv_i8;
         "conv.ovf.i", Conv_ovf_i;
      "conv.ovf.i.un", Conv_ovf_i_un;
        "conv.ovf.i1", Conv_ovf_i1;
     "conv.ovf.i1.un", Conv_ovf_i1_un;
        "conv.ovf.i2", Conv_ovf_i2;
     "conv.ovf.i2.un", Conv_ovf_i2_un;
        "conv.ovf.i4", Conv_ovf_i4;
     "conv.ovf.i4.un", Conv_ovf_i4_un;
        "conv.ovf.i8", Conv_ovf_i8;
     "conv.ovf.i8.un", Conv_ovf_i8_un;
         "conv.ovf.u", Conv_ovf_u;
      "conv.ovf.u.un", Conv_ovf_u_un;
        "conv.ovf.u1", Conv_ovf_u1;
     "conv.ovf.u1.un", Conv_ovf_u1_un;
        "conv.ovf.u2", Conv_ovf_u2;
     "conv.ovf.u2.un", Conv_ovf_u2_un;
        "conv.ovf.u4", Conv_ovf_u4;
     "conv.ovf.u4.un", Conv_ovf_u4_un;
        "conv.ovf.u8", Conv_ovf_u8;
     "conv.ovf.u8.un", Conv_ovf_u8_un;
          "conv.r.un", Conv_r_un;
            "conv.r4", Conv_r4;
            "conv.r8", Conv_r8;
             "conv.u", Conv_u;
            "conv.u1", Conv_u1;
            "conv.u2", Conv_u2;
            "conv.u4", Conv_u4;
            "conv.u8", Conv_u8;
              "cpblk", CpBlk;
              "cpobj", CpObj;
           "currency", Currency;
             "custom", Custom;
               "date", Date;
            "decimal", Decimal;
            "default", Default;
             "demand", Demand;
               "deny", Deny;
                "div", Div;
             "div_un", Div_un;
                "dup", Dup;
           "endfault", EndFault;
          "endfilter", EndFilter;
         "endfinally", EndFinally;
             "endmac", EndMac;
               "enum", Enum;
              "error", Error;
           "explicit", Explicit;
            "extends", Extends;
             "extern", Extern;
              "false", False;
        "famandassem", FamAndAssem;
             "family", Family;
         "famorassem", FamOrAssem;
           "fastcall", FastCall;
              "fault", Fault;
              "field", Field;
           "filetime", FileTime;
             "filter", Filter;
              "final", Final;
            "finally", Finally;
              "fixed", Fixed;
              "float", Float;
            "float32", Float32;
            "float64", Float64;
         "forwardref", ForwardRef;
      "fromunmanaged", FromUnmanaged;
            "handler", Handler;
          "hidebysig", HideBySig;
            "hresult", HResult;
          "idispatch", IDispatch;
                 "il", IL;
            "illegal", Illegal;
         "implements", Implements;
        "implicitcom", ImplicitCom;
        "implicitres", ImplicitRes;
             "import", Import;
                 "in", In;
       "inheritcheck", InheritCheck;
               "init", Init;
            "initblk", InitBlk;
            "initobj", InitObj;
           "initonly", InitOnly;
           "instance", Instance;
                "int", Int;
              "int16", Int16;
              "int32", Int32;
              "int64", Int64;
               "int8", Int8;
          "interface", Interface;
       "internalcall", InternalCall;
             "isinst", ISinst;
           "iunknown", Iunknown;
                "jmp", Jmp;
            "lasterr", LastErr;
               "lcid", LCid;
              "ldarg", LdArg;
            "ldarg.0", LdArg_0;
            "ldarg.1", LdArg_1;
            "ldarg.2", LdArg_2;
            "ldarg.3", LdArg_3;
            "ldarg.s", LdArg_s;
             "ldarga", LdArgA;
           "ldarga.s", LdArgA_s;
             "ldc.i4", LdC_i4;
           "ldc.i4.0", LdC_i4_0;
           "ldc.i4.1", LdC_i4_1;
           "ldc.i4.2", LdC_i4_2;
           "ldc.i4.3", LdC_i4_3;
           "ldc.i4.4", LdC_i4_4;
           "ldc.i4.5", LdC_i4_5;
           "ldc.i4.6", LdC_i4_6;
           "ldc.i4.7", LdC_i4_7;
           "ldc.i4.8", LdC_i4_8;
          "ldc.i4.M1", LdC_i4_M1;
          "ldc.i4.m1", LdC_i4_m1;
           "ldc.i4.s", LdC_i4_s;
             "ldc.i8", LdC_i8;
             "ldc.r4", LdC_r4;
             "ldc.r8", LdC_r8;
             "ldelem", LdElem;
           "ldelem.i", LdElem_i;
          "ldelem.i1", LdElem_i1;
          "ldelem.i2", LdElem_i2;
          "ldelem.i4", LdElem_i4;
          "ldelem.i8", LdElem_i8;
          "ldelem.r4", LdElem_r4;
          "ldelem.r8", LdElem_r8;
         "ldelem.ref", LdElem_ref;
          "ldelem.u1", LdElem_u1;
          "ldelem.u2", LdElem_u2;
          "ldelem.u4", LdElem_u4;
          "ldelem.u8", LdElem_u8;
            "ldelema", LdElemA;
              "ldfld", LdFld;
             "ldflda", LdFldA;
              "ldftn", LdFtn;
            "ldind.i", LdInd_i;
           "ldind.i1", LdInd_i1;
           "ldind.i2", LdInd_i2;
           "ldind.i4", LdInd_i4;
           "ldind.i8", LdInd_i8;
           "ldind.r4", LdInd_r4;
           "ldind.r8", LdInd_r8;
          "ldind.ref", LdInd_ref;
           "ldind.u1", LdInd_u1;
           "ldind.u2", LdInd_u2;
           "ldind.u4", LdInd_u4;
           "ldind.u8", LdInd_u8;
              "ldlen", LdLen;
              "ldloc", LdLoc;
            "ldloc.0", LdLoc_0;
            "ldloc.1", LdLoc_1;
            "ldloc.2", LdLoc_2;
            "ldloc.3", LdLoc_3;
            "ldloc.s", LdLoc_s;
             "ldloca", LdLocA;
           "ldloca.s", LdLocA_s;
             "ldnull", LdNull;
              "ldobj", LdObj;
             "ldsfld", LdSFld;
            "ldsflda", LdSFldA;
              "ldstr", LdStr;
            "ldtoken", LdToken;
          "ldvirtftn", LdVirtFtn;
              "leave", Leave;
            "leave.s", Leave_s;
          "linkcheck", LinkCheck;
            "literal", Literal;
           "localloc", LocalLoc;
              "lpstr", LpStr;
           "lpstruct", LpStruct;
             "lptstr", LpTStr;
             "lpvoid", LpVoid;
             "lpwstr", LpWStr;
            "managed", Managed;
            "marshal", Marshal;
             "method", Method;
           "mkrefany", MkRefAny;
             "modopt", ModOpt;
             "modreq", ModReq;
                "mul", Mul;
            "mul.ovf", Mul_ovf;
         "mul.ovf.un", Mul_ovf_un;
             "native", Native;
                "neg", Neg;
             "nested", Nested;
             "newarr", NewArr;
             "newobj", NewObj;
            "newslot", NewSlot;
        "noappdomain", NoAppDomain;
         "noinlining", NoInlining;
          "nomachine", NoMachine;
           "nomangle", NoMangle;
         "nometadata", NoMetadata;
       "noncasdemand", NonCasDemand;
  "noncasinheritance", NonCasInheritance;
   "noncaslinkdemand", NonCasLinkDemand;
                "nop", Nop;
          "noprocess", NoProcess;
                "not", Not;
     "not_in_gc_heap", Not_in_GC_heap;
       "notremotable", NotRemotable;
      "notserialized", NotSerialized;
               "null", Null;
            "nullref", NullRef;
             "object", Object;
          "objectref", ObjectRef;
                "opt", Opt;
              "optil", OptIL;
                 "or", Or;
                "out", Out;
         "permitonly", PermitOnly;
             "pinned", Pinned;
        "pinvokeimpl", PInvokeImpl;
                "pop", Pop;
            "prefix1", Prefix1;
            "prefix2", Prefix2;
            "prefix3", Prefix3;
            "prefix4", Prefix4;
            "prefix5", Prefix5;
            "prefix6", Prefix6;
            "prefix7", Prefix7;
          "prefixref", PrefixRef;
         "prejitdeny", PreJITdeny;
        "prejitgrant", PreJITgrant;
        "preservesig", PreserveSig;
            "private", Private;
       "privatescope", PrivateScope;
          "protected", Protected;
             "public", Public;
             "record", Record;
             "refany", RefAny;
         "refanytype", RefAnyType;
          "refanyval", RefAnyVal;
                "rem", Rem;
             "rem.un", Rem_un;
             "reqmin", ReqMin;
             "reqopt", ReqOpt;
          "reqrefuse", ReqRefuse;
          "reqsecobj", ReqSecObj;
            "request", Request;
                "ret", Ret;
            "rethrow", Rethrow;
             "retval", RetVal;
      "rtspecialname", RTspecialName;
            "runtime", Runtime;
          "safearray", SafeArray;
             "sealed", Sealed;
         "sequential", Sequential;
       "serializable", Serializable;
                "shl", ShL;
                "shr", ShR;
             "shr.un", ShR_un;
             "sizeof", SizeOf;
            "special", Special;
        "specialname", SpecialName;
              "starg", StArg;
            "starg.s", StArg_s;
             "static", Static;
            "stdcall", StdCall;
             "stelem", StElem;
           "stelem.i", StElem_i;
          "stelem.i1", StElem_i1;
          "stelem.i2", StElem_i2;
          "stelem.i4", StElem_i4;
          "stelem.i8", StElem_i8;
          "stelem.r4", StElem_r4;
          "stelem.r8", StElem_r8;
         "stelem.ref", StElem_ref;
              "stfld", StFld;
            "stind.i", StInd_i;
           "stind.i1", StInd_i1;
           "stind.i2", StInd_i2;
           "stind.i4", StInd_i4;
           "stind.i8", StInd_i8;
           "stind.r4", StInd_r4;
           "stind.r8", StInd_r8;
          "stind.ref", StInd_ref;
              "stloc", StLoc;
            "stloc.0", StLoc_0;
            "stloc.1", StLoc_1;
            "stloc.2", StLoc_2;
            "stloc.3", StLoc_3;
            "stloc.s", StLoc_s;
              "stobj", StObj;
            "storage", Storage;
      "stored_object", Stored_Object;
             "stream", Stream;
    "streamed_object", Streamed_Object;
             "string", String;
             "struct", Struct;
             "stsfld", StSFld;
                "sub", Sub;
            "sub.ovf", Sub_ovf;
         "sub.ovf.un", Sub_ovf_un;
             "switch", Switch;
       "synchronized", Synchronized;
            "syschar", SysChar;
          "sysstring", SysString;
              "tbstr", TbStr;
           "thiscall", ThisCall;
              "throw", Throw;
                "tls", TLS;
                 "to", TO;
               "true", True;
           "typedref", TypedRef;
              "unbox", UnBox;
          "unbox.any", Unbox_Any;
            "unicode", Unicode;
          "unmanaged", Unmanaged;
       "unmanagedexp", UnmanagedExp;
           "unsigned", Unsigned;
             "unused", Unused;
        "userdefined", UserDefined;
              "value", Value;
          "valuetype", ValueType;
             "vararg", VarArg;
            "variant", Variant;
             "vector", Vector;
            "virtual", Virtual;
               "void", Void;
              "wchar", WChar;
             "winapi", WInAPI;
               "with", With;
            "wrapper", Wrapper;
                "xor", Xor
]

let instr_map = List.fold_left add StringMap.empty instr_list

(* Concrete syntax of tokens *)

let string_of_token = 
  let open Parser in let open Lexis in function

(* Symbols *)

    LBrace -> "{"
|   RBrace -> "}"
|     Plus -> "+"
|    Comma -> ","
|       Eq -> "="
|     LPar -> "("
|     RPar -> ")"
|   LBrack -> "["
|   RBrack -> "]"
|   DColon -> "::"
|    Times -> "*"
|    Amper -> "&"
| Ellipsis -> "..."
|    Slash -> "/"
|     Bang -> "!"
|    Colon -> ":"

(* Metadata *)

| Meta          AddOn -> ".addon"
| Meta       Assembly -> ".assembly"
| Meta          CCtor -> ".cctor"
| Meta          Class -> ".class"
| Meta       CorFlags -> ".corflags"
| Meta           Ctor -> ".ctor"
| Meta         Custom -> ".custom"
| Meta           Data -> ".data"
| Meta       EmitByte -> ".emitbyte"
| Meta     EntryPoint -> ".entrypoint"
| Meta          Event -> ".event"
| Meta         Export -> ".export"
| Meta          Field -> ".field"
| Meta           File -> ".file"
| Meta           Fire -> ".fire"
| Meta            Get -> ".get"
| Meta           Hash -> ".hash"
| Meta      ImageBase -> ".imagebase"
| Meta         Import -> ".import"
| Meta       Language -> ".language"
| Meta           Line -> ".line"
| Meta      SharpLine -> "#line"
| Meta         Locale -> ".locale"
| Meta      Localized -> ".localized"
| Meta         Locals -> ".locals"
| Meta    ManifestRes -> ".manifestres"
| Meta       MaxStack -> ".maxstack"
| Meta         Method -> ".method"
| Meta         Module -> ".module"
| Meta      MResource -> ".mresource"
| Meta      Namespace -> ".namespace"
| Meta          Other -> ".other"
| Meta       Override -> ".override"
| Meta           Pack -> ".pack"
| Meta          Param -> ".param"
| Meta        PDirect -> ".pdirect"
| Meta     Permission -> ".permission"
| Meta  PermissionSet -> ".permissionset"
| Meta       Property -> ".property"
| Meta      PublicKey -> ".publickey"
| Meta PublicKeyToken -> ".publickeytoken"
| Meta       RemoveOn -> ".removeon"
| Meta            Set -> ".set"
| Meta           Size -> ".size"
| Meta      Subsystem -> ".subsystem"
| Meta            Try -> ".try"
| Meta            Ver -> ".ver"
| Meta         VTable -> ".vtable"
| Meta        VTEntry -> ".vtentry"
| Meta        VTFixup -> ".vtfixup"
| Meta       ZeroInit -> ".zeroinit"

(* Prefixes *)

| Prefix   Unaligned -> "unaligned."
| Prefix Constrained -> "constrained."
| Prefix          No -> "no."
| Prefix    ReadOnly -> "readonly."
| Prefix        Tail -> "tail."
| Prefix    Volatile -> "volatile."

(* Instructions *) 

| Instr          Abstract -> "abstract"
| Instr               Add -> "add"
| Instr           Add_ovf -> "add.ovf"
| Instr        Add_ovf_un -> "add.ovf.un"
| Instr         Algorithm -> "algorithm"
| Instr         Alignment -> "alignment"
| Instr               And -> "and"
| Instr              ANSI -> "ansi"
| Instr               Any -> "any"
| Instr           ArgList -> "arglist"
| Instr             Array -> "array"
| Instr                As -> "as"
| Instr          Assembly -> "assembly"
| Instr            Assert -> "assert"
| Instr                At -> "at"
| Instr              Auto -> "auto"
| Instr          AutoChar -> "autochar"
| Instr   BeforeFieldInit -> "beforefieldinit"
| Instr               Beq -> "beq"
| Instr             Beq_s -> "beq.s"
| Instr               Bge -> "bge"
| Instr             Bge_s -> "bge.s"
| Instr            Bge_un -> "bge.un"
| Instr          Bge_un_s -> "bge.un.s"
| Instr               Bgt -> "bgt"
| Instr             Bgt_s -> "bgt.s"
| Instr            Bgt_un -> "bgt.un"
| Instr          Bgt_un_s -> "bgt.un.s"
| Instr               Ble -> "ble"
| Instr             Ble_s -> "ble.s"
| Instr            Ble_un -> "ble.un"
| Instr          Ble_un_s -> "ble.un.s"
| Instr              Blob -> "blob"
| Instr       Blob_object -> "blob_object"
| Instr               Blt -> "blt"
| Instr             Blt_s -> "blt.s"
| Instr            Blt_un -> "blt.un"
| Instr          Blt_un_s -> "blt.un.s"
| Instr            Bne_un -> "bne.un"
| Instr          Bne_un_s -> "bne.un.s"
| Instr              Bool -> "bool"
| Instr               Box -> "box"
| Instr                Br -> "br"
| Instr              Br_s -> "br.s"
| Instr             Break -> "break"
| Instr           BrFalse -> "brfalse"
| Instr         BrFalse_s -> "brfalse.s"
| Instr            BrInst -> "brinst"
| Instr          BrInst_s -> "brinst.s"
| Instr            BrNull -> "brnull"
| Instr          BrNull_s -> "brnull.s"
| Instr            BrTrue -> "brtrue"
| Instr          BrTrue_s -> "brtrue.s"
| Instr            BrZero -> "brzero"
| Instr          BrZero_s -> "brzero.s"
| Instr              Bstr -> "bstr"
| Instr         ByteArray -> "bytearray"
| Instr          ByValStr -> "byvalstr"
| Instr              Call -> "call"
| Instr             CallI -> "calli"
| Instr   CallMostDerived -> "callmostderived"
| Instr          CallVirt -> "callvirt"
| Instr            CArray -> "carray"
| Instr         CastClass -> "castclass"
| Instr             Catch -> "catch"
| Instr             CDecl -> "cdecl"
| Instr               Ceq -> "ceq"
| Instr                Cf -> "cf"
| Instr               Cgt -> "cgt"
| Instr            Cgt_un -> "cgt.un"
| Instr              Char -> "char"
| Instr               CIL -> "cil"
| Instr          CKfinite -> "ckfinite"
| Instr             Class -> "class"
| Instr             CLSid -> "clsid"
| Instr               Clt -> "clt"
| Instr            Clt_un -> "clt.un"
| Instr             Const -> "const"
| Instr            Conv_i -> "conv.i"
| Instr           Conv_i1 -> "conv.i1"
| Instr           Conv_i2 -> "conv.i2"
| Instr           Conv_i4 -> "conv.i4"
| Instr           Conv_i8 -> "conv.i8"
| Instr        Conv_ovf_i -> "conv.ovf.i"
| Instr     Conv_ovf_i_un -> "conv.ovf.i.un"
| Instr       Conv_ovf_i1 -> "conv.ovf.i1"
| Instr    Conv_ovf_i1_un -> "conv.ovf.i1.un"
| Instr       Conv_ovf_i2 -> "conv.ovf.i2"
| Instr    Conv_ovf_i2_un -> "conv.ovf.i2.un"
| Instr       Conv_ovf_i4 -> "conv.ovf.i4"
| Instr    Conv_ovf_i4_un -> "conv.ovf.i4.un"
| Instr       Conv_ovf_i8 -> "conv.ovf.i8"
| Instr    Conv_ovf_i8_un -> "conv.ovf.i8.un"
| Instr        Conv_ovf_u -> "conv.ovf.u"
| Instr     Conv_ovf_u_un -> "conv.ovf.u.un"
| Instr       Conv_ovf_u1 -> "conv.ovf.u1"
| Instr    Conv_ovf_u1_un -> "conv.ovf.u1.un"
| Instr       Conv_ovf_u2 -> "conv.ovf.u2"
| Instr    Conv_ovf_u2_un -> "conv.ovf.u2.un"
| Instr       Conv_ovf_u4 -> "conv.ovf.u4"
| Instr    Conv_ovf_u4_un -> "conv.ovf.u4.un"
| Instr       Conv_ovf_u8 -> "conv.ovf.u8"
| Instr    Conv_ovf_u8_un -> "conv.ovf.u8.un"
| Instr         Conv_r_un -> "conv.r.un"
| Instr           Conv_r4 -> "conv.r4"
| Instr           Conv_r8 -> "conv.r8"
| Instr            Conv_u -> "conv.u"
| Instr           Conv_u1 -> "conv.u1"
| Instr           Conv_u2 -> "conv.u2"
| Instr           Conv_u4 -> "conv.u4"
| Instr           Conv_u8 -> "conv.u8"
| Instr             CpBlk -> "cpblk"
| Instr             CpObj -> "cpobj"
| Instr          Currency -> "currency"
| Instr            Custom -> "custom"
| Instr              Date -> "date"
| Instr           Decimal -> "decimal"
| Instr           Default -> "default"
| Instr            Demand -> "demand"
| Instr              Deny -> "deny"
| Instr               Div -> "div"
| Instr            Div_un -> "div_un"
| Instr               Dup -> "dup"
| Instr          EndFault -> "endfault"
| Instr         EndFilter -> "endfilter"
| Instr        EndFinally -> "endfinally"
| Instr            EndMac -> "endmac"
| Instr              Enum -> "enum"
| Instr             Error -> "error"
| Instr          Explicit -> "explicit"
| Instr           Extends -> "extends"
| Instr            Extern -> "extern"
| Instr             False -> "false"
| Instr       FamAndAssem -> "famandassem"
| Instr            Family -> "family"
| Instr        FamOrAssem -> "famorassem"
| Instr          FastCall -> "fastcall"
| Instr             Fault -> "fault"
| Instr             Field -> "field"
| Instr          FileTime -> "filetime"
| Instr            Filter -> "filter"
| Instr             Final -> "final"
| Instr           Finally -> "finally"
| Instr             Fixed -> "fixed"
| Instr             Float -> "float"
| Instr           Float32 -> "float32"
| Instr           Float64 -> "float64"
| Instr        ForwardRef -> "forwardref"
| Instr     FromUnmanaged -> "fromunmanaged"
| Instr           Handler -> "handler"
| Instr         HideBySig -> "hidebysig"
| Instr           HResult -> "hresult"
| Instr         IDispatch -> "idispatch"
| Instr                IL -> "il"
| Instr           Illegal -> "illegal"
| Instr        Implements -> "implements"
| Instr       ImplicitCom -> "implicitcom"
| Instr       ImplicitRes -> "implicitres"
| Instr            Import -> "import"
| Instr                In -> "in"
| Instr      InheritCheck -> "inheritcheck"
| Instr              Init -> "init"
| Instr           InitBlk -> "initblk"
| Instr           InitObj -> "initobj"
| Instr          InitOnly -> "initonly"
| Instr          Instance -> "instance"
| Instr               Int -> "int"
| Instr             Int16 -> "int16"
| Instr             Int32 -> "int32"
| Instr             Int64 -> "int64"
| Instr              Int8 -> "int8"
| Instr         Interface -> "interface"
| Instr      InternalCall -> "internalcall"
| Instr            ISinst -> "isinst"
| Instr          Iunknown -> "iunknown"
| Instr               Jmp -> "jmp"
| Instr           LastErr -> "lasterr"
| Instr              LCid -> "lcid"
| Instr             LdArg -> "ldarg"
| Instr           LdArg_0 -> "ldarg.0"
| Instr           LdArg_1 -> "ldarg.1"
| Instr           LdArg_2 -> "ldarg.2"
| Instr           LdArg_3 -> "ldarg.3"
| Instr           LdArg_s -> "ldarg.s"
| Instr            LdArgA -> "ldarga"
| Instr          LdArgA_s -> "ldarga.s"
| Instr            LdC_i4 -> "ldc.i4"
| Instr          LdC_i4_0 -> "ldc.i4.0"
| Instr          LdC_i4_1 -> "ldc.i4.1"
| Instr          LdC_i4_2 -> "ldc.i4.2"
| Instr          LdC_i4_3 -> "ldc.i4.3"
| Instr          LdC_i4_4 -> "ldc.i4.4"
| Instr          LdC_i4_5 -> "ldc.i4.5"
| Instr          LdC_i4_6 -> "ldc.i4.6"
| Instr          LdC_i4_7 -> "ldc.i4.7"
| Instr          LdC_i4_8 -> "ldc.i4.8"
| Instr         LdC_i4_M1 -> "ldc.i4.M1"
| Instr         LdC_i4_m1 -> "ldc.i4.m1"
| Instr          LdC_i4_s -> "ldc.i4.s"
| Instr            LdC_i8 -> "ldc.i8"
| Instr            LdC_r4 -> "ldc.r4"
| Instr            LdC_r8 -> "ldc.r8"
| Instr            LdElem -> "ldelem"
| Instr          LdElem_i -> "ldelem.i"
| Instr         LdElem_i1 -> "ldelem.i1"
| Instr         LdElem_i2 -> "ldelem.i2"
| Instr         LdElem_i4 -> "ldelem.i4"
| Instr         LdElem_i8 -> "ldelem.i8"
| Instr         LdElem_r4 -> "ldelem.r4"
| Instr         LdElem_r8 -> "ldelem.r8"
| Instr        LdElem_ref -> "ldelem.ref"
| Instr         LdElem_u1 -> "ldelem.u1"
| Instr         LdElem_u2 -> "ldelem.u2"
| Instr         LdElem_u4 -> "ldelem.u4"
| Instr         LdElem_u8 -> "ldelem.u8"
| Instr           LdElemA -> "ldelema"
| Instr             LdFld -> "ldfld"
| Instr            LdFldA -> "ldflda"
| Instr             LdFtn -> "ldftn"
| Instr           LdInd_i -> "ldind.i"
| Instr          LdInd_i1 -> "ldind.i1"
| Instr          LdInd_i2 -> "ldind.i2"
| Instr          LdInd_i4 -> "ldind.i4"
| Instr          LdInd_i8 -> "ldind.i8"
| Instr          LdInd_r4 -> "ldind.r4"
| Instr          LdInd_r8 -> "ldind.r8"
| Instr         LdInd_ref -> "ldind.ref"
| Instr          LdInd_u1 -> "ldind.u1"
| Instr          LdInd_u2 -> "ldind.u2"
| Instr          LdInd_u4 -> "ldind.u4"
| Instr          LdInd_u8 -> "ldind.u8"
| Instr             LdLen -> "ldlen"
| Instr             LdLoc -> "ldloc"
| Instr           LdLoc_0 -> "ldloc.0"
| Instr           LdLoc_1 -> "ldloc.1"
| Instr           LdLoc_2 -> "ldloc.2"
| Instr           LdLoc_3 -> "ldloc.3"
| Instr           LdLoc_s -> "ldloc.s"
| Instr            LdLocA -> "ldloca"
| Instr          LdLocA_s -> "ldloca.s"
| Instr            LdNull -> "ldnull"
| Instr             LdObj -> "ldobj"
| Instr            LdSFld -> "ldsfld"
| Instr           LdSFldA -> "ldsflda"
| Instr             LdStr -> "ldstr"
| Instr           LdToken -> "ldtoken"
| Instr         LdVirtFtn -> "ldvirtftn"
| Instr             Leave -> "leave"
| Instr           Leave_s -> "leave.s"
| Instr         LinkCheck -> "linkcheck"
| Instr           Literal -> "literal"
| Instr          LocalLoc -> "localloc"
| Instr             LpStr -> "lpstr"
| Instr          LpStruct -> "lpstruct"
| Instr            LpTStr -> "lptstr"
| Instr            LpVoid -> "lpvoid"
| Instr            LpWStr -> "lpwstr"
| Instr           Managed -> "managed"
| Instr           Marshal -> "marshal"
| Instr            Method -> "method"
| Instr          MkRefAny -> "mkrefany"
| Instr            ModOpt -> "modopt"
| Instr            ModReq -> "modreq"
| Instr               Mul -> "mul"
| Instr           Mul_ovf -> "mul.ovf"
| Instr        Mul_ovf_un -> "mul.ovf.un"
| Instr            Native -> "native"
| Instr               Neg -> "neg"
| Instr            Nested -> "nested"
| Instr            NewArr -> "newarr"
| Instr            NewObj -> "newobj"
| Instr           NewSlot -> "newslot"
| Instr       NoAppDomain -> "noappdomain"
| Instr        NoInlining -> "noinlining"
| Instr         NoMachine -> "nomachine"
| Instr          NoMangle -> "nomangle"
| Instr        NoMetadata -> "nometadata"
| Instr      NonCasDemand -> "noncasdemand"
| Instr NonCasInheritance -> "noncasinheritance"
| Instr  NonCasLinkDemand -> "noncaslinkdemand"
| Instr               Nop -> "nop"
| Instr         NoProcess -> "noprocess"
| Instr               Not -> "not"
| Instr    Not_in_GC_heap -> "not_in_gc_heap"
| Instr      NotRemotable -> "notremotable"
| Instr     NotSerialized -> "notserialized"
| Instr              Null -> "null"
| Instr           NullRef -> "nullref"
| Instr            Object -> "object"
| Instr         ObjectRef -> "objectref"
| Instr               Opt -> "opt"
| Instr             OptIL -> "optil"
| Instr                Or -> "or"
| Instr               Out -> "out"
| Instr        PermitOnly -> "permitonly"
| Instr            Pinned -> "pinned"
| Instr       PInvokeImpl -> "pinvokeimpl"
| Instr               Pop -> "pop"
| Instr           Prefix1 -> "prefix1"
| Instr           Prefix2 -> "prefix2"
| Instr           Prefix3 -> "prefix3"
| Instr           Prefix4 -> "prefix4"
| Instr           Prefix5 -> "prefix5"
| Instr           Prefix6 -> "prefix6"
| Instr           Prefix7 -> "prefix7"
| Instr         PrefixRef -> "prefixref"
| Instr        PreJITdeny -> "prejitdeny"
| Instr       PreJITgrant -> "prejitgrant"
| Instr       PreserveSig -> "preservesig"
| Instr           Private -> "private"
| Instr      PrivateScope -> "privatescope"
| Instr         Protected -> "protected"
| Instr            Public -> "public"
| Instr            Record -> "record"
| Instr            RefAny -> "refany"
| Instr        RefAnyType -> "refanytype"
| Instr         RefAnyVal -> "refanyval"
| Instr               Rem -> "rem"
| Instr            Rem_un -> "rem.un"
| Instr            ReqMin -> "reqmin"
| Instr            ReqOpt -> "reqopt"
| Instr         ReqRefuse -> "reqrefuse"
| Instr         ReqSecObj -> "reqsecobj"
| Instr           Request -> "request"
| Instr               Ret -> "ret"
| Instr           Rethrow -> "rethrow"
| Instr            RetVal -> "retval"
| Instr     RTspecialName -> "rtspecialname"
| Instr           Runtime -> "runtime"
| Instr         SafeArray -> "safearray"
| Instr            Sealed -> "sealed"
| Instr        Sequential -> "sequential"
| Instr      Serializable -> "serializable"
| Instr               ShL -> "shl"
| Instr               ShR -> "shr"
| Instr            ShR_un -> "shr.un"
| Instr            SizeOf -> "sizeof"
| Instr           Special -> "special"
| Instr       SpecialName -> "specialname"
| Instr             StArg -> "starg"
| Instr           StArg_s -> "starg.s"
| Instr            Static -> "static"
| Instr           StdCall -> "stdcall"
| Instr            StElem -> "stelem"
| Instr          StElem_i -> "stelem.i"
| Instr         StElem_i1 -> "stelem.i1"
| Instr         StElem_i2 -> "stelem.i2"
| Instr         StElem_i4 -> "stelem.i4"
| Instr         StElem_i8 -> "stelem.i8"
| Instr         StElem_r4 -> "stelem.r4"
| Instr         StElem_r8 -> "stelem.r8"
| Instr        StElem_ref -> "stelem.ref"
| Instr             StFld -> "stfld"
| Instr           StInd_i -> "stind.i"
| Instr          StInd_i1 -> "stind.i1"
| Instr          StInd_i2 -> "stind.i2"
| Instr          StInd_i4 -> "stind.i4"
| Instr          StInd_i8 -> "stind.i8"
| Instr          StInd_r4 -> "stind.r4"
| Instr          StInd_r8 -> "stind.r8"
| Instr         StInd_ref -> "stind.ref"
| Instr             StLoc -> "stloc"
| Instr           StLoc_0 -> "stloc.0"
| Instr           StLoc_1 -> "stloc.1"
| Instr           StLoc_2 -> "stloc.2"
| Instr           StLoc_3 -> "stloc.3"
| Instr           StLoc_s -> "stloc.s"
| Instr             StObj -> "stobj"
| Instr           Storage -> "storage"
| Instr     Stored_Object -> "stored_object"
| Instr            Stream -> "stream"
| Instr   Streamed_Object -> "streamed_object"
| Instr            String -> "string"
| Instr            Struct -> "struct"
| Instr            StSFld -> "stsfld"
| Instr               Sub -> "sub"
| Instr           Sub_ovf -> "sub.ovf"
| Instr        Sub_ovf_un -> "sub.ovf.un"
| Instr            Switch -> "switch"
| Instr      Synchronized -> "synchronized"
| Instr           SysChar -> "syschar"
| Instr         SysString -> "sysstring"
| Instr             TbStr -> "tbstr"
| Instr          ThisCall -> "thiscall"
| Instr             Throw -> "throw"
| Instr               TLS -> "tls"
| Instr                TO -> "to"
| Instr              True -> "true"
| Instr          TypedRef -> "typedref"
| Instr             UnBox -> "unbox"
| Instr         Unbox_Any -> "unbox.any"
| Instr           Unicode -> "unicode"
| Instr         Unmanaged -> "unmanaged"
| Instr      UnmanagedExp -> "unmanagedexp"
| Instr          Unsigned -> "unsigned"
| Instr            Unused -> "unused"
| Instr       UserDefined -> "userdefined"
| Instr             Value -> "value"
| Instr         ValueType -> "valuetype"
| Instr            VarArg -> "vararg"
| Instr           Variant -> "variant"
| Instr            Vector -> "vector"
| Instr           Virtual -> "virtual"
| Instr              Void -> "void"
| Instr             WChar -> "wchar"
| Instr            WInAPI -> "winapi"
| Instr              With -> "with"
| Instr           Wrapper -> "wrapper"
| Instr               Xor -> "xor"

(* Literals *)

|    Ident s -> "Ident(" ^ s ^ ")"
|      Nat s -> "Nat(" ^ s ^ ")"
|     Real s -> "Real(" ^ s ^ ")"
|  QString s -> "QString(" ^ s ^ ")"
| SQString s -> "SQString(" ^ s ^ ")"

(* Virtual tokens *)

| Space s -> "Space(" ^ s ^ ")"
|   EOL s -> "EOL(" ^ s ^ ")"
|     EOF -> "EOF"

}

(* Regular expressions for literals *)

(* White space *)

let newline = '\n' | '\r' | "\r\n"
let blank = ' ' | '\t'

(* Integers *)

let int_suf = ('u' | 'U') ('l' | 'L')?
            | ('u' | 'U') ("ll" | "LL")
            | ('l' | 'L') ('u' | 'U')?
            | ("ll" | "LL") ('u' | 'U')?
let dec = ['1'-'9'] ['0'-'9']*
let hex = ("0x" | "0X") ['0'-'9' 'A'-'F' 'a'-'f']+
let oct = '0' ['0'-'7']*
let nat = (dec | hex | oct) int_suf?

(* Identifiers *)

let digit = ['0'-'9']
let nondigit = ['_' 'a'-'z' 'A'-'Z']
let ident = nondigit (nondigit | digit)*

(* Float *)

let dig = ['0'-'9']
let exp = ['e' 'E'] ['+' '-']? dig+
let frac = dig* '.' dig+ | dig+ '.'
let real_suf = ['F' 'f' 'L' 'l']
let real = frac exp? real_suf?

(* Strings *)

let esc =
  "\\n" | "\\\"" | "\\?" | "\\\\"
| "\\a" | "\\b" | "\\f" | "\\n" | "\\r" | "\\t" | "\\v"

let qs_char = [^'"' '\\' '\n'] | esc
let sqs_char = [^'\'' '\\' '\n'] | esc
let qstring = '"' qs_char* '"'
let sqstring = "'" sqs_char* "'"

(* Rules *)

rule token = parse
  newline as s { handle_nl lexbuf; Parser.EOL s }
| blank+ as s  { Parser.Space s }
| eof          { Parser.EOF     }
| '{'          { Parser.LBrace  }
| '}'          { Parser.RBrace  }
| '+'          { Parser.Plus    }
| ','          { Parser.Comma   }
| '='          { Parser.Eq      }
| '('          { Parser.LPar    }
| ')'          { Parser.RPar    }
| '['          { Parser.LBrack  }
| ']'          { Parser.RBrack  }
| "::"         { Parser.DColon  }
| '*'          { Parser.Times   }
| '&'          { Parser.Amper   }
| "..."        { Parser.Ellipsis }
| '/'          { Parser.Slash   }
| '!'          { Parser.Bang    }
| ':'          { Parser.Colon   }
| "#line"      { Parser.Meta Lexis.SharpLine }

| nat as s      { Parser.Nat s      }
| real as s     { Parser.Real s     }
| qstring as s  { Parser.QString s  }
| sqstring as s { Parser.SQString s }
| "//"          { handle_err in_line_com lexbuf; token lexbuf }

| '.' (ident as m) { let open Parser in
                     try Meta (StringMap.find m metadata_map) with 
                       Not_found -> fail "Invalid metadata." lexbuf
                   }
| (ident as p) '.' { let open Parser in
                     try Prefix (StringMap.find p prefix_map) with
                       Not_found -> fail "Invalid prefix." lexbuf
                   }
| ident as i       { let open Parser in
                     try Instr (StringMap.find i instr_map) with
                       Not_found -> fail "Invalid instruction." lexbuf
                   }
| _ { fail "Invalid character." lexbuf }

(* Comments *)

and in_line_com = parse
  newline { handle_nl lexbuf }
| eof     { }
| _       { in_line_com lexbuf }

{

let get_token buffer =
  let open Parser in
match token buffer with
  Space _
|   EOL _ -> token buffer
|       t -> t

(* Standalone lexer for debugging purposes. *)

type filename = string

let trace ~(input: filename) ~(output: filename) : unit =
  match open_in input, open_out output with
    cin, cout ->
      let buffer = Lexing.from_channel cin in
      let rec iter () =
        match token buffer with
          Parser.EOF -> close_in cin; close_out cout
        |          t -> begin
                          output_string cout (string_of_token t);
                          output_string cout "\n";
                          flush cout;
                          iter ()
                        end
        | exception Error.Lexer diag -> 
            close_in cin; close_out cout; Error.print "Lexical" diag
      in iter ()
  | exception Sys_error msg -> prerr_endline msg

}
