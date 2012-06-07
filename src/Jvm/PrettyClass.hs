{-
PrettyClass
    Print the class format


Modifications:

Wed May 26 10:24:58 BOT 2010
    Added support for program counter in the attribute code, now all code is well enumerated.
-}

module Jvm.PrettyClass where
import Jvm.Data.ClassFormat

import UU.Pretty
import Data.Bits

instance PP ClassFile where
    pp cf =  pp (magic cf) 
         >-< pp (minver cf) 
         >-< pp (maxver cf)
         >-< text "Constant_Pool ->" >-< pp_brackets (foldr (\cp -> (>-<) (pp cp)) empty (array_cp cf))
         >-< text "AccessFlag ->" >#< pp (acfg cf)
         >-< pp (this cf)
         >-< pp (super cf)
         >-< pAttr "length_interfaces" (pp (count_interfaces cf))
         >-< text "Interfaces ->" >-< pp_brackets (foldr (\cp -> (>-<) (pp cp)) empty (array_interfaces cf))
         >-< pAttr "length_fields" (pp (count_fields cf))
         >-< text "Fields ->" >-< pp_brackets (foldr (\cp -> (>-<) (pp cp)) empty (array_fields cf))
         >-< pAttr "length_methods" (pp (count_methods cf))
         >-< text "Methods ->" >-< pp_brackets (foldr (\cp -> (>-<) (pp cp)) empty (array_methods cf))
         >-< pAttr "length_attributes" (pp (count_attributes cf))
         >-< text "Attributes ->" >-< pp_brackets (foldr (\cp -> (>-<) (pp cp)) empty (array_attributes cf))

pAttr name pvalue = text name >#< text "=" >#< pvalue

instance PP Magic where
    pp mg = text "Magic -> " >|< text "0xcafe 0xbabe"

instance PP MinorVersion where
    pp mv = text "Minor_Version -> " >|< pp (numMinVer mv)

instance PP MajorVersion where
    pp mv = text "Major_Version -> " >|< pp (numMaxVer mv)

instance PP CP_Info where
    pp (Class_Info tag index _)
        = let ptag = pAttr "Tag" (pp tag)
              picp = pAttr "index_cp" (pp index)
          in indent 3 (pp_block "{" "}" ", " [ptag, picp])
    
    pp (FieldRef_Info tag ind1 ind2 _)
        = let ptag  = pAttr "Tag" (pp tag)
              picp1 = pAttr "index_cp" (pp ind1)
              picp2 = pAttr "index_NameAndType_cp" (pp ind2)
          in indent 3 (pp_block "{" "}" ", " [ptag, picp1, picp2])
    
    pp (MethodRef_Info tag ind1 ind2 _)
        = let ptag  = pAttr "Tag" (pp tag)
              picp1 = pAttr "index_cp" (pp ind1)
              picp2 = pAttr "index_NameAndType_cp" (pp ind2)
          in indent 3 (pp_block "{" "}" ", " [ptag, picp1, picp2])

    pp (InterfaceMethodRef_Info tag ind1 ind2 _)
        = let ptag  = pAttr "Tag" (pp tag)
              picp1 = pAttr "index_cp" (pp ind1)
              picp2 = pAttr "index_NameAndType_cp" (pp ind2)
          in indent 3 (pp_block "{" "}" ", " [ptag, picp1, picp2])

    pp (String_Info tag index _)
        = let ptag = pAttr "Tag" (pp tag)
              picp = pAttr "index_cp" (pp index)
          in indent 3 (pp_block "{" "}" ", " [ptag, picp])

    pp (Float_Info tag float _)
        = let ptag = pAttr "Tag" (pp tag)
              pfloat = pAttr "Value" (pp float)
          in indent 3 (pp_block "{" "}" ", " [ptag, pfloat])

    pp (Long_Info tag int1 int2 _)
        = let ptag = pAttr "Tag" (pp tag)
              pint1 = pAttr "Value_1" (pp int1)
              pint2 = pAttr "Value_2" (pp int2)
          in indent 3 (pp_block "{" "}" ", " [ptag, pint1, pint2])

    pp (Double_Info tag int1 int2 _)
        = let ptag = pAttr "Tag" (pp tag)
              pint1 = pAttr "Value_1" (pp int1)
              pint2 = pAttr "Value_2" (pp int2)
          in indent 3 (pp_block "{" "}" ", " [ptag, pint1, pint2])

    pp (NameAndType_Info tag ind1 ind2 _)
        = let ptag  = pAttr "Tag" (pp tag)
              picp1 = pAttr "index_cp" (pp ind1)
              picp2 = pAttr "index_desc_cp" (pp ind2)
          in indent 3 (pp_block "{" "}" ", " [ptag, picp1, picp2])

    pp (Utf8_Info tag tam str _)
        = let ptag = pAttr "Tag" (pp tag)
              plen = pAttr "length" (pp tam)
              pstr = pAttr "Value" (text str)
          in indent 3 (pp_block "{" "}" ", " [ptag, plen, pstr])

instance PP Tag where
    pp (TagClass)               = text "Class"
    pp (TagFieldRef)            = text "FieldRef"
    pp (TagMethodRef)           = text "MethodRef"
    pp (TagInterfaceMethodRef)  = text "InterfaceMethodRef"
    pp (TagString)              = text "String"
    pp (TagInteger)             = text "Integer"
    pp (TagFloat)               = text "Float"
    pp (TagLong)                = text "Long"
    pp (TagDouble)              = text "Double"
    pp (TagNameAndType)         = text "NameAndType"
    pp (TagUtf8)                = text "Utf8"

instance PP AccessFlags where
    pp (AccessFlags lst) = hlist $ map showFlag lst
     where showFlag fg
            | fg == acc_Public     = text "Public"
            | fg == acc_Private    = text "Private"
            | fg == acc_Protected  = text "Protected"
            | fg == acc_Static     = text "Static"
            | fg == acc_Final      = text "Final"
            | fg == acc_Super_Synchronized = text "Super"
            | fg == acc_Volatile_Bridge    = text "Volatile"
            | fg == acc_Transient_Varargs  = text "Transient"
            | fg == acc_Native     = text "Native"
            | fg == acc_Interface  = text "Interface"
            | fg == acc_Abstract   = text "Abstract"
            | fg == acc_Strict     = text "Strict"
            | fg == acc_Synthetic  = text "Synthetic"
            | fg == acc_Enum       = text "Enum"
            | otherwise            = text "No Spec"

instance PP ThisClass where
    pp tc = text "ThisClass_index_cp -> " >|< pp (index_th tc)

instance PP SuperClass where
    pp tc = text "SuperClass_index_cp -> " >|< pp (index_sp tc)

instance PP Interface where
    pp iif = text "Interface_index_cp -> " >|< pp (index_if iif)

instance PP Field_Info where 
    pp fdi = let fg = pp (af_fi fdi)
                 ind1 = pAttr "index_name_cp" (pp (index_name_fi fdi))
                 ind2 = pAttr "index_desc_cp" (pp (index_descr_fi fdi))
                 tam  = pAttr "length" (pp (tam_fi fdi))
                 lst  = text "Constant_Pool ->" >-< pp_brackets (foldr (\at -> (>-<) (pp at)) empty (array_attr_fi fdi))
             in text "Field_Info" >-< pp_block "{" "}" ", " [fg, ind1, ind2, tam, lst]

instance PP Method_Info where 
    pp mth = let fg   = pp_mth (af_mi mth)
                 ind1 = pAttr "index_name_cp" (pp (index_name_mi mth))
                 ind2 = pAttr "index_desc_cp" (pp (index_descr_mi mth))
                 tam  = pAttr "length_Attr" (pp (tam_mi mth))
                 lst  = text "Attributes ->" >-< pp_brackets (foldr (\at -> (>-<) (pp at)) empty (array_attr_mi mth))
             in text "Method_Info" >-< pp_block "{" "}" ", " [fg, ind1, ind2, tam, lst]
     where pp_mth (AccessFlags lst) = hlist $ map showFlag lst
           showFlag fg
            | fg == acc_Public     = text "Public"
            | fg == acc_Private    = text "Private"
            | fg == acc_Protected  = text "Protected"
            | fg == acc_Static     = text "Static"
            | fg == acc_Final      = text "Final"
            | fg == acc_Super_Synchronized = text "Synchronized"
            | fg == acc_Volatile_Bridge    = text "Bridge"
            | fg == acc_Transient_Varargs  = text "Varargs"
            | fg == acc_Native     = text "Native"
            | fg == acc_Interface  = text "Interface"
            | fg == acc_Abstract   = text "Abstract"
            | fg == acc_Strict     = text "Strict"
            | fg == acc_Synthetic  = text "Synthetic"
            | fg == acc_Enum       = text "Enum"
            | otherwise            = text "No Spec" 

instance PP Attribute_Info where
    pp (AttributeGeneric inam tam rest)
        = let pinam = pAttr "index_name" (pp inam)
              plen  = pAttr "length" (pp tam)
              prest = pAttr "Rest" (text (show rest))
          in indent 3 (text "AttributeGeneric" >-< pp_block "{" "}" ", " [pinam, plen, prest])
    
    pp (AttributeConstantValue inam tam ivalue)
        = let pinam   = pAttr "index_name" (pp inam)
              plen    = pAttr "length" (pp tam)
              pivalue = pAttr "index_value" (pp ivalue)
          in indent 3 (text "AttributeConstantValue" >-< pp_block "{" "}" ", " [pinam, plen, pivalue])

    pp (AttributeCode inam tama lens lenl tamc lstc tame lste tamat lstat)
        = let pinam  = pAttr "index_name" (pp inam)
              plen1  = pAttr "length" (pp tama)
              plen2  = pAttr "length_stack" (pp lens)
              plen3  = pAttr "length_local" (pp lenl)
              plen4  = pAttr "length_code" (pp tamc)
              --listc  = indent 2 (vlist (map (text ">" >#<) (instruction2pp lstc))) --c) lstc))
              listc  = indent 2 $ vlist $ instruction2pp lstc 0
              plen5  = pAttr "length_exeptions" (pp tame)
              liste  = pp_brackets (foldr (>-<) empty (map (\(a,b,c,d) -> indent 2 (pp_block "{" "}" ", " [pAttr "start_counter" (pp a),pAttr "end_counter" (pp b),pAttr "handler_counter" (pp c),pAttr "catch_type" (pp d)])) lste))
              plen6  = pAttr "length_attributes" (pp tamat)
              listat = text "Attributes ->" >-< pp_brackets (foldr (\at -> (>-<) (pp at)) empty lstat)
          in indent 3 (text "AttributeCode" >-< pp_block "{" "}" ", " [pinam, plen1, plen2, plen3, plen4, listc, plen5, liste, plen6, listat])

    pp (AttributeSourceFile inam tam idesc)
        = let pinam = pAttr "index_name" (pp inam)
              ptam  = pAttr "length" (pp tam)
              pidesc = pAttr "index_desc" (pp idesc)
          in indent 3 (text "AttributeSourceFile" >-< pp_block "{" "}" ", " [pinam, ptam, pidesc])
    
    pp (AttributeLineNumberTable inam tam tamln lstln)
        = let pinam = pAttr "index_name" (pp inam)
              ptam = pAttr "length" (pp tam)
              ptamln = pAttr "length_linenumber" (pp tamln)
              listln = foldr (>-<) empty (map (\(a,b) -> indent 2 (pp_block "{" "}" ", " [pAttr "start_counter" (pp a),pAttr "line_number" (pp b)])) lstln)
          in indent 3 (text "AttributeLineNumberTable" >-< pp_block "{" "}" ", " [pinam, ptam, ptamln, listln])

    pp (AttributeLocalVariableTable inam tam tamlv lstlv)
        = let pinam = pAttr "index_name" (pp inam)
              ptam = pAttr "length" (pp tam)
              ptamlv = pAttr "length_linenumber" (pp tamlv)
              listln = foldr (>-<) empty (map (\(a,b,c,d,e) -> indent 2 (pp_block "{" "}" ", " [pAttr "start_counter" (pp a),pAttr "length" (pp b),pAttr "name_index" (pp c),pAttr "desc_index" (pp d), pAttr "index" (pp e)])) lstlv)
          in indent 3 (text "AttributeLocalVariableTable" >-< pp_block "{" "}" ", " [pinam, ptam, ptamlv, listln])

instruction2pp :: [Int] -> Int -> [PP_Doc]
instruction2pp [] counter = []
instruction2pp (inst:xs) counter = 
    case inst of
        0   -> (pp counter >#< text "nop"):instruction2pp xs (counter + 1)
        1   -> (pp counter >#< text "aconst_null"):instruction2pp xs (counter + 1)
        2   -> (pp counter >#< text "iconst_m1"):instruction2pp xs (counter + 1)
        3   -> (pp counter >#< text "iconst_0"):instruction2pp xs (counter + 1)
        4   -> (pp counter >#< text "iconst_1"):instruction2pp xs (counter + 1)
        5   -> (pp counter >#< text "iconst_2"):instruction2pp xs (counter + 1)
        6   -> (pp counter >#< text "iconst_3"):instruction2pp xs (counter + 1)
        7   -> (pp counter >#< text "iconst_4"):instruction2pp xs (counter + 1)
        8   -> (pp counter >#< text "iconst_5"):instruction2pp xs (counter + 1)
        9   -> (pp counter >#< text "lconst_0"):instruction2pp xs (counter + 1)
        10  -> (pp counter >#< text "lconst_1"):instruction2pp xs (counter + 1)
        11  -> (pp counter >#< text "fconst_0"):instruction2pp xs (counter + 1)
        12  -> (pp counter >#< text "fconst_1"):instruction2pp xs (counter + 1)
        13  -> (pp counter >#< text "fconst_2"):instruction2pp xs (counter + 1)
        14  -> (pp counter >#< text "dconst_0"):instruction2pp xs (counter + 1)
        15  -> (pp counter >#< text "dconst_1"):instruction2pp xs (counter + 1)
        16  -> (pp counter >#< text "bipush" >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        17  -> (pp counter >#< text "sipush" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        18  -> (pp counter >#< text "ldc"    >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        19  -> (pp counter >#< text "ldc_w"  >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        20  -> (pp counter >#< text "ldc2_w" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        21  -> (pp counter >#< text "iload"  >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        22  -> (pp counter >#< text "lload"  >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        23  -> (pp counter >#< text "fload"  >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        24  -> (pp counter >#< text "dload"  >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        25  -> (pp counter >#< text "aload"  >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        26  -> (pp counter >#< text "iload_0"):instruction2pp xs (counter + 1)
        27  -> (pp counter >#< text "iload_1"):instruction2pp xs (counter + 1)
        28  -> (pp counter >#< text "iload_2"):instruction2pp xs (counter + 1)
        29  -> (pp counter >#< text "iload_3"):instruction2pp xs (counter + 1)
        30  -> (pp counter >#< text "lload_0"):instruction2pp xs (counter + 1)
        31  -> (pp counter >#< text "lload_1"):instruction2pp xs (counter + 1)
        32  -> (pp counter >#< text "lload_2"):instruction2pp xs (counter + 1)
        33  -> (pp counter >#< text "lload_3"):instruction2pp xs (counter + 1)
        34  -> (pp counter >#< text "fload_0"):instruction2pp xs (counter + 1)
        35  -> (pp counter >#< text "fload_1"):instruction2pp xs (counter + 1)
        36  -> (pp counter >#< text "fload_2"):instruction2pp xs (counter + 1)
        37  -> (pp counter >#< text "fload_3"):instruction2pp xs (counter + 1)
        38  -> (pp counter >#< text "dload_0"):instruction2pp xs (counter + 1)
        39  -> (pp counter >#< text "dload_1"):instruction2pp xs (counter + 1)
        40  -> (pp counter >#< text "dload_2"):instruction2pp xs (counter + 1)
        41  -> (pp counter >#< text "dload_3"):instruction2pp xs (counter + 1)
        42  -> (pp counter >#< text "aload_0"):instruction2pp xs (counter + 1)
        43  -> (pp counter >#< text "aload_1"):instruction2pp xs (counter + 1)
        44  -> (pp counter >#< text "aload_2"):instruction2pp xs (counter + 1)
        45  -> (pp counter >#< text "aload_3"):instruction2pp xs (counter + 1)
        46  -> (pp counter >#< text "iaload"):instruction2pp xs (counter + 1)
        47  -> (pp counter >#< text "laload"):instruction2pp xs (counter + 1)
        48  -> (pp counter >#< text "faload"):instruction2pp xs (counter + 1)
        49  -> (pp counter >#< text "daload"):instruction2pp xs (counter + 1)
        50  -> (pp counter >#< text "aaload"):instruction2pp xs (counter + 1)
        51  -> (pp counter >#< text "baload"):instruction2pp xs (counter + 1)
        52  -> (pp counter >#< text "caload"):instruction2pp xs (counter + 1)
        53  -> (pp counter >#< text "saload"):instruction2pp xs (counter + 1)
        54  -> (pp counter >#< text "istore" >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        55  -> (pp counter >#< text "lstore" >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        56  -> (pp counter >#< text "fstore" >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        57  -> (pp counter >#< text "dstore" >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        58  -> (pp counter >#< text "astore" >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        59  -> (pp counter >#< text "istore_0"):instruction2pp xs (counter + 1)
        60  -> (pp counter >#< text "istore_1"):instruction2pp xs (counter + 1)
        61  -> (pp counter >#< text "istore_2"):instruction2pp xs (counter + 1)
        62  -> (pp counter >#< text "istore_3"):instruction2pp xs (counter + 1)
        63  -> (pp counter >#< text "lstore_0"):instruction2pp xs (counter + 1)
        64  -> (pp counter >#< text "lstore_1"):instruction2pp xs (counter + 1)
        65  -> (pp counter >#< text "lstore_2"):instruction2pp xs (counter + 1)
        66  -> (pp counter >#< text "lstore_3"):instruction2pp xs (counter + 1)
        67  -> (pp counter >#< text "fstore_0"):instruction2pp xs (counter + 1)
        68  -> (pp counter >#< text "fstore_1"):instruction2pp xs (counter + 1)
        69  -> (pp counter >#< text "fstore_2"):instruction2pp xs (counter + 1)
        70  -> (pp counter >#< text "fstore_3"):instruction2pp xs (counter + 1)
        71  -> (pp counter >#< text "dstore_0"):instruction2pp xs (counter + 1)
        72  -> (pp counter >#< text "dstore_1"):instruction2pp xs (counter + 1)
        73  -> (pp counter >#< text "dstore_2"):instruction2pp xs (counter + 1)
        74  -> (pp counter >#< text "dstore_3"):instruction2pp xs (counter + 1)
        75  -> (pp counter >#< text "astore_0"):instruction2pp xs (counter + 1)
        76  -> (pp counter >#< text "astore_1"):instruction2pp xs (counter + 1)
        77  -> (pp counter >#< text "astore_2"):instruction2pp xs (counter + 1)
        78  -> (pp counter >#< text "astore_3"):instruction2pp xs (counter + 1)
        79  -> (pp counter >#< text "iastore"):instruction2pp xs (counter + 1)
        80  -> (pp counter >#< text "lastore"):instruction2pp xs (counter + 1)
        81  -> (pp counter >#< text "fastore"):instruction2pp xs (counter + 1)
        82  -> (pp counter >#< text "dastore"):instruction2pp xs (counter + 1)
        83  -> (pp counter >#< text "aastore"):instruction2pp xs (counter + 1)
        84  -> (pp counter >#< text "bastore"):instruction2pp xs (counter + 1)
        85  -> (pp counter >#< text "castore"):instruction2pp xs (counter + 1)
        86  -> (pp counter >#< text "sastore"):instruction2pp xs (counter + 1)
        87  -> (pp counter >#< text "pop"):instruction2pp xs (counter + 1)
        88  -> (pp counter >#< text "pop2"):instruction2pp xs (counter + 1)
        89  -> (pp counter >#< text "dup"):instruction2pp xs (counter + 1)
        90  -> (pp counter >#< text "dup_x1"):instruction2pp xs (counter + 1)
        91  -> (pp counter >#< text "dup_x2"):instruction2pp xs (counter + 1)
        92  -> (pp counter >#< text "dup2"):instruction2pp xs (counter + 1)
        93  -> (pp counter >#< text "dup2_x1"):instruction2pp xs (counter + 1)
        94  -> (pp counter >#< text "dup2_x2"):instruction2pp xs (counter + 1)
        95  -> (pp counter >#< text "swap"):instruction2pp xs (counter + 1)
        96  -> (pp counter >#< text "iadd"):instruction2pp xs (counter + 1)
        97  -> (pp counter >#< text "ladd"):instruction2pp xs (counter + 1)
        98  -> (pp counter >#< text "fadd"):instruction2pp xs (counter + 1)
        99  -> (pp counter >#< text "dadd"):instruction2pp xs (counter + 1)
        100 -> (pp counter >#< text "isub"):instruction2pp xs (counter + 1)
        101 -> (pp counter >#< text "lsub"):instruction2pp xs (counter + 1)
        102 -> (pp counter >#< text "fsub"):instruction2pp xs (counter + 1)
        103 -> (pp counter >#< text "dsub"):instruction2pp xs (counter + 1)
        104 -> (pp counter >#< text "imul"):instruction2pp xs (counter + 1)
        105 -> (pp counter >#< text "lmul"):instruction2pp xs (counter + 1)
        106 -> (pp counter >#< text "fmul"):instruction2pp xs (counter + 1)
        107 -> (pp counter >#< text "dmul"):instruction2pp xs (counter + 1)
        108 -> (pp counter >#< text "idiv"):instruction2pp xs (counter + 1)
        109 -> (pp counter >#< text "ldiv"):instruction2pp xs (counter + 1)
        110 -> (pp counter >#< text "fdiv"):instruction2pp xs (counter + 1)
        111 -> (pp counter >#< text "ddiv"):instruction2pp xs (counter + 1)
        112 -> (pp counter >#< text "irem"):instruction2pp xs (counter + 1)
        113 -> (pp counter >#< text "lrem"):instruction2pp xs (counter + 1)
        114 -> (pp counter >#< text "frem"):instruction2pp xs (counter + 1)
        115 -> (pp counter >#< text "drem"):instruction2pp xs (counter + 1)
        116 -> (pp counter >#< text "ineg"):instruction2pp xs (counter + 1)
        117 -> (pp counter >#< text "lneg"):instruction2pp xs (counter + 1)
        118 -> (pp counter >#< text "fneg"):instruction2pp xs (counter + 1)
        119 -> (pp counter >#< text "dneg"):instruction2pp xs (counter + 1)
        120 -> (pp counter >#< text "ishl"):instruction2pp xs (counter + 1)
        121 -> (pp counter >#< text "lshl"):instruction2pp xs (counter + 1)
        122 -> (pp counter >#< text "ishr"):instruction2pp xs (counter + 1)
        123 -> (pp counter >#< text "lshr"):instruction2pp xs (counter + 1)
        124 -> (pp counter >#< text "iushr"):instruction2pp xs (counter + 1)
        125 -> (pp counter >#< text "lushr"):instruction2pp xs (counter + 1)
        126 -> (pp counter >#< text "iand"):instruction2pp xs (counter + 1)
        127 -> (pp counter >#< text "land"):instruction2pp xs (counter + 1)
        128 -> (pp counter >#< text "ior"):instruction2pp xs (counter + 1)
        129 -> (pp counter >#< text "lor"):instruction2pp xs (counter + 1)
        130 -> (pp counter >#< text "ixor"):instruction2pp xs (counter + 1)
        131 -> (pp counter >#< text "lxor"):instruction2pp xs (counter + 1)
        132 -> (pp counter >#< text "iinc" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        133 -> (pp counter >#< text "i2l"):instruction2pp xs (counter + 1)
        134 -> (pp counter >#< text "i2f"):instruction2pp xs (counter + 1)
        135 -> (pp counter >#< text "i2d"):instruction2pp xs (counter + 1)
        136 -> (pp counter >#< text "l2i"):instruction2pp xs (counter + 1)
        137 -> (pp counter >#< text "l2f"):instruction2pp xs (counter + 1)
        138 -> (pp counter >#< text "l2d"):instruction2pp xs (counter + 1)
        139 -> (pp counter >#< text "f2i"):instruction2pp xs (counter + 1)
        140 -> (pp counter >#< text "f2l"):instruction2pp xs (counter + 1)
        141 -> (pp counter >#< text "f2d"):instruction2pp xs (counter + 1)
        142 -> (pp counter >#< text "d2i"):instruction2pp xs (counter + 1)
        143 -> (pp counter >#< text "d2l"):instruction2pp xs (counter + 1)
        144 -> (pp counter >#< text "d2f"):instruction2pp xs (counter + 1)
        145 -> (pp counter >#< text "i2b"):instruction2pp xs (counter + 1)
        146 -> (pp counter >#< text "i2c"):instruction2pp xs (counter + 1)
        147 -> (pp counter >#< text "i2s"):instruction2pp xs (counter + 1)
        148 -> (pp counter >#< text "lcmp"):instruction2pp xs (counter + 1)
        149 -> (pp counter >#< text "fcmpl"):instruction2pp xs (counter + 1)
        150 -> (pp counter >#< text "fcmpg"):instruction2pp xs (counter + 1)
        151 -> (pp counter >#< text "dcmpl"):instruction2pp xs (counter + 1)
        152 -> (pp counter >#< text "dcmpg"):instruction2pp xs (counter + 1)
        153 -> (pp counter >#< text "ifeq" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        154 -> (pp counter >#< text "ifne" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        155 -> (pp counter >#< text "iflt" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        156 -> (pp counter >#< text "ifge" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        157 -> (pp counter >#< text "ifgt" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        158 -> (pp counter >#< text "ifle" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        159 -> (pp counter >#< text "if_icmpeq" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        160 -> (pp counter >#< text "if_icmpne" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        161 -> (pp counter >#< text "if_icmplt" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        162 -> (pp counter >#< text "if_icmpge" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        163 -> (pp counter >#< text "if_icmpgt" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        164 -> (pp counter >#< text "if_icmple" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        165 -> (pp counter >#< text "if_acmpeq" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        166 -> (pp counter >#< text "if_acmpne" >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        167 -> (pp counter >#< text "goto"      >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        168 -> (pp counter >#< text "jsr"       >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        169 -> (pp counter >#< text "ret"       >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        170 -> let npad         = (counter + 1) `mod` 4
                   (bpad,  rs1) = splitAt npad xs
                   (defpos,rs2) = (fromBytes2Int $ take 4 rs1, drop 4 rs1)
                   (low,   rs3) = (fromBytes2Int $ take 4 rs1, drop 4 rs2)
                   (hight, rs4) = (fromBytes2Int $ take 4 rs1, drop 4 rs3)
                   (lstbr, rs5) = let n = 4*(hight-low+1) in (toInts (take n rs4), drop n rs4)
               in (pp counter >#< text "tableswitch" >#< text "[ hight :" >#< pp hight >#< text "low :" >#< pp low >#< text " ]" >-<
                  vlist ((map (\br -> text "->" >#< pp br) lstbr) ++ [text "-> default :" >#< pp defpos])) : instruction2pp rs5 (counter + npad + 12 + (hight-low+1)*4)
        171 -> let npad         = (counter+1) `mod` 4
                   (bpad,  rs1) = splitAt npad xs
                   (defpos,rs2) = (fromBytes2Int $ take 4 rs1, drop 4 rs1)
                   (npairs,rs3) = (fromBytes2Int $ take 4 rs2, drop 4 rs2)
                   (lstpairs, rs4) = (fromBytes2Tupla $ take (npairs*2*4) rs3, drop (npairs*2*4) rs3)
               in (pp counter >#< text "lookupswitch" >#< text "[ pad :" >#< pp npad >#< text ", ntable :" >#< pp npairs >#< text " ]" >-<
                  vlist ((map (\(n1,n2) -> pp n1 >#< text "->" >#< pp n2) lstpairs) ++ [text "default ->" >#< pp defpos])) : instruction2pp rs4 (counter + npad + 8 + npairs*2*4)
        172 -> (pp counter >#< text "ireturn"):instruction2pp xs (counter + 1)
        173 -> (pp counter >#< text "lreturn"):instruction2pp xs (counter + 1)
        174 -> (pp counter >#< text "freturn"):instruction2pp xs (counter + 1)
        175 -> (pp counter >#< text "dreturn"):instruction2pp xs (counter + 1)
        176 -> (pp counter >#< text "areturn"):instruction2pp xs (counter + 1)
        177 -> (pp counter >#< text "return"):instruction2pp xs (counter + 1)
        178 -> (pp counter >#< text "getstatic" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        179 -> (pp counter >#< text "putstatic" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        180 -> (pp counter >#< text "getfield" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        181 -> (pp counter >#< text "putfield" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        182 -> (pp counter >#< text "invokevirtual" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        183 -> (pp counter >#< text "invokespecial" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        184 -> (pp counter >#< text "invokestatic" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        185 -> (pp counter >#< text "invokeinterface" >#< let [b1,b2,b3,b4] = take 4 xs in pp b1 >#< pp b2 >#< pp b3 >#< pp b4):instruction2pp (drop 4 xs) (counter + 5)
        --186 (0xba)   xxxunusedxxx1
        187 -> (pp counter >#< text "new" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        188 -> (pp counter >#< text "newarray" >#< pp (take 1 xs)):instruction2pp (drop 1 xs) (counter + 2)
        189 -> (pp counter >#< text "anewarray" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        190 -> (pp counter >#< text "arraylength"):instruction2pp xs (counter + 1)
        191 -> (pp counter >#< text "athrow"):instruction2pp xs (counter + 1)
        192 -> (pp counter >#< text "checkcast" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        193 -> (pp counter >#< text "instanceof" >#< let [b1,b2] = take 2 xs in pp b1 >#< pp b2):instruction2pp (drop 2 xs) (counter + 3)
        194 -> (pp counter >#< text "monitorenter"):instruction2pp xs (counter + 1)
        195 -> (pp counter >#< text "monitorexit"):instruction2pp xs (counter + 1)
        196 -> let (vinstr, rs1) = (head xs, tail xs)
                   (vindex, rs2) = (fromBytes2Int (take 2 rs1), drop 2 rs1)
               in if (vinstr == 132) -- 132 == iinc
                  then let (vconst, rs3) = (fromBytes2Int (take 2 rs2), drop 2 rs2)
                       in (pp counter >#< text "wide" >#< toPP vinstr >#< text "index :" >#< pp vindex >#< text "const :" >#< pp vconst) : instruction2pp rs3 (counter + 6)
                  else (pp counter >#< text "wide"   >#< toPP vinstr >#< text "index :" >#< pp vindex) : instruction2pp rs2 (counter + 4)
        197 -> (pp counter >#< text "multianewarray" >#< let [b1,b2,b3] = take 3 xs in pp b1 >#< pp b2 >#< pp b3):instruction2pp (drop 3 xs) (counter + 4)
        198 -> (pp counter >#< text "ifnull"         >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        199 -> (pp counter >#< text "ifnonnull"      >#< let n = fromBytes2Int (take 2 xs) in pp (getSignedInt (n+counter) 17)):instruction2pp (drop 2 xs) (counter + 3)
        200 -> (pp counter >#< text "goto_w"         >#< let n = fromBytes2Int (take 4 xs) in pp (getSignedInt (n+counter) 33)):instruction2pp (drop 4 xs) (counter + 5)
        201 -> (pp counter >#< text "jsr_w"          >#< let n = fromBytes2Int (take 4 xs) in pp (getSignedInt (n+counter) 33)):instruction2pp (drop 4 xs) (counter + 5)
        --Reserved ocounterodes:
        --202 (0xca)   breakpoint
        --254 (0xfe)   impdep1
        --255 (0xff)   impdep2

-- Auxiliar functions
toPP :: Int -> PP_Doc
toPP i = case i of
            21 -> text "iload"
            22 -> text "lload"
            23 -> text "fload"
            24 -> text "dload"
            25 -> text "aload"
            54 -> text "istore"
            55 -> text "lstore"
            56 -> text "fstore"
            57 -> text "dstore"
            58 -> text "astore"
            169 -> text "ret"
            139 -> text "iinc"

fromBytes2Tupla :: [Int] -> [(Int, Int)]
fromBytes2Tupla = entuplar . toInts

entuplar [] = []
entuplar (x:y:zs) = (x,y) : entuplar zs

{-
fromBytes2Tupla xs = (\(_, lst1, lst2) -> zip lst1 lst2) $ foldr cons nil (toInts xs)
    where nil = (0, [], [])
          cons b (v, lst1, lst2) = if odd v then (v+1, lst1, b:lst2) else (v+1, b:lst1, lst2)
-}

toInts :: [Int] -> [Int]
toInts [] = []
toInts xs = let v = fromBytes2Int $ take 4 xs
            in v : (toInts $ drop 4 xs)

fromBytes2Int :: [Int] -> Int
fromBytes2Int xs = fst $ foldr cons nil xs
    where nil          = (0, -8)
          cons n (v,c) = (n =<<= (c+8) =|= v, c+8)

infixl 5 =<<=, =|=

(=<<=) :: Int -> Int -> Int
a =<<= b = a `shiftL` b

(=|=) :: Int -> Int -> Int
a =|= b = a .|. b

getSignedInt :: Int -> Int -> Int
getSignedInt n t = let (l:ls) = reverse $ toBinary n
                       (xs, ys) = if length (l:ls) == t then span (== 0) (if l == 1 then ls else l:ls) else ([], l:ls)
                   in toInt ys (length ys - 1)
    where toBinary :: Int -> [Int]
          toBinary 0 = []
          toBinary n = (n `mod` 2) : (toBinary (n `div` 2))

          toInt :: [Int] -> Int -> Int
          toInt [] c = 0
          toInt (n:ns) c = n * (2 ^ c) + toInt ns (c-1)

