module Jvm.BinaryClass where
import Jvm.Data.ClassFormat

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Control.Monad
import Data.Word
import Data.Int
import Data.Bits

instance Binary ClassFile where
    put (ClassFile mg mnv mjv tam_cp lst_cp flgs ths spr tam_if lst_if tam_fd lst_fd tam_mth lst_mth tam_attr lst_attr)
        = put mg                        >> 
          put mnv                       >> 
          put mjv                       >> 
          put (fromInt2Word16 tam_cp)   >> 
          mapM_ put lst_cp              >> 
          put flgs                      >> 
          put ths                       >> 
          put spr                       >> 
          put (fromInt2Word16 tam_if)   >> 
          mapM_ put lst_if              >>
          put (fromInt2Word16 tam_fd)   >>
          mapM_ put lst_fd              >>
          put (fromInt2Word16 tam_mth)  >>
          mapM_ put lst_mth             >>
          put (fromInt2Word16 tam_attr) >>
          mapM_ put lst_attr

    -- el tamanio del constant_pool = 1 + length(constant_pool)
    get = do mg          <- get :: Get Magic
             mnv         <- get :: Get MinorVersion
             mjv         <- get :: Get MajorVersion
             wtam_cp     <- getWord16
             let tam_cp  =  fromWord162Int wtam_cp
             lst_cp      <- getMany $ tam_cp-1
             flgs        <- get :: Get AccessFlags
             ths         <- get :: Get ThisClass
             spr         <- get :: Get SuperClass
             wtam_if     <- getWord16
             let tam_if  =  fromWord162Int wtam_if
             lst_if      <- getMany tam_if
             wtam_fd     <- getWord16
             let tam_fd  =  fromWord162Int wtam_fd
             lst_fd      <- getMany tam_fd
             wtam_mth    <- getWord16
             let tam_mth =  fromWord162Int wtam_mth
             lst_mth     <- getMany tam_mth
             wtam_attr   <- getWord16
             let tam_attr = fromWord162Int wtam_attr
             lst_attr    <- getMany tam_attr
             return $ ClassFile mg mnv mjv tam_cp lst_cp flgs ths spr tam_if lst_if tam_fd lst_fd tam_mth lst_mth tam_attr lst_attr

    

instance Binary Magic where
    put (Magic) = put (202::Word8) >> put (254::Word8) >> put (186::Word8) >> put (190::Word8)
    get = do ca <- getWord8
             fe <- getWord8
             ba <- getWord8
             be <- getWord8
             return Magic

instance Binary MinorVersion where
    put (MinorVersion i) 
        = put $ fromInt2Word16 i
    get = do str <- getWord16
             return $ MinorVersion $ fromWord162Int  str

instance Binary MajorVersion where
    put (MajorVersion i) = put $ fromInt2Word16 i
    get = do str <- getWord16
             return $ MajorVersion $ fromWord162Int str

instance Binary CP_Info where
    put (Class_Info tag_cp index_cp str)
        = put tag_cp >> put (fromInt2Word16 index_cp)
    put (FieldRef_Info tag_cp index_name_cp index_nameandtype_cp str)
        = put tag_cp >> put (fromInt2Word16 index_name_cp) >> put (fromInt2Word16 index_nameandtype_cp)
    put (MethodRef_Info tag_cp index_name_cp index_nameandtype_cp _)
        = put tag_cp >> put (fromInt2Word16 index_name_cp) >> put (fromInt2Word16 index_nameandtype_cp)
    put (InterfaceMethodRef_Info tag_cp index_name_cp index_nameandtype_cp _)
        = put tag_cp >> put (fromInt2Word16 index_name_cp) >> put (fromInt2Word16 index_nameandtype_cp)
    put (String_Info tag_cp index_cp _)
        = put tag_cp >> put (fromInt2Word16 index_cp)
    put (Integer_Info tag_cp numi_cp _)
        = put tag_cp >> put (fromInt2Word32 numi_cp)
    put (Float_Info tag_cp numf_cp _)
        = put tag_cp >> put (fromFloat2Word32 numf_cp)
    put (Long_Info tag_cp numi_l1_cp numi_l2_cp _)
        = put tag_cp >> put (fromInt2Word32 numi_l1_cp) >> put (fromInt2Word32 numi_l2_cp)
    put (Double_Info tag_cp numi_d1_cp numi_d2_cp _)
        = put tag_cp >> put (fromInt2Word32 numi_d1_cp) >> put (fromInt2Word32 numi_d2_cp)
    put (NameAndType_Info tag_cp index_name_cp index_descr_cp _)
        = put tag_cp >> put (fromInt2Word16 index_name_cp) >> put (fromInt2Word16 index_descr_cp)
    put (Utf8_Info tag_cp tam_cp cad_cp _)
        = put tag_cp >> put (fromInt2Word16 tam_cp) >> mapM_ put cad_cp

    get = do tag  <- get :: Get Tag
             case tag of 
                TagClass 
                    -> do wind <- getWord16
                          let ind = fromWord162Int wind
                          return $ Class_Info tag ind ""
                TagFieldRef 
                    -> do wind1 <- getWord16
                          wind2 <- getWord16
                          let ind1 = fromWord162Int wind1
                              ind2 = fromWord162Int wind2
                          return $ FieldRef_Info tag ind1 ind2 ""
                TagMethodRef
                    -> do wind1 <- getWord16
                          wind2 <- getWord16
                          let ind1 = fromWord162Int wind1
                              ind2 = fromWord162Int wind2
                          return $ MethodRef_Info tag ind1 ind2 ""
                TagInterfaceMethodRef
                    -> do wind1 <- getWord16
                          wind2 <- getWord16
                          let ind1 = fromWord162Int wind1
                              ind2 = fromWord162Int wind2
                          return $ InterfaceMethodRef_Info tag ind1 ind2 ""
                TagString
                    -> do wind <- getWord16
                          let ind = fromWord162Int wind
                          return $ String_Info tag ind ""
                TagInteger
                    -> do wnum <- getWord32
                          let num = fromWord322Int wnum
                          return $ Integer_Info tag num ""
                TagFloat
                    -> do wnum <- getWord32
                          let num = fromWord322Float wnum
                          return $ Float_Info tag num ""
                TagLong
                    -> do wnum1 <- getWord32
                          wnum2 <- getWord32
                          let num1 = fromWord322Int wnum1
                              num2 = fromWord322Int wnum2
                          return $ Long_Info tag num1 num2 ""
                TagDouble
                    -> do wnum1 <- getWord32
                          wnum2 <- getWord32
                          let num1 = fromWord322Int wnum1
                              num2 = fromWord322Int wnum2
                          return $ Double_Info tag num1 num2 ""
                TagNameAndType
                    -> do wnum1 <- getWord16
                          wnum2 <- getWord16
                          let num1 = fromWord162Int wnum1
                              num2 = fromWord162Int wnum2
                          return $ NameAndType_Info tag num1 num2 ""
                TagUtf8
                    -> do wnum <- getWord16
                          let num = fromWord162Int wnum
                          lst  <- getMany num
                          return $ Utf8_Info tag num lst ""

instance Binary Tag where
    put (TagClass)              = put (7 ::Word8)
    put (TagFieldRef)           = put (9 ::Word8)
    put (TagMethodRef)          = put (10::Word8)
    put (TagInterfaceMethodRef) = put (11::Word8)
    put (TagString)             = put (8 ::Word8)
    put (TagInteger)            = put (3 ::Word8)
    put (TagFloat)              = put (4 ::Word8)
    put (TagLong)               = put (5 ::Word8)
    put (TagDouble)             = put (6 ::Word8)
    put (TagNameAndType)        = put (12::Word8)
    put (TagUtf8)               = put (1 ::Word8)
    get = do num <- get::Get Word8
             let val = fromWord82Int num
             let tag = case val of
                         7  -> TagClass
                         9  -> TagFieldRef
                         10 -> TagMethodRef
                         11 -> TagInterfaceMethodRef
                         8  -> TagString
                         3  -> TagInteger
                         4  -> TagFloat
                         5  -> TagLong
                         6  -> TagDouble
                         12 -> TagNameAndType
                         1  -> TagUtf8
                         _  -> error $ "Error: Unknow Tag " ++ show val
             return tag

instance Binary AccessFlags where
    put (AccessFlags lst) = do
        let flag = if null lst
                   then 0
                   else foldl1 (.+.) lst
        put $ fromInt2Word16 flag
    get = do wmask <- getWord16
             let mask = fromWord162Int wmask
                 lst  = filter (bitsSet mask) [ acc_Public
                                              , acc_Private
                                              , acc_Protected
                                              , acc_Static
                                              , acc_Final
                                              , acc_Super_Synchronized
                                              , acc_Volatile_Bridge
                                              , acc_Transient_Varargs
                                              , acc_Native
                                              , acc_Interface
                                              , acc_Abstract
                                              , acc_Strict
                                              , acc_Synthetic
                                              , acc_Annotation
                                              , acc_Enum]
             return $ AccessFlags lst

instance Binary ThisClass where
    put (ThisClass i) = put $ fromInt2Word16 i
    get = do wnum <- get :: Get Word16
             let num = fromWord162Int wnum
             return $ ThisClass num

instance Binary SuperClass where
    put (SuperClass i) = put $ fromInt2Word16 i
    get = do wnum <- get :: Get Word16
             let num = fromWord162Int wnum
             return $ SuperClass num

instance Binary Interface where
    put (Interface i) = put $ fromInt2Word16 i
    get = do wiif <- get :: Get Word16
             let iif = fromWord162Int wiif
             return $ Interface iif

instance Binary Field_Info where
    put (Field_Info accs inam idsr tam lst_attr)
        = put accs >> put (fromInt2Word16 inam) >> put (fromInt2Word16 idsr) >> put (fromInt2Word16 tam) >> mapM_ put lst_attr
    get = do accs  <- get :: Get AccessFlags
             winam <- getWord16
             widsr <- getWord16
             wtam  <- getWord16
             let inam     = fromWord162Int winam
             let idsr     = fromWord162Int widsr
             let tam_attr = fromWord162Int wtam
             lst_attr <- getMany tam_attr
             return $ Field_Info accs inam idsr tam_attr lst_attr

instance Binary Method_Info where
    put (Method_Info accs inam idsr tam_attr lst_attr)
        = put accs >> put (fromInt2Word16 inam) >> put (fromInt2Word16 idsr) >> put (fromInt2Word16 tam_attr) >> mapM_ put lst_attr
    get = do accs  <- get :: Get AccessFlags
             winam <- getWord16
             widsr <- getWord16
             wtam  <- getWord16
             let inam     = fromWord162Int winam
             let idsr     = fromWord162Int widsr
             let tam_attr = fromWord162Int wtam
             lst_attr <- getMany tam_attr 
             return $ Method_Info accs inam idsr tam_attr lst_attr

instance Binary Attribute_Info where
    put (AttributeGeneric inam tam_all rest_attr)
        = put (fromInt2Word16 inam) >> put (fromInt2Word32 tam_all) >> putLazyByteString rest_attr   --error "Invalid Attribute, Class Error"
    
    put (AttributeConstantValue inam tam_all ival)
        = put (fromInt2Word16 inam) >> put (fromInt2Word32 tam_all ) >> put (fromInt2Word16 ival)
    
    put (AttributeCode inam tam_all mlen_stack mlen_local tam_code lst_code tam_ex lst_ex tam_attr lst_attr)
        = put (fromInt2Word16 inam)                        >> 
          put (fromInt2Word32 tam_all)                     >> 
          put (fromInt2Word16 mlen_stack)                  >> 
          put (fromInt2Word16 mlen_local)                  >> 
          put (fromInt2Word32 tam_code)                    >> 
          mapM_ (\cod -> putWord8 (fromInt2Word8 cod)) lst_code     >> 
          put (fromInt2Word16 tam_ex)                      >> 
          mapM_ (\(e1,e2,e3,e4) -> put (fromInt2Word16 e1) >> put (fromInt2Word16 e2) >> put (fromInt2Word16 e3) >> put (fromInt2Word16 e4)) lst_ex >>
          put (fromInt2Word16 tam_attr) >> 
          mapM_ put lst_attr

    put (AttributeExceptions inam tam_all tam_num_ex lst_ex)
        = put (fromInt2Word16 inam)                        >> 
          put (fromInt2Word32 tam_all)                     >> 
          put (fromInt2Word16 tam_num_ex)                  >> 
          mapM_ (\ind -> putWord8 (fromInt2Word8 ind)) lst_ex 
     
    put (AttributeInnerClasses inam tam_all tam_classes lst_classes)
        = put (fromInt2Word16 inam)                        >> 
          put (fromInt2Word32 tam_all)                     >> 
          put (fromInt2Word16 tam_classes)                  >> 
          mapM_ (\(incl,outcl,innm,inflg) -> put (fromInt2Word16 incl)  >>
                                             put (fromInt2Word16 outcl) >> 
                                             put (fromInt2Word16 innm)  >> 
                                             put inflg) lst_classes
 
    put (AttributeSynthetic inam tam_all)
        = put (fromInt2Word16 inam)                        >> 
          put (fromInt2Word32 tam_all)

    put (AttributeSourceFile inam tam_all ind_src)
        = put (fromInt2Word16 inam) >> put (fromInt2Word32 tam_all) >> put (fromInt2Word16 ind_src)

    put (AttributeLineNumberTable inam tam_all tam_table lst_line)
        = put (fromInt2Word16 inam)                  >> 
          put (fromInt2Word32 tam_all)               >> 
          put (fromInt2Word16 tam_table)             >> 
          mapM_ (\(e1,e2) -> put (fromInt2Word16 e1) >> put (fromInt2Word16 e2)) lst_line

    put (AttributeLocalVariableTable inam tam_all tam_var lst_var)
        = put (fromInt2Word16 inam) >>
          put (fromInt2Word32 tam_all) >>
          put (fromInt2Word16 tam_var) >>
          mapM_ (\(e1,e2,e3,e4,e5) -> put (fromInt2Word16 e1) >> put (fromInt2Word16 e2) >> put (fromInt2Word16 e3) >> put (fromInt2Word16 e4) >> put (fromInt2Word16 e5)) lst_var
    
    put (AttributeDeprecated inam tam_all)
        = put (fromInt2Word16 inam)                        >> 
          put (fromInt2Word32 tam_all)

    get = do winam     <- getWord16
             wtam_all  <- getWord32
             let inam    = fromWord162Int winam
                 tam_all = fromWord322Int wtam_all
             rest_attr <- getLazyByteString $ toInt64 tam_all
             return $ AttributeGeneric inam tam_all rest_attr

type Tupla5Int = [(Int, Int, Int, Int, Int)]
type Tupla2Int = [(Int, Int)]
type Tupla4Int = [(Int, Int, Int, Int)]
type ListaInt  = [Int]
type ConstantPool_Count  = Int
type Interfaces_Count    = Int
type Fields_Count        = Int
type Methods_Count       = Int
type Attributes_Count    = Int
type Index_Constant_Pool = Int


-- auxiliar functions
infixl 5 .+.
(.+.) :: Int -> Int -> Int
a .+. b = a .|. b

bitsSet :: Int -> Int -> Bool
bitsSet mask i
    = (mask .&. i == i)

toInt64 :: Int -> Int64
toInt64 = read.show

getWord16 = get :: Get Word16
getWord32 = get :: Get Word32

fromWord162Int :: Word16 -> Int
fromWord162Int = read.show

fromWord82Int :: Word8 -> Int
fromWord82Int = read.show

fromWord322Int :: Word32 -> Int
fromWord322Int = read.show

fromWord322Float :: Word32 -> Float
fromWord322Float = read.show

fromInt2Word8 :: Int -> Word8
fromInt2Word8 = read.show

fromInt2Word16 :: Int -> Word16
fromInt2Word16 = read.show

fromInt2Word32 :: Int -> Word32
fromInt2Word32 = read.show

fromFloat2Word32 :: Float -> Word32
fromFloat2Word32 = read.show

getMany :: Binary a => Int -> Get [a]
getMany n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 x `seq` go (x:xs) (i-1) -- we must seq x to avoid stack overflows due to laziness in (>>=)

-- functions to modify attributes
getListAttr cp_infos 0 str = ([],str,toInt64 0)
getListAttr cp_infos n str
    = let (winam,rs1,n1) = runGetState (get :: Get Word16) str (toInt64 0)
          inam           = fromWord162Int winam
          (wtam,rs2,n2)  = runGetState (get :: Get Word32) rs1 (toInt64 0)
          tam            = fromWord322Int wtam
          (rest,rs3,n3)  = runGetState (getLazyByteString (toInt64 tam)) rs2 (toInt64 0)
          attr_generic   = AttributeGeneric inam tam rest
          attr_specific  = fChgAttr cp_infos attr_generic
      in let (lstn, rsn, nn) = getListAttr cp_infos (n-1) rs3 in (attr_specific : lstn, rsn, nn)

getListExCod 0 str = ([],str)
getListExCod n str
    = let (wstart_pc  , rs1, n1) = runGetState (get :: Get Word16) str (toInt64 0)
          (wend_pc    , rs2, n2) = runGetState (get :: Get Word16) rs1 (toInt64 0)
          (whandler_pc, rs3, n3) = runGetState (get :: Get Word16) rs2 (toInt64 0)
          (wcatch_type, rs4, n4) = runGetState (get :: Get Word16) rs3 (toInt64 0)
          start_pc               = fromWord162Int wstart_pc
          end_pc                 = fromWord162Int wend_pc
          handler_pc             = fromWord162Int whandler_pc
          catch_type             = fromWord162Int wcatch_type
      in let (lst, r) = getListExCod (n-1) rs4 in ((start_pc,end_pc,handler_pc,catch_type):lst, r)

getListTuplaInner 0 str = ([],str)
getListTuplaInner n str
    = let (wincl , rs1, n1) = runGetState (get :: Get Word16) str (toInt64 0)
          (woutcl, rs2, n2) = runGetState (get :: Get Word16) rs1 (toInt64 0)
          (winnm , rs3, n3) = runGetState (get :: Get Word16) rs2 (toInt64 0)
          (inflg, rs4, n4) = runGetState (get :: Get AccessFlags) rs3 (toInt64 0)
          incl              = fromWord162Int wincl
          outcl             = fromWord162Int woutcl
          innm              = fromWord162Int winnm
      in let (lst, r) = getListTuplaInner (n-1) rs4 in ((incl,outcl,innm,inflg):lst, r)

getListEx 0 str = ([],str)
getListEx n str
    = let (wcod, str', no) = runGetState (get :: Get Word16) str (toInt64 0)
          ex = fromWord162Int wcod
      in let (lst,r) = getListEx (n-1) str' in (ex:lst, r)

getListCode 0 str = ([],str)
getListCode n str
    = let (wcod, str', no) = runGetState (get :: Get Word8) str (toInt64 0)
          cod = fromWord82Int wcod
      in let (lst,r) = getListCode (n-1) str' in (cod:lst, r)

getListLineNumber 0 str = ([], str)
getListLineNumber n str
    = let (wstart_pc, str1, n1)    = runGetState (get :: Get Word16) str  (toInt64 0)
          start_pc                 = fromWord162Int wstart_pc
          (wline_number, str2, n2) = runGetState (get :: Get Word16) str1 (toInt64 0)
          line_number              = fromWord162Int wline_number
      in let (lst,r) = getListLineNumber (n-1) str2 in ((start_pc,line_number):lst, r)

-- Get the name from Utf8_Info in the Contant Pool list
getNameCP_Utf8 :: Int -> CP_Infos -> String
getNameCP_Utf8 index cp_infos = cad_cp $ cp_infos !! (index-1)

fChgAttr :: CP_Infos -> Attribute_Info -> Attribute_Info
fChgAttr cp_infos (AttributeGeneric inam tam rbs) 
    = case getNameCP_Utf8 inam cp_infos of
        "SourceFile" 
            -> let (wisrc, rest, n) = runGetState (get :: Get Word16) rbs (toInt64 0)
                   isrc  = fromWord162Int wisrc
               in AttributeSourceFile inam tam isrc
        "Code"
            -> let (wstack   , rs1, n1) = runGetState (get :: Get Word16) rbs (toInt64 0)
                   stack                = fromWord162Int wstack
                   (wlocal   , rs2, n2) = runGetState (get :: Get Word16) rs1 (toInt64 0)
                   local                = fromWord162Int wlocal
                   (wtam_code, rs3, n3) = runGetState (get :: Get Word32) rs2 (toInt64 0)
                   tam_code             = fromWord322Int wtam_code
                   (lst_code, rs4)      = getListCode tam_code rs3
                   (wtam_ex, rs5, n4)   = runGetState (get :: Get Word16) rs4 (toInt64 0)
                   tam_ex               = fromWord162Int wtam_ex
                   (lst_ex, rs6)        = getListExCod tam_ex rs5
                   (wtam_attr,rs7,n5)   = runGetState (get :: Get Word16) rs6 (toInt64 0)
                   tam_attr             = fromWord162Int wtam_attr
                   (lst_attr,rs8, n6)   = getListAttr cp_infos tam_attr rs7
                   -- arreglar, porque no hay soporte para excepciones, ni la segunda lista de attributos ??
               in AttributeCode inam tam stack local tam_code lst_code tam_ex lst_ex tam_attr lst_attr
        "LineNumberTable"
            -> let (wntable, rs0, n0)   = runGetState (get :: Get Word16) rbs (toInt64 0)
                   ntable               = fromWord162Int wntable
                   (lst_line, rs1)      = getListLineNumber ntable rs0
               in AttributeLineNumberTable inam tam ntable lst_line
        "Exceptions"
            -> let (wntable, rs0, n0)   = runGetState (get :: Get Word16) rbs (toInt64 0)
                   ntable               = fromWord162Int wntable
                   (lst_ex, rs1)        = getListEx ntable rs0
               in AttributeExceptions inam tam ntable lst_ex

        "Synthetic"
            -> AttributeSynthetic inam tam

        "InnerClasses"
            -> let (wntable, rs0, n0)   = runGetState (get :: Get Word16) rbs (toInt64 0)
                   ntable               = fromWord162Int wntable
                   (lst_classes, rs1)   = getListTuplaInner ntable rs0
               in AttributeInnerClasses inam tam ntable lst_classes
        
        "Deprecated"
            -> AttributeDeprecated inam tam

        otherwise
            -> AttributeGeneric inam tam rbs


chgAttrG_Fields :: ClassFile -> ClassFile
chgAttrG_Fields cf = cf{array_fields = new_array_fields}
    where new_array_fields = map fun $ array_fields cf
          fun fld = fld{array_attr_fi = new_fi fld}
          new_fi fld' = map (fChgAttr (array_cp cf)) $ array_attr_fi fld'

chgAttrG_Methods :: ClassFile -> ClassFile
chgAttrG_Methods cf = cf{array_methods = new_array_methods}
    where new_array_methods = map fun $ array_methods cf
          fun mth = mth{array_attr_mi = new_mi mth}
          new_mi mth' = map (fChgAttr (array_cp cf)) $ array_attr_mi mth'

chgAttrG_ClassFile :: ClassFile -> ClassFile
chgAttrG_ClassFile cf = cf{array_attributes = new_array_attributes}
    where new_array_attributes = map (fChgAttr (array_cp cf)) $ array_attributes cf


-- functions accessors to to codify and decodify a class file format
encodeClassFile :: FilePath -> ClassFile -> IO ()
encodeClassFile = encodeFile

decodeClassFile :: FilePath -> IO ClassFile
decodeClassFile fn = do 
    obj <- decodeFile fn :: IO ClassFile
    let obj1 = chgAttrG_ClassFile obj
        obj2 = chgAttrG_Methods obj1
        obj3 = chgAttrG_Fields obj2
    return obj3

