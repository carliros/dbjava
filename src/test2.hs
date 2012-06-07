module Test where 
import Jvm.Data.ClassFormat
import Jvm.BinaryClass

example = 
    ClassFile
        Magic
        (MinorVersion 0)
        (MajorVersion 50)
        1   -- count constant pool
        []  -- constant pool
        (AccessFlags [0]) -- access flags
        (ThisClass 0)   -- this
        (SuperClass 0)   -- super
        0   -- count intefaces
        []  -- interfaces
        0   -- count fields
        []  -- fields
        0   -- count methods
        []  -- methods
        0   -- count attributes
        []  -- attributes

-- serializarlo
serializar = encodeClassFile "example.class" example


