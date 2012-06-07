module Main where

import Jvm.Data.ClassFormat
import Jvm.BinaryClass
import Jvm.PrettyClass
import UU.Pretty
import System.Environment

main::IO()
main = do args <- getArgs
          if null args then putStrLn "You need an *.class"
                       else do let file =  (!!) args 0
                               putStrLn "Java Bytecode Decompiler\n"
                               obj <- decodeClassFile file
                               render (pp obj) 100
                               putStrLn ""

