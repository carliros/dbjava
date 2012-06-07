module Main where
import Data.Binary
import Data.Bits

fromBytes2Int :: [Int] -> Int
fromBytes2Int xs = fst $ foldr cons nil xs
    where nil          = (0, -8)
          cons n (v,c) = (n =<<= (c+8) =|= v, c+8)

-- foldr :: (a -> b -> b) -> b -> [a] -> b

infixl 5 =<<=, =|=

(=<<=) :: Int -> Int -> Int
a =<<= b = a `shiftL` b

(=|=) :: Int -> Int -> Int
a =|= b = a .|. b


data Exp = IntE Int
          | OpE  String Exp Exp
    deriving Show

instance Binary Exp where
       put (IntE i)          = do put (0 :: Word8)
                                  put i
       put (OpE s e1 e2)     = do put (1 :: Word8)
                                  put s
                                  put e1
                                  put e2
 
       get = do t <- get :: Get Word8
                case t of
                     0 -> do i <- get
                             return (IntE i)
                     1 -> do s  <- get
                             e1 <- get
                             e2 <- get
                             return (OpE s e1 e2)

e = OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))

