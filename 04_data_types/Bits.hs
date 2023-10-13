import Data.List (foldl', unfoldr)

data Bit = Zero | One

data Sign = Minus | Plus

data Z = Z Sign [Bit]

fromBit :: (Integral a) => Bit -> a
fromBit Zero = 0
fromBit One = 1

fromBits :: (Integral a) => [Bit] -> [a]
fromBits = map fromBit

fromSign :: (Integral a) => Sign -> a
fromSign Minus = -1
fromSign Plus = 1

fromZ :: (Integral a) => Z -> a
fromZ (Z sign bits) = sign' * value
  where
    sign' = fromSign sign
    value = (toDecimal . fromBits . reverse) bits

intoBit :: (Integral a) => a -> Bit
intoBit 0 = Zero
intoBit 1 = One
intoBit _ = undefined

intoBits :: (Integral a) => [a] -> [Bit]
intoBits = map intoBit

intoSign :: (Integral a) => a -> Sign
intoSign x = case signum x of
  -1 -> Minus
  _ -> Plus

intoZ :: (Integral a) => a -> Z
intoZ x = Z sign bits
  where
    sign = intoSign x
    bits = (intoBits . toReversedBinary) x

toDecimal :: Integral a => [a] -> a
toDecimal = foldl' f 0
  where
    f x acc = x * 2 + acc

toReversedBinary :: (Integral a) => a -> [a]
toReversedBinary = unfoldr f
  where
    f x = case divMod (abs x) 2 of
      (0, 0) -> Nothing
      (div, mod) -> Just (mod, div)

add :: Z -> Z -> Z
add a b = intoZ (fromZ a + fromZ b)

mul :: Z -> Z -> Z
mul a b = intoZ (fromZ a * fromZ b)