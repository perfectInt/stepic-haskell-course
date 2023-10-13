class Printable value where
    toString :: value -> [Char]
    
instance Printable Bool where
    toString x = if x then "true" else "false"

instance Printable () where
    toString _ = "unit type"

-----------------------------------------------------------------

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

-----------------------------------------------------------------

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x
        | both = stomp (stab x)
        | mork = stomp x
        | gork = stab x
        | otherwise = x
        where
            mork = doesEnrageMork x
            gork = doesEnrageGork x
            both = mork && gork

-----------------------------------------------------------------

class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
      | x == maxBound = minBound
      | otherwise = succ x

  spred :: a -> a
  spred x
      | x == minBound = maxBound
      | otherwise = pred x

-----------------------------------------------------------------

avg :: Int -> Int -> Int -> Double
avg x y z = fromInteger (toInteger x + toInteger y + toInteger z) / 3.0