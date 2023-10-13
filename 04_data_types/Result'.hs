data Result' = Ok | Result' Int

instance Show Result' where
    show Ok = "Success"
    show (Result' n) = "Fail: " ++ show n

doSomeWork' :: SomeData -> Result'
doSomeWork' x = case snd $ doSomeWork x of
  0 -> Ok
  n -> Result' n