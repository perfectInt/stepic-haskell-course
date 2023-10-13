data Result = Fail | Success

processData :: SomeData -> String
processData x = case doSomeWork x of
  (Success, _) -> "Success"
  (_, n) -> "Fail: " ++ show n