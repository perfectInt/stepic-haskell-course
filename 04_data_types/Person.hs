data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p =
	| length (firstName p) < 2 = p
	| otherwise = p { firstName = take 1 (firstName p) ++ "." }

updateLastName :: Person -> Person -> Person
updateLastName person1 person2 = person2 { lastName = lastName person1 }