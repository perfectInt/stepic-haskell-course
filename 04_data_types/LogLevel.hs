data LogLevel = Error | Warning | Info deriving Eq

cmp :: LogLevel -> LogLevel -> Ordering
cmp x y = compare (rank x) (rank y)
	where
		rank Error = 3
		rank Warning = 2
		rank Info = 1