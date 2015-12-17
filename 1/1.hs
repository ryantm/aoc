a :: String -> Int
a [] = 0
a ('(':rest) = 1 + a rest
a (')':rest) = (-1) + a rest

b :: String -> Int -> Int -> Int
b [] _ _ = 0
b _ (-1) pos = pos
b ('(':rest) acc pos = b rest (acc+1) (pos+1)
b (')':rest) acc pos = b rest (acc-1) (pos+1)
