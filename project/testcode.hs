
refractor :: [Substitution] -> [(Term, [Term])] -> [(Term, [Term])]
refractor ((s0,s1):ss) [] = [(s0,[s1])] 
refractor [] sofar = sofar
refractor ((s0,s1):ss) sofar  = case lookup s0 sofar of
                                Just list -> refractor ss (replace sofar s0 (list++[s1]))
                                Nothing -> refractor ss (sofar++[(s0,[s1])])

replace :: [(Term, [Term])] -> Term -> [Term] -> [(Term, [Term])]
replace ((t0,[l0]):rest) term list | t0 == term = ((t0,list):rest)
                                   | otherwise = ((t0,[l0]):(replace rest term list))

intersectAll [[(Term, [Term]]] -> [(Term, [Term])] -> [(Term, [Term])]
intersectAll (((t0, l0):l1):l2) -> 

intersectAll :: [[Substitution]] -> [Substitution]
intersectAll [] = []
intersectAll [a] = a
intersectAll [[(Var x, Const a)],[(Var y, Const b)]] | x == y = [(Var x, Const a)] `intersect` [(Var y, Const b)]
                                                     | otherwise = [(Var x, Const a),(Var y, Const b)]
intersectAll (a:as) = a `intersect` (intersectAll as)
