
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

-- Return all the possible substitutions with a given atom
unifyTerm :: Atom -> Program -> [Substitution]
unifyTerm a prog = [ fromJust unifiedRule | rule <- prog, let unifiedRule = unify (fst rule) a,  unifiedRule /= Nothing ]

-- Return all the possible substitutions with a given query
unifyQuery :: Query -> Program -> [Substitution]
unifyQuery as prog =  foldl union [] [ unifyTerm s prog | s <- as]

-- Return the substituted query
substituteQuery :: Query -> Substitution -> Query
substituteQuery query sub = [q ⇐ sub | q <- query]

-- Return all the possible substituted queries 
substituteAll :: Query -> Program -> [(Substitution, Query)]
substituteAll query prog = [ (sub, substituteQuery query sub) | sub <- unifyQuery query prog  ]


-- 'Proofs' a given atom according to the program, does not work with variables
proof :: Atom -> Program -> Bool
proof atom prog = case lookup atom prog of
                    Just atoms -> proofQuery atoms program
                    Nothing -> False

-- 'Proofs' a given query according to the program, does not work with variables
proofQuery :: Query -> Program -> Bool
proofQuery [] prog = True
proofQuery (a:atoms) prog = proof a prog && proofQuery atoms prog


