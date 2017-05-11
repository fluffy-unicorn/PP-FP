module PreParserGen where

-- NOTE: introductory form -- NOT complete, NOT correct, NO error messages

-- ==========================================================================================================

parserGen :: Grammar -> [Alphabet] -> ParseState -> [(ParseTree,[Token])]


parserGen gr []        (nt0,ts,tokens)  = [(PNode nt0 ts, tokens)]

parserGen gr (nt:rule) (nt0,ts,[])      = []


parserGen gr (nt:rule) (nt0,ts,(cat,str):tokens) = case nt of

        -- ============================================================================================================
        -- Backus-Naur constructions

        Alt  nts mts    ->    parserGen  gr  (nts++rule)                 (nt0, ts, (cat,str):tokens)
                           ++ parserGen  gr  (mts++rule)                 (nt0, ts, (cat,str):tokens)

        Opt  nts        ->    parserGen  gr  (nts++rule)                 (nt0, ts, (cat,str):tokens)
                           ++ parserGen  gr   rule                       (nt0, ts, (cat,str):tokens)

        Rep0 nts        ->    parserGen  gr  (nts ++ (Rep0 nts : rule))  (nt0, ts, (cat,str):tokens)
                           ++ parserGen  gr   rule                       (nt0, ts, (cat,str):tokens)

        Rep1 nts        ->    parserGen  gr  (nts ++ (Rep0 nts : rule))  (nt0, ts, (cat,str):tokens)

        -- ============================================================================================================
        -- Terminals

        Terminal str'   | str==str'     -> parserGen gr rule (nt0, ts++[PLeaf (cat,str)], tokens)
                        | otherwise     -> []

        Symbol str'     | str==str'     -> parserGen gr rule (nt0, ts, tokens)
                        | otherwise     -> []

        SyntCat cat'    | cat==cat'     -> parserGen gr rule (nt0, ts++[PLeaf (cat,str)], tokens)
                        | otherwise     -> []

        -- ============================================================================================================
        -- Non-terminals

        _               ->     [  (t2,tokens2)  | r <- gr nt

                                                , (t1,tokens1) <- parserGen gr r (nt, [], (cat,str):tokens)

                                                , (t2,tokens2) <- parserGen gr rule (nt0, ts++[t1], tokens1)
                                                ]

