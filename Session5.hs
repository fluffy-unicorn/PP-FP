module Session5 where
import FP_Core
import FPPrac.Trees

--Exercise 2
codeGen2 :: Expr -> [Instr]
codeGen2 exp = codeGen2 exp ++ [EndProg]

codeGen2' :: Expr -> [Instr]
codeGen2' (Const i) = [PushConst i]
codeGen2' (BinExpr op e1 e2) = (codeGen2' e1) ++ (codeGen2' e2) ++ [Calc op]

--Exercise 3
transform3 :: Expr -> RoseTree
transform3 (Const i) = RoseNode (show i) []
transform3 (BinExpr op e1 e2) = RoseNode (show op) [transform3 e1, transform3 e2]

--Exercise 5
codeGen' :: Stmnt -> [Instr]
codeGen' (Assign v e) = codeGen2' e ++ [Store v]

--Exercise 6
class CodeGen c where
    codeGen :: CodeGen c => c -> [Instr]

instance CodeGen Expr where
    codeGen (Const i) = [PushConst i]
    codeGen (Load  v) = [PushAddr  v]
    codeGen (BinExpr op e1 e2) = (codeGen e1) ++ (codeGen e2) ++ [Calc op] 

instance CodeGen Stmnt where
    codeGen (Assign v e) = (codeGen e) ++ [Store v]
    codeGen (Repeat e ss)= (codeGen e) ++ [PushPC] ++ (concatMap codeGen ss) ++ [EndRep] 

class Transform t where
    transform :: Transform t => t -> RoseTree

instance Transform Expr where
    transform (Const i) = RoseNode (show i) []
    transform (BinExpr op e1 e2) = RoseNode (show op) [transform e1, transform e2]

instance Transform Stmnt where
    transform (Assign v e) = RoseNode "=" [ RoseNode ('*':show v) [], transform e ]

--Exercise 8
code = [Assign 0 (Const 0), Assign 1 (Const 0), Repeat (Const 10) [Assign 1 (BinExpr Add (Load 1) (Const 1)), Assign 0 (BinExpr Add (Load 0) (Load 1))]]
compiled = concatMap codeGen code
testc = testp compiled
