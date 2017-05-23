data Expr = Const Int
          | Var String
          | BinOp String Expr Expr
          | App Expr Expr
          | If Expr Expr ElsePart
          | ConstB BoolD
          | Tuple2 Expr Expr
          | Tuple3 Expr Expr Expr
          | Lambda String Type Expr

data ElsePart = Else Expr

data Type = IntType
          | FunType Type Type
          | BoolType
          | Tuple2Type Type Type
          | Tuple3Type Type Type Type
           deriving (Show,Eq)

data BoolD = BTrue | BFalse
type Env = [(String, Type)]


env = [("x",IntType),("y", IntType), ("+", FunType IntType (FunType IntType IntType)),("*", FunType IntType (FunType IntType IntType)),("-", FunType IntType (FunType IntType IntType)), ("&&", FunType BoolType (FunType BoolType BoolType)), ("||",  FunType BoolType (FunType BoolType BoolType))]

typeOf :: Env -> Expr -> Type
typeOf _ (Const _) = IntType
typeOf env (Var n) = case result of
                        Nothing -> error "Variable is not defined in given environment"
                        Just t  -> t
                     where result = lookup n env
typeOf env (BinOp op e0 e1) = case t_op of
                                Nothing -> error "Could not deduce type for binary operation"
                                Just (FunType t_0 (FunType t_1 t_2)) 
                                    | t_0 == t_e0 && t_1 == t_e1 -> t_2
                                    | otherwise ->  error "Could not deduce type for binary operation"
                                Just _ -> error "Could not deduce type for binary operation"
                              where
                                t_op = lookup op env
                                t_e0 = typeOf env e0
                                t_e1 = typeOf env e1
typeOf env (App f x) = case t_f of
                        FunType t_a t_b | t_a == t_x -> t_b
                                        | otherwise -> error "Could not deduce type for function application"
                        _ -> error "Could not deduce type for function application"
                       where 
                        t_f = typeOf env f
                        t_x = typeOf env x
typeOf env (If e0 e1 (Else e2)) | typeOf env e0 /= BoolType      = error "If-Then-Else condition is not of type bool"
                                | t1            /= typeOf env e2 = error "If- and Else-types are not the same"
                                | otherwise                      = t1
                               where t1 = typeOf env e1                        
typeOf env (ConstB b) = BoolType
typeOf env (Tuple2 e0 e1) = Tuple2Type (typeOf env e0) (typeOf env e1)
typeOf env (Tuple3 e0 e1 e2) = Tuple3Type (typeOf env e0) (typeOf env e1) (typeOf env e2)
typeOf env (Lambda _ a expr) = FunType a t where t = typeOf env expr 

