module Tree where
data ASTree = Add ASTree ASTree
            | Sub ASTree ASTree
            | Mul ASTree ASTree
            | Div ASTree ASTree
            | Value Int
instance Show ASTree where
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2
    show (Mul e1 e2) = show e1 ++ " * " ++ show e2
    show (Div e1 e2) = show e1 ++ " / " ++ show e2
    show (Value n)   = show n

buildExpr :: ASTree -> String -> ASTree -> ASTree
buildExpr a "+" b = Add a b
buildExpr a "-" b = Sub a b
buildExpr a "*" b = Mul a b
buildExpr a "/" b = Div a b

mapValues :: [String] -> [ASTree]
mapValues [] = []
mapValues (x:xs) = Value (read x) : mapValues xs

buildAST :: ([String], [String]) -> ASTree
buildAST ([], [val]) = Value (read val)
buildAST (ops, vals) = buildListExpr ops (mapValues vals)

buildListExpr:: [String] -> [ASTree] -> ASTree
buildListExpr [] [a] = a
buildListExpr (x:xs) (y:y1:ys) = buildListExpr xs (buildExpr y x y1 : ys)

treeT :: ASTree -> Int
treeT (Add e1 e2) = treeT e1 + treeT e2
treeT (Sub e1 e2) = treeT e1 - treeT e2
treeT (Mul e1 e2) = treeT e1 * treeT e2
treeT (Div e1 e2) = treeT e1 `div` treeT e2
treeT (Value n) = n