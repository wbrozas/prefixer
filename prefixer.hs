-------------------------------
--Written by: William Brozas --
-------------------------------
import Data.List
import Data.Char
import Data.Function
import System.Environment

--Types used for intermediate representation
data Operator = Div | Mult | Plus deriving (Eq, Ord, Enum, Show)
data Exp = Exp { operator :: Operator
				, terms :: [Exp] } 
		 | Term { coefficient :: Double
				, mvars :: String           --Variables multiplied
				, dvars :: String           --Variables divided
				} deriving (Eq, Ord, Show)  --Example term: '5 * x * x / y' would be 'Term 5 "xx" "y"'

--helper function for toPostfix
isOperator :: String -> Bool
isOperator (x:_) = x `elem` "+-*/("
isOperator _ = error "ERROR: Bad input for isOperator"

--helper function for toPostfix
hasPrecedence :: String -> String -> Bool
hasPrecedence (x:_) (y:_) = x `elem` "*/" && y `elem` "+-" || x == '('
hasPrecedence _ _ = error "ERROR: Bad input for hasPrecedence"

--(Infix to Postfix): Takes in a list of strings where each element is a string that represents a number, variable or operator and returns the list in a postfix form
--First argument is used as a stack of operators and should be [] to start
toPostfix :: [String] -> [String] -> [String]
toPostfix os [] = os
toPostfix [] (x:xs) = if isOperator x then toPostfix [x] xs else x : toPostfix [] xs
toPostfix os@(o:os') (x:xs)
	| isOperator x 	= if o == "(" && x /= "(" 
                        then toPostfix (x:os') xs
                        else if hasPrecedence x o 
                                then toPostfix (x:os) xs 
                                else o : toPostfix (x:os') xs
	| x == ")" 		= o : toPostfix os' xs
	| otherwise 	= x : toPostfix os xs

--This is used if no reduction is needed. Converts postfix list of strings to prefix string.
--First argument is used as a stack and should be [] to start
postfixToPrefix :: [String] -> [String] -> String
postfixToPrefix [z] [] = z
postfixToPrefix [] (x:xs) = postfixToPrefix [x] xs
postfixToPrefix zs@(z:zs') (x:xs) = if isOperator x 
                                then postfixToPrefix (("( " ++ x ++ (' ': head zs') ++ (' ' : z) ++ " )") : tail zs') xs 
                                else postfixToPrefix (x:zs) xs
postfixToPrefix _ _ = error "ERROR: Bad input for postfixToPrefix"

--helper function for postfixToIr
isVar :: String -> Bool
isVar (x:_) = isAlpha x
isVar _ = error "ERROR: Bad input for isVar"

--helper function for postfixToIr
negateExp :: Exp -> Exp
negateExp (Term c ms ds) = Term (-1 * c) ms ds
negateExp (Exp o ts) = Exp o (foldr (\x acc -> negateExp x : acc) [] ts)

--helper function for postfixToIr
createExp :: String -> Exp -> Exp -> Exp
createExp o x y
	| o == "+" = Exp Plus (x:[y])
	| o == "-" = Exp Plus (x:[negateExp y])
	| o == "*" = Exp Mult (x:[y])
	| o == "/" = Exp Div (x:[y])
    | otherwise = error "ERROR: Bad input for createExp"

--Converts postfix list to an intermediate representation which makes it easier to reduce the expression
--First argument is used as a stack and should start out as []
postfixToIr :: [Exp] -> [String] -> Exp
postfixToIr [z] [] = z
postfixToIr [] (x:xs) = if isVar x
                        then postfixToIr [Term 1 x ""] xs 
                        else postfixToIr [Term (read x :: Double) "" ""] xs
postfixToIr zs@[_] (x:xs) = if isVar x 
                            then postfixToIr (Term 1 x "" : zs) xs 
                            else postfixToIr (Term (read x :: Double) "" "" : zs) xs
postfixToIr zs@(z1:z2:zs') (x:xs)
    | isOperator x  = postfixToIr (createExp x z2 z1 : zs') xs 
    | isVar x       = postfixToIr (Term 1 x "" : zs) xs 
    | otherwise     = postfixToIr (Term (read x :: Double) "" "" : zs) xs
postfixToIr _ _ = error "ERROR: Bad input for postfixToIr"

--Takes in expression in the intermediate representation and distributes all multiplications and divisions
distribute :: Exp -> Exp
distribute t@(Term {}) = t
distribute (Exp o ts)
	| o == Plus = Exp Plus (foldr (\x acc -> getAllTerms (distribute x) ++ acc) [] ts)
	| o == Mult = case ts of
                    (x:[y]) -> Exp Plus (distributeMult (distribute x) (distribute y))
                    _       -> error "ERROR: Cannot distribute Mult expression" 
	| o == Div = case ts of
                    (x:[y]) -> Exp Plus (distributeDiv (distribute x) (distribute y))
                    _       -> error "ERROR: Cannot distribute Div expression"
    | otherwise = error "ERROR: Bad input for distribute"

--helper function for distribute: Creates a list of terms from an expression and the expressions in its list of expressions and so on
getAllTerms :: Exp -> [Exp]
getAllTerms t@(Term {}) = [t]
getAllTerms (Exp _ ts) = foldr (\x acc -> getAllTerms x ++ acc) [] ts

--helper function for distribute for distributing multiplications
distributeMult :: Exp -> Exp -> [Exp]
distributeMult (Exp _ ts) (Exp _ t2s) = foldr (\x acc -> distributeMultHelper x t2s ++ acc) [] ts
distributeMult t@(Term {}) (Exp _ ts) = distributeMultHelper t ts
distributeMult (Exp _ ts) t@(Term {}) = distributeMultHelper t ts
distributeMult t@(Term {}) t2@(Term {}) = [eval Mult t t2]

distributeMultHelper :: Exp -> [Exp] -> [Exp]
distributeMultHelper e = foldr ((:) . eval Mult e) []

--helper function for distribute for distributing divisions
distributeDiv :: Exp -> Exp -> [Exp]
distributeDiv (Exp _ ts) e@(Term {}) = foldr (\x acc -> eval Div x e : acc) [] ts
distributeDiv t@(Term {}) t2@(Term {}) = [eval Div t t2]
distributeDiv t@(Term {}) e@(Exp Plus _) = case addLikeTerms e of 
                                                (Exp Plus [z]) -> [eval Div t z]
                                                divExp -> [Exp Div [t,divExp]]
distributeDiv (Exp _ ts) (Exp Plus [z]) = foldr (\x acc -> eval Div x z : acc) [] ts
distributeDiv e1 e2 = let a = addLikeTerms (distribute e1)
                        in let b = addLikeTerms (distribute e2) in
                            if a == b
                                then [Term 1 "" ""] 
                                else [Exp Div [a,b]]

--function to be called after distribute to add all like terms
addLikeTerms :: Exp -> Exp
addLikeTerms t@(Term {}) = t
addLikeTerms (Exp Plus ts) = let xs = (foldr (\x acc -> if isTerm x && isZero x then acc else x:acc) []
                                            $ sortBy (compare `on` (\x -> case x of 
                                                                            (Term c _ _ ) -> c
                                                                            _ -> -1)) 
                                            $ foldr (\x acc -> addLikeTermsHelper x : acc) [] 
                                            $ groupBy varsEqual 
                                            $ sortBy (compare `on` getVarStr) ts)
                                in if xs == []
                                    then Term 0 "" ""
                                    else Exp Plus xs
addLikeTerms _ = error "ERROR: Bad input for addLikeTerms"

addLikeTermsHelper :: [Exp] -> Exp
addLikeTermsHelper [t] = t
addLikeTermsHelper (t:ts) = foldr (eval Plus) t ts
addLikeTermsHelper _ = error "ERROR: Bad input for addLikeTermsHelper"

--helper function to evaluate two terms and an operator
eval :: Operator -> Exp -> Exp -> Exp
eval o (Term xc xms xds) (Term yc yms yds)
	| o == Plus = Term (xc + yc) xms xds
	| o == Mult = let (ms,ds) = combineVars xms xds yms yds in Term (xc * yc) ms ds
	| o == Div 	= let (ms,ds) = combineVars xms xds yds yms in Term (xc / yc) ms ds
eval _ _ _ = error "ERROR: Bad input for eval"

--helper function for eval, it is used to divide and multiply variables
combineVars :: String -> String -> String -> String -> (String,String)
combineVars xms xds yms yds = (merge (xms \\ yds) (yms \\ xds), merge (xds \\ yms) (yds \\ xms))

merge :: (Ord a) => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge s@(x:xs) t@(y:ys) = if x < y then x : merge xs t else y : merge s ys

isTerm :: Exp -> Bool
isTerm (Term {}) = True
isTerm _ = False

isZero :: Exp -> Bool
isZero (Term c _ _) = c == 0
isZero _ = error "ERROR: Bad input for isZero"

--Concats multiplied and divided variables to determine if a term is a like term with another term
getVarStr :: Exp -> String
getVarStr (Term _ ms ds) = ms ++ ":" ++ ds
getVarStr _ = error "ERROR: Bad input for getVarStr"

varsEqual :: Exp -> Exp -> Bool
varsEqual (Term _ xms xds) (Term _ yms yds) = xms == yms && xds == yds
varsEqual _ _ = False

isNegative :: Exp -> Bool
isNegative (Term c _ _) = c < 0
isNegative _ = error "ERROR: Bad input for isNegative"

negateTerm :: Exp -> Exp
negateTerm (Term c ms ds) = Term (-1 * c) ms ds
negateTerm _ = error "ERROR: Bad input for negateTerm"

--After reducing and adding like terms this function is used to convert the intermediate representation to prefix notation
fromIrToPrefix :: Exp -> String
fromIrToPrefix t@(Term {}) = fromTermToPrefix t
fromIrToPrefix (Exp Plus ts) = fromIrToPrefixHelper ts 
fromIrToPrefix (Exp Div (t1:[t2])) = "( / " ++ fromIrToPrefix t1 ++ " " ++ fromIrToPrefix t2 ++ " )"
fromIrToPrefix _ = error "ERROR: Bad input for fromIrToPrefix"

fromIrToPrefixHelper :: [Exp] -> String
fromIrToPrefixHelper [x] = if isTerm x && isNegative x 
                                then "( - 0 " ++ fromTermToPrefix (negateTerm x) ++ " )" 
                                else fromTermToPrefix x
fromIrToPrefixHelper (x:[y]) 
    | isNegative y  = "( - ( - 0 " ++ fromTermToPrefix (negateTerm y) ++ " ) " ++ fromTermToPrefix (negateTerm x) ++ " )" 
    | isNegative x  = "( - " ++ fromTermToPrefix y ++ " " ++ fromTermToPrefix (negateTerm x) ++ " )" 
    | otherwise     = "( + " ++ fromTermToPrefix y ++ " " ++ fromTermToPrefix x ++ " )"
fromIrToPrefixHelper (x:xs) = if isNegative x 
                                    then "( - " ++ fromIrToPrefixHelper xs ++ " " ++ fromTermToPrefix (negateTerm x) ++ " )" 
                                    else "( + " ++ fromIrToPrefixHelper xs ++ " " ++ fromTermToPrefix x ++ " )"
fromIrToPrefixHelper _ = error "ERROR: Bad input for fromIrToPrefixHelper"

--helper function for fromIrToPrefix that converts a term to prefix notation
fromTermToPrefix :: Exp -> String
fromTermToPrefix (Term c "" "") = show c
fromTermToPrefix (Term c "" d) = take (4 * length d) (cycle "( / ") ++ show c ++ foldl (\acc x -> " " ++ [x] ++ " )" ++ acc) [] d
fromTermToPrefix (Term c m d) = if c == 1 
                                    then take (4 * length d) (cycle "( / ")
                                            ++ take (4 * (length m - 1)) (cycle "( * ")
                                            ++ head m : foldl (\acc x -> " " ++ [x] ++ " )" ++ acc) [] (tail m)
                                            ++ foldl (\acc x -> " " ++ [x] ++ " )" ++ acc) [] d
                                    else take (4 * length d) (cycle "( / ")
                                            ++ take (4 * length m) (cycle "( * ") 
                                            ++ show c
                                            ++ foldl (\acc x -> " " ++ [x] ++ " )" ++ acc) [] m 
                                            ++ foldl (\acc x -> " " ++ [x] ++ " )" ++ acc) [] d
fromTermToPrefix e = fromIrToPrefix e

--takes in the command line arguments and either converts to prefix or reduces and converts
toPrefix :: [String] -> String -> String
toPrefix args str = if "-r" `elem` args
                        then fromIrToPrefix $ addLikeTerms $ distribute $ postfixToIr [] $ toPostfix [] $ words str
                        else postfixToPrefix [] $ toPostfix [] $ words str 

main :: IO ()
main = do
	args <- getArgs
	file <- readFile $ last args
	putStrLn $ toPrefix args (head $ lines file)
