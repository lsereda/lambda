module Term where

data Term = V Char
          | App Term Term
          | Abs Char Term 

instance Show Term where
    show x = filter (isFromAlphabet termAlphabet) $ showFunc True x

showFunc :: Bool -> Term -> String
showFunc _ (V ch)               = show ch
showFunc _ (Abs arg right)      = "(" ++ "\\" ++ show arg ++ "." ++ showFunc False right ++ ")"
showFunc True (App left right)  = "(" ++ showFunc False (App left right) ++ ")"
showFunc False (App left right) = showFunc False left ++ showFunc True right

symbols :: String
symbols = ['a'..'z']

termAlphabet :: String
termAlphabet = "\\.()" ++ symbols

isFromAlphabet :: String -> Char -> Bool
isFromAlphabet alph x = x `elem` alph

intersection :: Eq a => [a] -> [a] -> [a]
intersection x y = [z | z <- x, z `elem` y]

difference :: Eq a => [a] -> [a] -> [a]
difference x y = [z | z <- x, z `notElem` y]

union :: Eq a => [a] -> [a] -> [a]
union x y = intersection x y ++ difference x y ++ difference y x

boundSymbols :: Term -> String
boundSymbols (V x)            = ""
boundSymbols (App left right) = intersection (boundSymbols left) (boundSymbols right)
boundSymbols (Abs arg right)  = arg : boundSymbols right

usedSymbols :: Term -> String
usedSymbols (V x)            = [x]
usedSymbols (App left right) = union (usedSymbols left) (usedSymbols right)
usedSymbols (Abs arg right)  = union [arg] (usedSymbols right)

freeSymbols :: Term -> String
freeSymbols t = difference (usedSymbols t) (boundSymbols t)

nonboundSymbols :: Term -> String
nonboundSymbols t = difference symbols (boundSymbols t)

substitute :: Term -> Char -> Term -> Term
substitute (V x) f s
    | x == f    = s
    | otherwise = V x
substitute (App left right) f s = App (substitute left f s) (substitute right f s)
substitute (Abs arg right) f s
    | arg == f  = Abs arg right
    | otherwise = Abs arg (substitute right f s)

-- modified version of alphaConversion where we convert certain variable, not the left-most
alphaConversion :: Term -> Char -> Char -> Term
alphaConversion (Abs arg right) x y
    | arg == x  = Abs y (substitute right x (V y))
    | otherwise = Abs arg (alphaConversion right x y)

symbolsToRename :: Term -> Term -> String
symbolsToRename x y = intersection (boundSymbols x) (freeSymbols y)

availableSymbols :: Term -> Term -> String
availableSymbols x y = difference (nonboundSymbols x) (freeSymbols y)

fixLeftSide :: Term -> Term
fixLeftSide (App (Abs x right) t) = App alphaReduced t
                                    where
                                      symbolsToChange = symbolsToRename (Abs x right) t
                                      available       = availableSymbols (Abs x right) t    
                                      alphaReduced    = fst $ foldl f (Abs x right, available) symbolsToChange
                                      f (t, y:ys) z   = (alphaConversion t z y, ys)

fixNamingConflict :: Term -> Term
fixNamingConflict term@(App (Abs _ _) _) = fixLeftSide term

isInNormalForm :: Term -> Bool
isInNormalForm (V _)              = True
isInNormalForm (Abs _ right)      = isInNormalForm right
isInNormalForm (App (Abs _ _) _ ) = False
isInNormalForm (App left right)   = isInNormalForm left && isInNormalForm right

betaReduction :: Term -> Term
betaReduction term@(App (Abs x right) t)
    | isCorrect = substitute right x t
    | otherwise = betaReduction (fixNamingConflict term)
    where
      isCorrect = null $ symbolsToRename (Abs x right) t

normalize :: Term -> Term
normalize t@(V _)             = t
normalize t@(Abs x right) 
    | isInNormalForm right    = Abs x right
    | otherwise               = normalize $ Abs x (normalize right)
normalize t@(App (Abs _ _) _) = normalize $ betaReduction t
normalize t@(App left right)
    | isInNormalForm t        = t
    | isInNormalForm left     = normalize $ App left (normalize right)
    | isInNormalForm right    = normalize $ App (normalize left) right
    | otherwise               = normalize $ App (normalize left) (normalize right)

splitByParams :: String -> (String, String)
splitByParams str
    | head rest == '.' = (arg, tail rest)
    | otherwise        = (arg, '\\':rest)
    where
      (arg, rest) = splitAt 1 str

extractBrackets :: String -> (String, String)
extractBrackets str = helper str 1 []
                      where
                        helper xs 0 acc       = ((reverse . tail) acc, xs)
                        helper ('(':xs) n acc = helper xs (n + 1) ('(':acc)
                        helper (')':xs) n acc = helper xs (n - 1) (')':acc)
                        helper (x:xs) n acc   = helper xs n (x:acc)

splitByTerms :: String -> [String]
splitByTerms []          = []
splitByTerms ('(':xs)    = inside : splitByTerms rightSide
                           where
                             (inside, rightSide) = extractBrackets xs 
splitByTerms t@('\\':xs) = [t]
splitByTerms (x:xs)      = [x] : splitByTerms xs

parse :: String -> Term
parse [x]       = V x
parse ('\\':xs) = Abs (head param) (parse rightSide)
                  where
                    (param, rightSide) = splitByParams xs
parse input     = foldl f (parse x) xs
                  where
                    (x:xs)    = splitByTerms input
                    f acc str = App acc (parse str) 

main :: IO ()
main = do
    term <- getLine
    print $ (normalize . parse) term