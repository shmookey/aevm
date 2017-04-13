module Fluidity.EVM.Analyse.Solver where

import Data.List (groupBy, maximum, minimum, sortOn)
import Data.Tuple (swap)





userVariable :: Expr -> Bool
userVariable expr = case expr of
  Var _   -> True
  Val _   -> False
  Add a b -> hasVar a || hasVar b
  Sub a b -> hasVar a || hasVar b
  Mul a b -> hasVar a || hasVar b
  Div a b -> hasVar a || hasVar b
  Eq  a b -> hasVar a || hasVar b

modulo = 2 ^ 256 

mod256 x = let r = x `mod` modulo
           in if r < 0 then r + modulo else r


--  Everything below appears to be a lost cause...
-- ---------------------------------------------------------------------




{-
rearrange2 :: (Expr, Expr) -> (Expr, Expr)
rearrange2 (l,r) = 
  rearrange (prepare $ Sub l r, Val 0)

-- | Attempt to place vars on the left and vals on the right
rearrange :: (Expr, Expr) -> (Expr, Expr)
rearrange (l,r) = case (l,r) of
  ( Add (Val c) e , _ ) -> rearrange ( e , Sub r (Val c) )
  ( Sub (Val c) e , _ ) -> rearrange ( e , Add r (Val c) )
  ( Mul (Val c) e , _ ) -> rearrange ( e , Div r (Val c) )
  ( Div (Val c) e , _ ) -> rearrange ( e , Div r (Val c) )
  ( Div e (Val c) , _ ) -> rearrange ( e , Mul r (Val c) )
  _                     -> (l,r)


prepare = rightAssoc . groupAssoc . leftify . simplify . distribute . simplify

-- | Replace all arithmetic operations on constants with their result
simplify :: Expr -> Expr
simplify expr = case expr of
  Var x               -> expr
  Val c               -> expr
  Add (Val a) (Val b) -> Val . mod256 $ a + b
  Sub (Val a) (Val b) -> Val . mod256 $ a - b
  Mul (Val a) (Val b) -> Val . mod256 $ a * b
  Div (Val a) (Val b) -> Val . mod256 $ a `div` b

  Add e1 e2 -> case (simplify e1, simplify e2) of
    (Val a, Val b) -> Val . mod256 $ a + b
    (e1', e2')     -> Add e1' e2'
  Sub e1 e2 -> case (simplify e1, simplify e2) of
    (Val a, Val b) -> Val . mod256 $ a - b
    (e1', e2')     -> Sub e1' e2'
  Mul e1 e2 -> case (simplify e1, simplify e2) of
    (Val a, Val b) -> Val . mod256 $ a * b
    (e1', e2')     -> Mul e1' e2'
  Div e1 e2 -> case (simplify e1, simplify e2) of
    (Val a, Val b) -> Val . mod256 $ a `div` b
    (e1', e2')     -> Div e1' e2'

-- Pulls all the `Val`s to the left of commutative operators
leftify :: Expr -> Expr
leftify expr = case expr of
  Var _               -> expr
  Val _               -> expr
  Add (Val a) e       -> Add (Val a)      (leftify e)
  Add e       (Val a) -> Add (Val a)      (leftify e)
  Add e1      e2      -> Add (leftify e1) (leftify e2)
  Sub (Val a) e       -> Sub (Val a)      (leftify e)
  Sub e       (Val a) -> Sub (Val a)      (leftify e)
  Sub e1      e2      -> Sub (leftify e1) (leftify e2)
  Mul (Val a) e       -> Mul (Val a)      (leftify e)
  Mul e       (Val a) -> Mul (Val a)      (leftify e)
  Mul e1      e2      -> Mul (leftify e1) (leftify e2)
  Div e1      e2      -> Div (leftify e1) (leftify e2)
  

-- | associate to the right
rightAssoc :: Expr -> Expr
rightAssoc expr = leftify $ case expr of
  Var _             -> expr
  Val _             -> expr
  Add (Add a b) c   -> rightAssoc $ Add a (Add b c) -- (a+b)+c == a+(b+c)
  Add (Sub a b) c   -> rightAssoc $ Add a (Sub c b) -- (a-b)+c == a+(c-b)
--  Add a (Add b c)   -> rightAssoc $ Add a (Add b c) -- a+(b+c) == a+(b+c)
--  Add a (Sub b c)   -> rightAssoc $ Add a (Sub c b) -- (a-b)+c == a+(c-b)
  Sub (Sub a b) c   -> rightAssoc $ Sub a (Sub b c) -- (a-b)-c == a-(b-c)
  Sub (Add a b) c   -> rightAssoc $ Sub a (Add b c) -- (a+b)-c == a+(b-c)
  Mul (Mul a b) c   -> rightAssoc $ Mul a (Mul b c) -- (a*b)*c == a*(b*c)
  Mul (Div a b) c   -> rightAssoc $ Mul a (Div c b) -- (a/b)*c == a*(c/b)
  Div (Mul a b) c   -> rightAssoc $ Mul a (Div b c) -- (a*b)/c == a*(b/c)
  Div (Div a b) c   -> rightAssoc $ Div a (Mul b c) -- (a/b)/c == a/(b*c)
  Add a b           -> Add a $ rightAssoc b
  Sub a b           -> Sub a $ rightAssoc b
  Mul a b           -> Mul a $ rightAssoc b
  Div a b           -> Div a $ rightAssoc b

-- | associate to the right
groupAssoc :: Expr -> Expr
groupAssoc expr = leftify $ case expr of
  Var _                   -> expr
  Val _                   -> expr
  Add (Mul x y) (Add a b) -> groupAssoc $ Add (Add a b) (Mul x y)
  Add (Div x y) (Add a b) -> groupAssoc $ Add (Add a b) (Div x y)
  Add (Mul x y) (Sub a b) -> groupAssoc $ Add (Sub a b) (Mul x y)
  Add (Div x y) (Sub a b) -> groupAssoc $ Add (Sub a b) (Div x y)
  Sub (Mul x y) (Add a b) -> groupAssoc $ Sub (Val 0) (Sub (Add a b) (Mul x y))
  Sub (Div x y) (Add a b) -> groupAssoc $ Sub (Val 0) (Sub (Add a b) (Div x y))
  Sub (Mul x y) (Sub a b) -> groupAssoc $ Sub (Val 0) (Sub (Sub a b) (Mul x y))
  Sub (Div x y) (Sub a b) -> groupAssoc $ Sub (Val 0) (Sub (Sub a b) (Div x y))
  Mul (Add a b) (Mul x y) -> groupAssoc $ Mul (Mul x y) (Add a b)
  Mul (Sub a b) (Mul x y) -> groupAssoc $ Mul (Mul x y) (Sub a b)
  Mul (Add a b) (Div x y) -> groupAssoc $ Mul (Div x y) (Add a b)
  Mul (Sub a b) (Div x y) -> groupAssoc $ Mul (Div x y) (Sub a b)
  Add a b                 -> Add (groupAssoc a) (groupAssoc b) 
  Sub a b                 -> Sub (groupAssoc a) (groupAssoc b) 
  Mul a b                 -> Mul (groupAssoc a) (groupAssoc b) 
  Div a b                 -> Div (groupAssoc a) (groupAssoc b) 

distribute :: Expr -> Expr
distribute expr = case expr of -- Ensures that anything that doesn't have a var in it will be a val
  Val _ -> expr
  Var _ -> expr

  Mul (Add e1 e2) e3 -> let e1' = distribute e1
                            e2' = distribute e2
                            e3' = distribute e3
                            m1  = distribute $ Mul e1' e3'
                            m2  = distribute $ Mul e2' e3'
                        in Add m1 m2
  Mul (Sub e1 e2) e3 -> let e1' = distribute e1
                            e2' = distribute e2
                            e3' = distribute e3
                            m1  = distribute $ Mul e1' e3'
                            m2  = distribute $ Mul e2' e3'
                        in Sub m1 m2
  Mul e1 (Add e2 e3) -> distribute $ Mul (Add e2 e3) e1
  Mul e1 (Sub e2 e3) -> distribute $ Mul (Sub e2 e3) e1
  Mul e1 e2          -> Mul (distribute e1) (distribute e2)
  Add e1 e2          -> Add (distribute e1) (distribute e2)
  Sub e1 e2          -> Sub (distribute e1) (distribute e2)

---- Move vars leftwards and upwards
--promote :: Expr -> (Expr -> Expr, Expr -> Expr)
--promote expr = case simplify expr of
--  -- Terminating cases that we can't do anything with:
--  Var x               -> (expr, id)
--  Val c               -> (id, expr)
--  Add 
--
--
--  Add (Var x) e       -> Add (Var x) (promote e)
--  Add e       (Var x) -> Add (Var x) (promote e)
--  Add e1      e2      -> case (promote e1, promote e2) of
--                           
--
--  Sub (Var _) (Var _) -> expr
--  Mul (Var _) (Var _) -> expr
--  Div (Var _) (Var _) -> expr
--  Add (Var _) (Val _) -> expr
--  Sub (Var _) (Val _) -> expr
--  Mul (Var _) (Val _) -> expr
--  Div (Var _) (Val _) -> expr
--
--  -- Terminating cases that we can move Vars left with:
--  Add (Val c) (Var x) -> Add (Var x) (Val c)
--  Sub (Val c) (Var x) -> Sub (Var x) (Val c)
--  Mul (Val c) (Var x) -> Mul (Var x) (Val c)
--  Div (Val c) (Var x) -> Div (Var x) (Val c)
--
--  -- Recursive cases that already have vars on the left:


-}





--type Range = (Integer, Integer)
--
--max_int     = 2 ^ 256 - 1
--full        = (0, max_int)
--width (a,b) = b - a
--
--range :: Expr -> [Range]
--range expr = case expr of
--  Var _   -> [full]
--  Val x   -> [(x, x)]
--  Add a b -> combine . concat $ addRanges 8 <$> range a <*> range b
--  Sub a b -> combine . concat $ subRanges 8 <$> range a <*> range b
--  Div a b -> combine . concat $ divRanges 8 <$> range a <*> range b
--
---- Eliminate overlapping ranges
--combine :: [Range] -> [Range]
--combine rs =
--  let
--    rs' = sortOn fst rs
--    gs  = groupBy (\(a1,b1) (a2,b2) -> a2 <= b1) rs'
--    f g = (minimum (map fst g), maximum (map snd g))
--  in
--    map f gs
--
---- | The ranges of values attainable by the integer division of two other values, given their ranges
--divRanges :: Int -> Range -> Range -> [Range]
--divRanges sz (a1, b1) (a2, b2) = [(a1 `div` a2, b1 `div` b2)]
--
---- | The ranges of values attainable by the subtraction of two other values, given their ranges
--subRanges :: Int -> Range -> Range -> [Range]
--subRanges sz (a1, b1) (a2, b2) =
--  let a = a1 - a2
--      b = b1 - b2
--      m = 2 ^ sz
--      max_int = m - 1
--  in if a >= 0
--     then if b >= 0
--          then [(a, b)]
--          else error "invalid range"
--     else if b <= 0
--          then [(a `mod` m + m, b `mod` m + m)]
--          else [(a `mod` m + m, max_int), (0, b)]
--
---- | The ranges of values attainable by the addition of two other values, given their ranges
--addRanges :: Int -> Range -> Range -> [Range] 
--addRanges sz (a1, b1) (a2, b2) =
--  let a = a1 + a2
--      b = b1 + b2
--      m = 2 ^ sz
--      max_int = m - 1
--  in if a <= max_int
--     then if b <= max_int
--          then [(a, b)]
--          else [(a, max_int), (0, b `mod` m)]
--     else if b >= max_int
--          then [(a `mod` m, b `mod` m)]
--          else error "invalid range"
        
    

{-
   x = y >> 0  x:(0,   2^256)
   x = y >> 1  x:(0,   2^255)
   x = y >> n  x:(0,   2^(256-n))

   x = y << 0  x:(0,   2^256)  
   x = y << 1  x:(2,   2^256)  
   x = y << n  x:(2^n, 2^256)

   x:(a, a)
   y:(b, b)
   (x + y):(a + b, a + b)

   x:(a,b)
   y:(c,c)
   (x+y):(a+c,b+c)

-}
