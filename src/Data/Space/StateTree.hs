module Data.Space.StateTree where

import Data.Semigroup

data STree h a
  = Halt h a
  | Branch (Int, STree h a) a (Int, STree h a)
  deriving (Show)

instance Functor (STree h) where
  fmap f fa = case fa of
    Halt x k     -> Halt x (f k)
    Branch (li, l) k (ri, r) -> Branch (li, fmap f l) (f k) (ri, fmap f r)

class Functor w => Comonad w where
  coreturn :: w a -> a
  cojoin   :: w a -> w (w a)

  (=>>)    :: w a -> (w a -> b) -> w b
  x =>> f = fmap f (cojoin x)

instance Comonad (STree h) where
  coreturn fa = case fa of
    Halt _ k     -> k
    Branch _ k _ -> k

  cojoin fa = case fa of
    Halt x _     -> Halt x fa
    Branch (li, l) _ (ri, r) -> Branch (li, cojoin l) fa (ri, cojoin r)

instance Foldable (STree h) where
  foldr f z fa = case fa of
    Halt _ k     -> f k z
    Branch (_, l) k (_, r) -> foldr f (f k (foldr f z r)) l


fmaph :: (h -> h') -> STree h a -> STree h' a
fmaph f fa = case fa of
  Halt x k     -> Halt (f x) k
  Branch (li, l) k (ri, r) -> Branch (li, fmaph f l) k (ri, fmaph f r)

--select :: (a -> Bool) -> STree h a -> [STree h a]
--select f fa = select_ [] fa
--  where
--    select_ acc fa = case fa of
--      Halt _ _     -> acc
--      Branch l _ r -> if f (coreturn fa)
--                      then fa : select_ (select_ acc r) l
--                      else select_ (select_ acc r) l

prune :: (a -> Bool) -> STree h a -> STree h a -> STree h a
prune f z fa = 
  if not (f $ coreturn fa) then z
  else case fa of
    Halt _ _     -> fa
    Branch (li, l) k (ri, r) -> Branch (li, prune f z l) k (ri, prune f z r)

pruneWithDepth :: (Int -> a -> Bool) -> STree h a -> STree h a -> STree h a
pruneWithDepth f z fa = pruneWithDepth_ 0 fa
  where
    pruneWithDepth_ n fa =
      if not (f n $ coreturn fa) then z
      else case fa of
        Halt _ _     -> fa
        Branch (li, l) k (ri, r) -> Branch (li, pruneWithDepth_ (n+1) l) k (ri, pruneWithDepth_ (n+1) r)

foldTraverse :: (b -> Int -> STree h a -> (b, Maybe (STree h a))) -> b -> STree h a -> (b, STree h a)
foldTraverse f acc fa = traverse_ acc 0 fa
  where
    traverse_ acc n fa = case f acc n fa of
      (acc1, Just fa') -> (acc1, fa')
      (acc1, Nothing)  -> case fa of
        Halt _ _     -> (acc1, fa)
        Branch (li, l) k (ri, r) ->
          let (acc2, l') = traverse_ acc1 (n+1) l
              (acc3, r') = traverse_ acc2 (n+1) r
          in (acc3, Branch (li, l') k (ri, r'))

--deduplicate :: STree h a -> STree h a -> 
--deduplicateOn :: Eq b => (a -> b) -> STree h a -> STree h a -> ([b], STree h a)
--deduplicateOn f z = foldTraverse g []
--  where
--    g acc _ fa = case fa of
--      Halt _ k     -> chk acc k
--      Branch _ k _ -> chk acc k
--    chk acc k = 
--      let r = f k
--      in if r `elem` acc
--         then (acc, Just z)
--         else (r : acc, Nothing)

pruneTo :: Int -> STree h a -> STree h a -> STree h a
pruneTo n = pruneWithDepth (\i -> const (i <= n))

filterEnd :: Semigroup a => (h -> Bool) -> STree h a -> Maybe (STree h a)
filterEnd f fa = case fa of
  Halt x _     -> if f x then Just fa else Nothing
  Branch (li, l) k (ri, r) -> case (filterEnd f l, filterEnd f r) of
    (Just l', Just r') -> Just $ Branch (li, l) k (ri, r)
    (Just l', Nothing) -> Just $ shallowMap (k <>) l'
    (Nothing, Just r') -> Just $ shallowMap (k <>) r'
    (Nothing, Nothing) -> Nothing

shallowMap :: (a -> a) -> STree h a -> STree h a
shallowMap f fa = case fa of
  Halt x k     -> Halt x (f k)
  Branch l k r -> Branch l (f k) r

paths :: STree h a -> [([a], h)]
paths fa = paths_ [] [] fa
  where
    paths_ :: [a] -> [([a], h)] -> STree h a -> [([a], h)]
    paths_ acc lists fa = case fa of
      Halt x k     -> (k:acc, x):lists
      Branch (_, l) k (_, r) -> 
        let
          acc'   = k:acc
          lists' = paths_ acc' lists l
        in
          paths_ acc' lists' r

