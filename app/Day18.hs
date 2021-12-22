module Day18 where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Parse (SnailfishNum(..))

solve :: [SnailfishNum] -> (Int, Int)
solve nums = (magnitude $ foldl1 add nums, largestSum nums)

largestSum :: [SnailfishNum] -> Int
largestSum nums = maximum do
  x <- nums
  y <- nums
  pure $ magnitude $ add x y

-- After an addition, we first need to reduce all explosions. After that, we
-- need to reduce splits but switch back to explosions if even one triggers.
-- Fortunately, they only trigger at the split location so we can resolve it
-- immediately and we won't need to switch back to explosions.

add :: SnailfishNum -> SnailfishNum -> SnailfishNum
add x y = reduceAllWith splitOne 4 $ reduceAllWith explode 4 $ Pair x y

magnitude :: SnailfishNum -> Int
magnitude (Leaf n) = n
magnitude (Pair x y) = 3 * magnitude x + 2 * magnitude y

half :: Int -> Int
half x = div x 2

half' :: Int -> Int
half' x = x - half x

addRight :: SnailfishNum -> Int -> SnailfishNum
addRight n 0 = n
addRight (Leaf n) a = Leaf (n + a)
addRight (Pair x y) a = Pair x (addRight y a)

addLeft :: Int -> SnailfishNum -> SnailfishNum
addLeft 0 n = n
addLeft a (Leaf n) = Leaf (n + a)
addLeft a (Pair x y) = Pair (addLeft a x) y

explosion :: Int -> SnailfishNum -> Int -> SnailfishNum
explosion 0 n 0 = n
explosion l n r = Explosion l n r

explode :: Int -> SnailfishNum -> Maybe SnailfishNum
explode 0 (Pair (Leaf x) (Leaf y)) = Just (explosion x (Leaf 0) y)
explode _ _ = Nothing

splitOne :: Int -> SnailfishNum -> Maybe SnailfishNum
splitOne 0 (Leaf n) | n > 9 = Just (Explosion (half n) (Leaf 0) (half' n))
splitOne _ (Leaf n) | n > 9 = Just (Pair (Leaf (half n)) (Leaf (half' n)))
splitOne _ _ = Nothing

-- In order to (marginally) increase efficiency, I decided to recompute only the
-- parts of the tree that changed. The reduceStepWith function takes a node and
-- tries to perform a step in it. If no step is found, it returns Nothing so the
-- reduceAllWith function knows it can stop iterating. If a step is found, the
-- resulting updated node is returned and reduceAllWith will try to improve it
-- again. This trick runs into an issue: Explosions can cause earlier nodes to
-- need updating. In order to resolve that, I represent Explosions explicitly in
-- the tree and set reduceStepWith to find no step in Explosions. Instead, it
-- will find a step in the parent Pair which contains the Explosion and can
-- propagate it until it is absorbed. At that point, reduceAllWith will continue
-- looking for steps starting from the parent node in which the Explosion was
-- absorbed.

reduceAllWith
  :: (Int -> SnailfishNum -> Maybe SnailfishNum)
  -> Int -> SnailfishNum -> SnailfishNum
reduceAllWith customStep depth n
  = maybe n (reduceAllWith customStep depth)
  $ reduceStepWith customStep depth n

-- It's important that reduceStepWith tries only one step at a time. If it
-- recurses to itself with a change and then finds no change, it will report no
-- change. The iteration must happen through reduceAllWith. Recursing to a
-- subtree is fine, as long as only a single step is found at a time.

reduceStepWith
  :: (Int -> SnailfishNum -> Maybe SnailfishNum)
  -> Int -> SnailfishNum -> Maybe SnailfishNum
reduceStepWith customStep 4 (Explosion _ x _) = Just x
reduceStepWith _ _ (Pair (Explosion l x r) y)
  = Just (explosion l (Pair x (r `addLeft` y)) 0)
reduceStepWith _ _ (Pair x (Explosion l y r))
  = Just (explosion 0 (Pair (x `addRight` l) y) r)
reduceStepWith customStep depth n = customStep depth n <|> case n of
  Pair x y -> case reduceStepWith customStep (depth - 1) x of
    Just x' -> Just (Pair x' y)
    Nothing -> case reduceStepWith customStep (depth - 1) y of
      Just y' -> Just (Pair x y')
      Nothing -> Nothing
  _ -> Nothing
