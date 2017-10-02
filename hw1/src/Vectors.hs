module Vectors
       ( Vector (..)
       , squaredLen
       , len
       , addVec
       , scalarProduct
       , negateVec
       , subVec
       , squaredDist
       , dist
       , crossProduct
       , toList
       , fromList
       ) where


data Vector a = Vector2D a a | Vector3D a a a
  deriving Show

squaredLen :: (Num a) => Vector a -> a
squaredLen v = scalarProduct v v

len :: (Real a, Floating b) => Vector a -> b
len = sqrt . realToFrac . squaredLen

addVec :: (Num a, Eq a) => Vector a -> Vector a -> Vector a
addVec x y = fromListSimplify $ zipWith (+) (toNormalizedList x) (toNormalizedList y)

scalarProduct :: (Num a) => Vector a -> Vector a -> a
scalarProduct x y = sum $ zipWith (*) (toNormalizedList x) (toNormalizedList y)

negateVec :: (Num a) => Vector a -> Vector a
negateVec = fromList . (map negate) . toList

subVec :: (Num a, Eq a) => Vector a -> Vector a -> Vector a
subVec x y = addVec x (negateVec y)

squaredDist :: (Num a, Eq a) => Vector a -> Vector a -> a
squaredDist x y = squaredLen $ subVec x y

dist :: (Real a, Floating b) => Vector a -> Vector a -> b
dist x y = sqrt . realToFrac $ squaredDist x y

crossProduct :: (Num a, Eq a) => Vector a -> Vector a -> Vector a
crossProduct x y = cp (normalize x) (normalize y)
  where
    cp (Vector3D u1 u2 u3) (Vector3D v1 v2 v3) = Vector3D (u2 * v3 - u3 * v2)
                                                          (u1 * v3 - u3 * v1)
                                                          (u1 * v2 - u2 * v1)
    cp _ _ = error "Could not happen"

toList :: Vector a -> [a]
toList (Vector2D x y)   = [x, y]
toList (Vector3D x y z) = [x, y, z]

fromList :: [a] -> Vector a
fromList [x, y]    = Vector2D x y
fromList [x, y, z] = Vector3D x y z
fromList _         = error "Supports only 2D/3D Vectors"

normalize :: (Num a) => Vector a -> Vector a
normalize (Vector2D x y) = Vector3D x y 0
normalize v              = v

simplify :: (Num a, Eq a) => Vector a -> Vector a
simplify (Vector3D x y 0) = Vector2D x y
simplify v                = v 

toNormalizedList :: (Num a) => Vector a -> [a]
toNormalizedList = toList . normalize

fromListSimplify :: (Num a, Eq a) => [a] -> Vector a
fromListSimplify = simplify . fromList
