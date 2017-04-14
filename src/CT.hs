module CT where

import Data.Array         as A
import Data.IntMap.Strict as I
import Data.List          as L (foldl')
import Data.Set           as S


type Dim    = (Int, Int, Int)
type Vertex = Int
type Coord  = (Int, Int, Int)

data Dataset sc =
  Dataset {
     dim    :: Dim,
     mesh   :: Vertex -> [Vertex],
     scalar :: Vertex -> sc
  }

instance Functor Dataset where
  fmap f (Dataset dim mesh scalar) = Dataset dim mesh (f . scalar)

vertexRange :: Dim -> Int
vertexRange (dimX, dimY, dimZ) = dimX * dimY * dimZ - 1

coord2vert :: Dim -> Coord -> Vertex
coord2vert (dimX, dimY, dimZ) (x,y,z) = case (x,y,z) of
  (0,0,0) -> 0
  (x,0,z) -> z * dimX * dimY + x
  (x,y,0) -> y * dimX + x
  (0,y,z) -> z * dimX * dimY + y * dimX
  (x,y,z) -> z * dimX * dimY + y * dimX + x

vert2coord :: Dim -> Vertex -> Coord
vert2coord dim@(dimX, dimY, dimZ) vert
  | vert == 0  = (0,0,0)
  | otherwise  = (x, y, z)
    where
      z     = head [ n - 1 | n <- [1..], n * (dimX*dimY) > vert]
      vert' = vert - (dimX*dimY) * z
      y     = head [ n - 1 | n <- [1..], n * dimX > vert']
      x     = vert' - dimX * y


-- construct datasets
dataset :: Dim -> (Coord -> [Coord]) -> [sc] -> Dataset sc
dataset dim nbhood samples = Dataset dim (neighbours dim nbhood) ((A.!)array)
    where
      array                    = A.listArray (0, vertexRange dim) samples
      neighbours :: Dim -> (Coord -> [Coord]) -> (Vertex -> [Vertex])
      neighbours dim nbhood vertex = Prelude.map (coord2vert dim) (nbhood $ vert2coord dim vertex)


neighbourhood :: Dim -> Coord -> [Coord]
neighbourhood (dimX, dimY, dimZ) coord@(x,y,z) = Prelude.filter edgeCase [ (x+i, y+j, z+k) | i <- [-1,0,1], j <- [-1,0,1], k <- [-1,0,1] ]
    where
      edgeCase = \v@(x',y',z') -> (x' >= 0 && y' >= 0 && z' >= 0) && coord /= v


type Node = Int
type EdgeList = [(Node, Node)]
data Adjacency = Adj { inc :: [Node], out :: [Node] }
type Graph = I.IntMap Adjacency

type Component = S.Set Vertex

data JoinInfo =
  JoinInfo {
     disj :: I.IntMap Component, -- Vertex -> Component[Vertex]
     tree :: EdgeList, -- the current tree
     base :: Component -> Vertex, -- lowest
     seen :: S.Set Vertex -- mark which has been already seen
  }

initJoin :: [Vertex] -> JoinInfo
initJoin vertices = JoinInfo { disj = I.fromList [ (v, S.singleton v) | v <- vertices ], tree = [], base = head . S.toList, seen = S.empty } -- TODO: disj, base correct?

mark :: Vertex -> JoinInfo -> JoinInfo
mark v (JoinInfo disj tree base seen) = JoinInfo disj tree base (S.insert v seen)

foreach :: [a] -> b -> (b -> a -> b) -> b
foreach xs acc f = L.foldl' f acc xs


set :: (Component -> Vertex) -> Component -> Vertex -> (Component -> Vertex)
set lowest cp v = \x -> if   x == cp
                        then v
                        else lowest x

joinTree :: [Vertex] -> (Vertex -> [Vertex]) -> EdgeList
joinTree vertices adjacent = tree $ foreach vertices (initJoin vertices) $ \jI v ->
    foreach (adjacent v) (mark v jI) $ \jI n ->

      let components   = disj jI
          lowest       = base jI
          comp s       = case I.lookup s components of
                         (Just c) -> c
                         Nothing  -> S.empty
          compV        = comp v
          compN        = comp n
          replaceUnion = \_ -> S.union compV compN
          u            = lowest compN
      in if ( n `S.member` (seen jI) ) && (compV /= compN)
         then
               jI {
                  disj = I.adjust replaceUnion v components,
                  tree = (u,v) : (tree jI),
                  base = set (set lowest compV v) compN v,
                  seen = S.insert n (seen jI)
               }
         else jI

splitTree :: [Vertex] -> (Vertex -> [Vertex]) -> EdgeList
splitTree xs adjacent = undefined

contourTree :: Graph -> Graph -> EdgeList
contourTree = undefined

dim1  = (3,3,3)
vert1 = reverse [1..9]
ds1   = dataset dim1 (neighbourhood dim1) vert1
tree1 = joinTree vert1 (mesh ds1)

dim2  = (5,5,5)
vert2 = reverse [1..5^3]
ds2   = dataset dim2 (neighbourhood dim2) vert2
tree2 = joinTree vert2 (mesh ds2)
