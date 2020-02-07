module Core.PlanarDiagram.Operations
where

class DisjointUnionable a where
  dUnion :: a -> a -> a
