module Pi4S3 where

import System.IO

import Math.Algebra.Group
import Math.Topology.SSet
import Math.Topology.SSet.Effective
import Math.Topology.SSet.Sphere
import Math.Topology.SSet.TwistedProduct
import Math.Topology.SGrp.Wbar
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SGrp.KGn

s3 = Sphere 3


type KZ2 = Wbar KZ1
type KZ3 = Wbar KZ2
-- kz1 = WbarDiscrete Z
kz2 = Wbar kz1
kz3 = Wbar kz2

classifying :: Morphism Sphere KZ3
classifying = Morphism m
  where m Cell = NonDegen $ WbarSimplex [
              NonDegen (WbarSimplex [NonDegen [1], NonDegen []]),
              Degen 0 (NonDegen (WbarSimplex [])),
              NonDegen (WbarSimplex [])
            ]
        m Basepoint = NonDegen $ WbarSimplex []

fibration :: Twist Sphere KZ2
fibration = pullback kz3 kz2 (canonicalTwist kz2) classifying

x :: TotalSpace Sphere KZ2
x = totalSpace s3 kz2 fibration

main = do
  putStrLn $ "π₄ S³ is: " ++ show (homology x !! 4)
