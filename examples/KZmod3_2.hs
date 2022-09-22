module KZmod3_2 where

import System.IO

import Math.Algebra.Group
import Math.Topology.SGrp.Wbar
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SGrp.KGn
import Math.Topology.SSet.Effective

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  print (homology (Wbar (WbarDiscrete (Zmod 3))))
