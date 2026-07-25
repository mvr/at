module Pi7S3 where

import Math.Topology.SSet.Sphere
import Math.Topology.SSet.Whitehead

s3 :: Sphere
s3 = Sphere 3

main :: IO ()
main = case homotopyGroup 7 s3 of
  Left err -> print err
  Right group -> putStrLn $ "pi_7(S^3) is: " ++ show group
