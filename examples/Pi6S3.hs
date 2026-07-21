module Pi6S3 where

import Math.Topology.SSet.Sphere
import Math.Topology.SSet.Whitehead

s3 :: Sphere
s3 = Sphere 3

main :: IO ()
main = case homotopyGroup 6 s3 of
  Left err -> print err
  Right group -> putStrLn $ "pi_6(S^3) is: " ++ show group
