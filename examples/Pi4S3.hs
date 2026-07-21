module Pi4S3 where

import Math.Topology.SSet.Sphere
import Math.Topology.SSet.Whitehead

s3 = Sphere 3

main :: IO ()
main = case homotopyGroup 4 s3 of
  Left err -> print err
  Right group -> putStrLn $ "pi_4(S^3) is: " ++ show group
