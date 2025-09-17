-- MLMain.hs
-- Orchestration & debugging
-- No unauthorized libraries. © 2025 You.

module Main where

import MLBasic
import MLCode
import MLCloud

main :: IO ()
main = do
  putStrLn "[MLMain] Starting system..."
  myID <- generateID
  putStrLn $ "[MLMain] This week's ID: " ++ myID

  peers <- cloudSync myID
  case peers of
    Just ids -> putStrLn $ "[MLMain] Peer IDs: " ++ show ids
    Nothing -> putStrLn "[MLMain] No peers found."

  let kernel = [[1, 0], [0, -1]]
      fcWeights = replicate 3 (replicate 4 0.5)
      fcBias = [0.1, 0.2, 0.3]
      cnn = CNN kernel fcWeights fcBias

  runInference cnn
  putStrLn "[MLMain] Done."

-- === Generate 16-character hex ID ===

generateID :: IO String
generateID = do
  let timeSeed = 1736800000
  return (toHex16 (timeSeed `mod` 0xFFFFFFFFFFFFFFFF))

toHex16 :: Integer -> String
toHex16 n =
  let h = toHex n
  in replicate (16 - length h) '0' ++ h

toHex :: Integer -> String
toHex 0 = "0"
toHex n = reverse (toHex' n)
  where
    toHex' 0 = ""
    toHex' x = let d = x `mod` 16 in hexChar d : toHex' (x `div` 16)
    hexChar d
      | d < 10    = head (show d)
      | otherwise = ['a'..'f'] !! (fromIntegral d - 10)
