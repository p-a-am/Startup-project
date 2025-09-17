-- MLBasic.hs
-- Core ML functions: Matrix ops, CNN layers, CSV loader
-- © 2025 Pam Durham. No unauthorized libraries used.

module MLBasic where

type Matrix = [[Double]]
type Vector = [Double]

-- === Basic Matrix Operations ===

dot :: Vector -> Vector -> Double
dot xs ys = sum (zipWith (*) xs ys)

matVecMul :: Matrix -> Vector -> Vector
matVecMul m v = map (`dot` v) m

matAddVec :: Matrix -> Vector -> Matrix
matAddVec m b = zipWith (zipWith (+)) m (replicate (length m) b)

-- === Convolution Layer ===

convolve2D :: Matrix -> Matrix -> Matrix
convolve2D input kernel =
  let (h, w) = (length input, length (head input))
      (kh, kw) = (length kernel, length (head kernel))
      outputH = h - kh + 1
      outputW = w - kw + 1
  in [ [ sum [ input !! (i + ki) !! (j + kj) * kernel !! ki !! kj
             | ki <- [0..kh-1], kj <- [0..kw-1] ]
       | j <- [0..outputW-1] ]
     | i <- [0..outputH-1] ]

-- === Activation Function ===

relu :: Matrix -> Matrix
relu = map (map (\x -> if x < 0 then 0 else x))

-- === Fully Connected Layer ===

fullyConnected :: Matrix -> Matrix -> Vector -> Vector
fullyConnected input weights bias =
  let flat = concat input
  in zipWith (+) (matVecMul weights flat) bias

-- === CNN Definition ===

data CNN = CNN {
    convKernel :: Matrix,
    fcWeights  :: Matrix,
    fcBias     :: Vector
} deriving Show

forward :: CNN -> Matrix -> Vector
forward cnn input =
  let conv = relu (convolve2D input (convKernel cnn))
  in fullyConnected conv (fcWeights cnn) (fcBias cnn)

-- === Output Interpretation ===

interpretOutput :: Vector -> [String]
interpretOutput vec = map (\(i, v) -> "Port " ++ show i ++ " → " ++ classify v) (zip [1..] vec)

classify :: Double -> String
classify x
  | x < 0.2   = "GND"
  | x < 0.5   = "COMM"
  | x < 0.8   = "VCC"
  | otherwise = "UNKNOWN"

-- === CSV Database Loader ===

loadCSV :: FilePath -> IO [(Bool, Bool, Double)]
loadCSV path = do
  content <- readFile path
  return $ map parseLine (lines content)

parseLine :: String -> (Bool, Bool, Double)
parseLine line =
  let [c1, c2, t] = splitByComma line
  in (readBool c1, readBool c2, read t)

splitByComma :: String -> [String]
splitByComma [] = []
splitByComma xs =
  let (pre, rest) = break (== ',') xs
  in pre : case rest of
    [] -> []
    (_:xs') -> splitByComma xs'

readBool :: String -> Bool
readBool s = s == "true" || s == "True"
