module Time where

import Control.Lens

data TimeSplice = TS { days :: Int
                     , hours :: Int
                     , minutes :: Int
                     , seconds :: Float
                  }
                  deriving (Show, Eq)

convert :: Iso TimeSplice TimeSplice Float Float
convert = iso (\(TS d h m s) -> fromIntegral d * 86400 + fromIntegral h * 3600 + fromIntegral m * 60 + s) 
            (\secs ->
            let
              s = secs - ((fromIntegral . (*60) . (`div` 60) . floor) secs)
              m = mod m' 60
              m' = div (floor secs) 60
              h = mod h' 24
              h' = div m' 60
              d = div h' 24
            in
              TS d h m s
            )

alterSeconds :: (Float -> Float) -> TimeSplice -> TimeSplice
alterSeconds = over convert
