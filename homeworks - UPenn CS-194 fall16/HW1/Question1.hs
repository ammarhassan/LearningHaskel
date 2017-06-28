import CodeWorld

botCircle :: Color -> Picture
midCircle :: Color -> Picture
topCircle :: Color -> Picture


botCircle c = colored c (translated 0 (-3) (solidCircle 1))
topCircle c = colored c (translated 0  3  (solidCircle 1))
midCircle c = colored c (translated 0 (0) (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 9

trafficLightFunc :: Int -> Picture
trafficLightFunc 0 = topCircle red & midCircle black & botCircle black & frame
trafficLightFunc 1 = topCircle red & midCircle yellow & botCircle black & frame
trafficLightFunc 2 = topCircle black & midCircle black & botCircle green & frame
trafficLightFunc 3 = topCircle black & midCircle yellow & botCircle black & frame

trafficController :: Double -> Picture
trafficController t
  | round (5*t/5) `mod` 8 <= 3 = trafficLightFunc 0
  | round (5*t/5) `mod` 8 <= 4 = trafficLightFunc 1
  | round (5*t/5) `mod` 8 < 7 = trafficLightFunc 2
  | otherwise = trafficLightFunc 3

main :: IO ()
main = animationOf trafficController
