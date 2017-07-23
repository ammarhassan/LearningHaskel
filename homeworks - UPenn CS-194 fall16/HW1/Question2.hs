import CodeWorld

tree :: Integer -> Picture -> Picture
tree n flower
	| n == 0 = flower
	| n > 0 = path [(0,0),(0,1)] & translated 0 1 (
    	rotated (pi/10) (tree (n-1) flower) & rotated (- pi/10) (tree (n-1) flower))

yellowcircle :: Double -> Picture
yellowcircle t = colored yellow (solidCircle (t/(5*t+10)))

bloomer :: Double -> Picture
bloomer t = tree 8 (yellowcircle t)

main :: IO ()
main = animationOf bloomer