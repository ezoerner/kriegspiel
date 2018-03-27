module Board where
  
import Linear.V2 (V2(V2))
import Helm.Graphics2D
import Helm.Asset
import Helm.Engine (Engine)

form :: Engine e => Image e -> Image e -> Int -> Form e
form lightSquare darkSquare boardSide = 
    toForm $ collage [move (V2 h v) (mkForm (toInteger x) (toInteger y)) |
                         x <- [0..7]
                       , y <- [0..7]
                       , let h = x * squareSide
                       , let v = y * squareSide]
  where
    squareSide = fromIntegral boardSide / 8
    imageDims = (V2 squareSide squareSide)
    chooseImage x y = if (x + y) `mod` 2 == 0
                        then lightSquare
                        else darkSquare
    mkForm x y = image imageDims $ chooseImage x y

                       
