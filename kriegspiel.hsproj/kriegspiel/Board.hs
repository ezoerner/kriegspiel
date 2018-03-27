module Board where

import Foundation (ifThenElse)  
import Helm.Graphics2D
import Helm.Asset
import Helm.Engine (Engine)
import Linear.V2 (V2(V2))

form :: Engine e => Image e -> Image e -> Int -> Form e
form lightSquare darkSquare boardSide = let
    squareSide = fromIntegral boardSide / 8
    imageDims = V2 squareSide squareSide
    chooseImage x y = ifThenElse ((x + y) `mod` 2 == 0) lightSquare darkSquare
    mkForm x y = image imageDims $ chooseImage x y
  in
    toForm $ collage [move (V2 hOffset vOffset) $ mkForm (floor x) $ floor y |
                        x <- [0..7]
                      , y <- [0..7]
                      , let hOffset = x * squareSide
                      , let vOffset = y * squareSide]                 
