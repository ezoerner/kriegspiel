{-# LANGUAGE NamedFieldPuns #-}

module BoardView where

import           Chess
import           Control.Applicative (pure)
import           Control.Monad (guard)
import           Data.Array as A
import           Data.Char (ord, chr, toLower)
import           Data.List (map, sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isJust)
import           Data.Maybe.HT (toMaybe)
import           Helm
import qualified Helm.Color as HelmColor
import           Helm.Engine (Engine)
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Graphics2D as HGfx
import qualified Helm.Graphics2D.Text as Text
import           Linear.V2 (V2(V2))

import           ChessUtils
import           Options

-- for this game a bounding box is always square
data BoundingSquare = BSquare
    { width :: !Double
    , topLeft :: !(V2 Double)
    } deriving (Show)

data BoardView = BoardView
    { bbox :: !BoundingSquare
    , orient :: !Color
    , posInMotion :: !(Maybe BoardPosition)
    }
    deriving (Show)

initialBoardView :: V2 Int -> BoardView
initialBoardView windowDims =
    BoardView
    { bbox = calcBoardBBox windowDims
    , orient = White
    , posInMotion = Nothing
    }

border :: Num a => V2 a
border = V2 100 100 

calcBoardBBox :: (Integral a, Ord a) => V2 a -> BoundingSquare
calcBoardBBox windowSize =
  let
    constrainSquare (V2 x y) = x `min` y
    calcBoardSize = constrainSquare . subtract (2 * border)
  in
    BSquare { width = fromIntegral $ calcBoardSize windowSize, topLeft = border}

findPositionWithPiece :: Board -> BoardView -> V2 Double -> Color -> Maybe BoardPosition
findPositionWithPiece bord BoardView{bbox, orient} point playerTurn =
  let
    maybeBoardPos = toBoardPosition bbox point orient
  in
    maybeBoardPos >>= \testPos ->
        bord `pieceAt` toCoord testPos >>= \(Piece color _) ->
        guard (color == playerTurn) >>
        return testPos

toBoardPosition :: BoundingSquare -> V2 Double -> Color -> Maybe BoardPosition
toBoardPosition bbox (V2 x y) playerOrient = let
    ssize = squareSize bbox
    tryPos White = (chr $ ord 'a' + floor (x / ssize), 8 - floor (y / ssize))
    tryPos Black = (chr $ ord 'a' + floor (x / ssize), floor (y / ssize) + 1)
    thisTryPos = tryPos playerOrient
  in
    isOnBoard thisTryPos `toMaybe` thisTryPos

toBoardLocal :: V2 Double -> BoundingSquare -> V2 Double
toBoardLocal globalV2 bbox = globalV2 - topLeft bbox

toCoord :: BoardPosition -> String
toCoord (file, rank) = file : show rank

toCoordMove :: BoardPosition -> BoardPosition -> String
toCoordMove fromPos toPos = toCoord fromPos ++ "-" ++ toCoord toPos

findPawnTries :: GameState -> Color -> [BoardPosition]
findPawnTries gameState thisPlayer =
    [ pawnTry |
        (coords@(coordRank, coordFile), square)
            <- A.assocs $ board gameState,
        square /= Empty,
        let (Piece color pieceType) = fromMaybe undefined (squareToMaybe square),
        pieceType == Pawn,
        color == thisPlayer,
        let direction = if thisPlayer == White then -1 else 1,
        pawnTryCoords <- [ (coordRank + direction, coordFile - 1)
                        , (coordRank + direction, coordFile + 1)
                        ],
        let pawnTry = coordsToBoardPosition pawnTryCoords,
        let fromPos = coordsToBoardPosition coords,
        let moveSpec = toCoordMove fromPos pawnTry,
        isLegalMove gameState moveSpec
    ]

overlay :: HelmColor.Color -> BoardView -> GameState -> MoveAttempt -> Maybe Check -> Maybe GameOver -> HGfx.Form SDLEngine
overlay helmColor BoardView{bbox=BSquare{width, topLeft = (V2 left top)}}
    gameState lastMoveAttempt maybeCheck maybeGameOver =
  let
    currPlayer = currentPlayer gameState
    topX = width / 2 + left
    topY = top / 2
    sidebarX = width + left + 100
    sidebarY = top + 15
    textHeight = 30
    showLastMoveAttempt Successful = ""
    showLastMoveAttempt Illegal = "No"
    showLastMoveAttempt Impossible = "Hell, No!"
    showToMove = case maybeGameOver of
        Nothing -> " To Move: " ++ show currPlayer
        Just gameOver -> show gameOver -- TO DO improve this
    showCheck (Just LongDiagonal) = "Check on long diagonal"
    showCheck (Just ShortDiagonal) = "Check on short diagonal"
    showCheck (Just KnightCheck) = "Check from a Knight"
    showCheck (Just ckType) = show ckType ++ " check"
    showCheck Nothing = ""
  in
    HGfx.group [ HGfx.move (V2 topX topY) $ HGfx.text $ Text.height textHeight $
                Text.color helmColor $
                Text.toText showToMove
          , HGfx.move (V2 sidebarX sidebarY) $ HGfx.text $ Text.height textHeight $
                Text.color helmColor $ Text.toText $ showCheck (checkType <$> maybeCheck)
          , HGfx.move (V2 sidebarX $ sidebarY + textHeight) $ HGfx.text $ Text.height textHeight $
                Text.color helmColor $
                Text.toText $ showLastMoveAttempt lastMoveAttempt
          ]

boardForm :: Engine e => Image e -> Image e -> BoardView -> HGfx.Form e
boardForm lightSquare darkSquare BoardView{bbox, orient} =
  let
    ssize = squareSize bbox
    imageDims = V2 ssize ssize
    pivot White = 0
    pivot Black = 1
    chooseImage x y = if floor (x + y) `mod` (2 :: Integer) == pivot orient
                      then lightSquare
                      else darkSquare
    mkForm x y = HGfx.image imageDims $ chooseImage x y
  in
    HGfx.toForm $ HGfx.collage [HGfx.move (V2 hOff vOff) $ mkForm x y
                                | x <- [0..7]
                                , y <- [0..7]
                                , let hOff = x * ssize
                                , let vOff = y * ssize
                                ]

piecesForm :: Engine e => GameState -> Options -> BoardView ->
    M.Map String (Image e) -> V2 Int -> HGfx.Form e
piecesForm gameState Options{gameVariant} BoardView{bbox, orient, posInMotion} assets mousePos =
  let
    showColor color = toLower (head $ show color)
    showPieceType pieceType = toLower <$> show pieceType
    pieceName (Piece color pieceType) = showColor color : "_" ++ showPieceType pieceType
    chooseImage piece = assets M.! pieceName piece
    ssize = squareSize bbox
    imageDims = pure ssize
    mkForm piece = HGfx.image imageDims $ chooseImage piece

    pieceImage boardPosition piece
        | posInMotion == Just boardPosition = 
            HGfx.move (toBoardLocal (fromIntegral <$> mousePos) bbox - imageDims / 2) $ mkForm piece
        | otherwise = 
            HGfx.move (toOffset boardPosition orient ssize) $ mkForm piece

    pieces = [(coordsToBoardPosition coords, piece) |
                (coords, square) <- assocs $ board gameState,
                let maybePiece = squareToMaybe square,
                isJust maybePiece,
                -- the undefined here is filtered out by above guard
                let piece@(Piece color _) = fromMaybe undefined maybePiece,
                gameVariant == Chess ||
                    isGameOver gameState ||
                    currentPlayer gameState == color]
    sortedPieces = sortOn ((== posInMotion) . Just . fst) pieces
    imageCollage = HGfx.collage $ map (uncurry pieceImage) sortedPieces
  in
    HGfx.toForm imageCollage

-- Private functions
squareToMaybe :: Square -> Maybe Piece
squareToMaybe Empty = Nothing
squareToMaybe (Square piece) = Just piece

isOnBoard :: BoardPosition -> Bool
isOnBoard pos = fst pos >= fst minPosition &&
                snd pos >= snd minPosition &&
                fst pos <= fst maxPosition &&
                snd pos <= snd maxPosition

minPosition :: BoardPosition
minPosition = ('a', 1)

maxPosition :: BoardPosition
maxPosition = ('h', 8)

squareSize :: BoundingSquare -> Double
squareSize bbox = width bbox / 8

-- Color: orientation of the board
toOffset :: BoardPosition -> Color -> Double -> V2 Double
toOffset (file, rank) White ssize =
    (fromIntegral <$> V2 (ord file - ord 'a') (8 - rank)) * pure ssize
toOffset (file, rank) Black ssize =
    (fromIntegral <$> V2 (ord file - ord 'a') (rank - 1)) * pure ssize

coordsToBoardPosition :: Coordinates -> BoardPosition
coordsToBoardPosition (coordRank, coordFile) =
    (chr $ ord 'a' + coordFile, 8 - coordRank)
