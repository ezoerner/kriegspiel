{-# LANGUAGE NamedFieldPuns #-}

module BoardView where

import           Chess
import           Control.Applicative (pure)
import           Control.Monad (guard)
import           Data.Array as A
import           Data.Char (ord, chr, toLower)
import           Data.List (map, sortOn, foldl')
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isJust, fromJust)
import           Data.Maybe.HT (toMaybe)
import           Helm
import qualified Helm.Color as HelmColor
import           Helm.Engine (Engine)
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Graphics2D as HGfx
import           Helm.Graphics2D.Text
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

data PlayerState =
    Playing |
    HotSeatWait |
    HotSeatBlank |
    PromotionPrompt BoardPosition BoardPosition
  deriving (Show, Eq)

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
        bord `pieceAt` toCoord testPos >>= \(Piece clr _) ->
        guard (clr == playerTurn) >>
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

toCoordMovePromote :: BoardPosition -> BoardPosition -> PieceType -> String
toCoordMovePromote fromPos toPos pieceType =
  let
    showPieceType Queen = "Q"
    showPieceType Knight = "N"
    showPieceType Rook = "R"
    showPieceType Bishop = "B"
  in
    toCoordMove fromPos toPos ++ "=" ++ showPieceType pieceType

findPawnTries :: GameState -> Color -> [BoardPosition]
findPawnTries gameState thisPlayer =
    [ pawnTry |
        (coords@(coordRank, coordFile), square)
            <- A.assocs $ board gameState,
        square /= Empty,
        let (Piece clr pieceType) = fromMaybe undefined (squareToMaybe square),
        pieceType == Pawn,
        clr == thisPlayer,
        let direction = if thisPlayer == White then -1 else 1,
        pawnTryCoords <- [ (coordRank + direction, coordFile - 1)
                        , (coordRank + direction, coordFile + 1)
                        ],
        let pawnTry = coordsToBoardPosition pawnTryCoords,
        let fromPos = coordsToBoardPosition coords,
        let moveSpec = toCoordMove fromPos pawnTry,
        isLegalMove gameState moveSpec
    ]

sideBarTexts :: HelmColor.Color -- ^ the text color
             -> MoveAttempt
             -> Maybe Check
             -> PlayerState
             -> [Text]
sideBarTexts helmColor moveAttempt maybeCheck playerState =
  checkText helmColor maybeCheck ++
  moveAttemptText helmColor moveAttempt ++
  promptPromoteText helmColor playerState

promptPromoteText :: HelmColor.Color -> PlayerState -> [Text]
promptPromoteText helmColor playerState =
  let
    showPlayerState (PromotionPrompt _ _) =
      [ "Promote Pawn:"
      , "Press Q for Queen"
      , "B for Bishop"
      , "R for Rook"
      , "N for Knight"
      ]
    showPlayerState _ = []
  in
    height 20 . color helmColor . toText <$> showPlayerState playerState

checkText :: HelmColor.Color
          -> Maybe Check
          -> [Text]
checkText helmColor maybeCheck =
  let
    showCheck (Just LongDiagonal) = ["Check on long diagonal"]
    showCheck (Just ShortDiagonal) = ["Check on short diagonal"]
    showCheck (Just KnightCheck) = ["Check from a Knight"]
    showCheck (Just ckType) = [show ckType ++ " check"]
    showCheck Nothing = []
  in
    height 30 . color helmColor . toText <$> showCheck (checkType <$> maybeCheck)

moveAttemptText :: HelmColor.Color
                -> MoveAttempt
                -> [Text]
moveAttemptText helmColor moveAttempt =
  let
    showLastMoveAttempt Successful = []
    showLastMoveAttempt Illegal = ["No"]
    showLastMoveAttempt Impossible = ["Hell, No!"]
  in
    height 30 . color helmColor . toText <$> showLastMoveAttempt moveAttempt

toMoveText :: HelmColor.Color
           -> GameState
           -> Maybe GameOver
           -> Text
toMoveText
    helmColor
    gameState
    maybeGameOver =
  let
    currPlayer = currentPlayer gameState
    showToMove = case maybeGameOver of
        Nothing -> " To Move: " ++ show currPlayer
        Just gameOver -> show gameOver -- TODO improve this
  in
    height 30 $ color helmColor $ toText showToMove

textOverlay :: HelmColor.Color
            -> BoardView
            -> GameState
            -> MoveAttempt
            -> Maybe Check
            -> Maybe GameOver
            -> PlayerState
            -> HGfx.Form SDLEngine
textOverlay
    helmColor
    BoardView{bbox=BSquare{width, topLeft = (V2 left top)}}
    gameState
    moveAttempt
    maybeCheck
    maybeGameOver
    playerState =
  let
    topX = width / 2 + left
    topY = top / 2
    sidebarX = width + left + 150
    sidebarY = top + 15
    topForm = HGfx.move (V2 topX topY) $ HGfx.text $ toMoveText helmColor gameState maybeGameOver

    sbarTexts = sideBarTexts helmColor moveAttempt maybeCheck playerState

    calcOffsets :: [V2 Double] -> Text -> [V2 Double]
    calcOffsets offs@(V2 x y : _) txt = V2 x (y + textHeight txt) : offs
    calcOffsets [] _ = []

    offsets :: [V2 Double]
    offsets = reverse $ foldl' calcOffsets [pure 0] sbarTexts

    toForm :: V2 Double -> Text -> HGfx.Form SDLEngine
    toForm offset txt = HGfx.move offset $ HGfx.text txt

    sidebarOffsetForm :: V2 Double -> Text -> HGfx.Form SDLEngine
    sidebarOffsetForm (V2 x y) = toForm (V2 (sidebarX + x) (sidebarY + y))
  in
    case sbarTexts of
      [] -> topForm
      [firstTxt] -> HGfx.group
          [ topForm
          , toForm (V2 sidebarX sidebarY) firstTxt
          ]
      firstTxt : txts -> HGfx.group
          ( topForm
          : toForm (V2 sidebarX sidebarY) firstTxt
          : (uncurry sidebarOffsetForm <$>  zip (tail offsets) txts)
          )

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

piecesForm :: Engine e => PlayerState -> GameState -> Options -> BoardView ->
    M.Map String (Image e) -> V2 Int -> HGfx.Form e
piecesForm HotSeatBlank _ _ _ _ _ = HGfx.blank
piecesForm playerState gameState Options{gameVariant} BoardView{bbox, orient, posInMotion} assets mousePos =
  let
    showColor clr = toLower (head $ show clr)
    showPieceType pieceType = toLower <$> show pieceType
    pieceName (Piece clr pieceType) = showColor clr : "_" ++ showPieceType pieceType
    chooseImage piece = assets M.! pieceName piece
    ssize = squareSize bbox
    imageDims = pure ssize
    mkForm piece = HGfx.image imageDims $ chooseImage piece

    -- sort pieces so moving or promoting piece is on top
    sortF :: PlayerState -> BoardPosition -> Bool
    sortF (PromotionPrompt _ toPos) thisPos = (do
      p <- posInMotion
      return $ p == thisPos || toPos == thisPos) == Just True
    sortF _ thisPos = Just thisPos == posInMotion

    pieceImage boardPosition piece
        | (PromotionPrompt fromPos toPos) <- playerState, boardPosition == fromPos =
            HGfx.move (toOffset toPos orient ssize) $ mkForm piece
        | posInMotion == Just boardPosition =
            HGfx.move (toBoardLocal (fromIntegral <$> mousePos) bbox - imageDims / 2) $ mkForm piece
        | otherwise =
            HGfx.move (toOffset boardPosition orient ssize) $ mkForm piece

    pieces = [(coordsToBoardPosition coords, piece) |
                (coords, square) <- assocs $ board gameState,
                let maybePiece = squareToMaybe square,
                isJust maybePiece,
                let piece@(Piece clr _) = fromJust maybePiece,
                case (isGameOver gameState, playerState, gameVariant, clr) of
                    (True, _ , _, _) -> True
                    (_, _, Chess, _) -> True
                    (_, HotSeatWait, Kriegspiel, White) -> currentPlayer gameState == Black
                    (_, HotSeatWait, Kriegspiel, Black) -> currentPlayer gameState == White
                    (_, Playing, Kriegspiel, _) -> currentPlayer gameState == clr
                    _ -> True]
    sortedPieces = sortOn (sortF playerState . fst) pieces
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
