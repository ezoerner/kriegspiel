{-# LANGUAGE NamedFieldPuns #-}

module BoardView where

import           Chess
import           Control.Applicative (pure)
import           Control.Monad (guard)
import           Data.Array as A
import           Data.Char (ord, chr, toLower)
import           Data.List (map, sortOn, foldl')
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust, fromJust)
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
        bord `pieceAt` toStringCoord testPos >>= \(Piece clr _) ->
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

sideBarTexts :: HelmColor.Color -- ^ the text color
             -> MoveAttempt
             -> GameState
             -> PlayerState
             -> [Text]
sideBarTexts helmColor moveAttempt gameState playerState =
  hotSeatTexts helmColor playerState (currentPlayer gameState) ++
  checkText helmColor gameState ++
  moveAttemptText helmColor moveAttempt ++
  promptPromoteText helmColor playerState

hotSeatTexts :: HelmColor.Color -> PlayerState -> Color -> [Text]
hotSeatTexts helmColor playerState currPlayer =
  let
    prompt HotSeatWait =
      [ "Type <Space> to blank screen", "for " ++ show currPlayer]
    prompt HotSeatBlank =
      [ "Type <Space> to start turn", "for " ++ show currPlayer]
    prompt _ = []
  in
    height 20 . color helmColor . toText <$> prompt playerState

promptPromoteText :: HelmColor.Color -> PlayerState -> [Text]
promptPromoteText helmColor playerState =
  let
    showPlayerState (PromotionPrompt _ _) =
      [ "Promote Pawn:"
      , "Type Q for Queen"
      , "B for Bishop"
      , "R for Rook"
      , "N for Knight"
      ]
    showPlayerState _ = []
  in
    height 20 . color helmColor . toText <$> showPlayerState playerState

checkText :: HelmColor.Color
          -> GameState
          -> [Text]
checkText helmColor gameState =
  let
    showCheck LongDiagonal = "Check on long diagonal"
    showCheck ShortDiagonal = "Check on short diagonal"
    showCheck KnightCheck = "Check from a Knight"
    showCheck ckType = show ckType ++ " check"
  in
    height 30 . color helmColor . toText . showCheck <$> findChecks gameState

moveAttemptText :: HelmColor.Color
                -> MoveAttempt
                -> [Text]
moveAttemptText helmColor moveAttempt =
  let
    showLastMoveAttempt Successful = []
    showLastMoveAttempt (Illegal (Piece _ pieceType) from (Just to)) =
        [ "Sorry, you are not"
        , "allowed to move " ++ show pieceType
        , "from " ++ toStringCoord from ++ " to " ++ toStringCoord to
        ]
    showLastMoveAttempt (Illegal (Piece _ pieceType) from Nothing) =
        [ "Not Allowed to move"
        , show pieceType
        , "  from " ++ toStringCoord from ++ " to off the board"
        ]
  in
    height 30 . color helmColor . toText <$> showLastMoveAttempt moveAttempt

toMoveText :: HelmColor.Color
           -> GameState
           -> Text
toMoveText helmColor gameState =
  let
    currPlayer = currentPlayer gameState
    showToMove = case maybeGameOver gameState of
        Nothing -> " To Move: " ++ show currPlayer
        Just gameOver -> show gameOver
  in
    height 30 $ color helmColor $ toText showToMove

textOverlay :: HelmColor.Color
            -> BoardView
            -> GameState
            -> MoveAttempt
            -> PlayerState
            -> HGfx.Form SDLEngine
textOverlay
    helmColor
    BoardView{bbox=BSquare{width, topLeft = (V2 left top)}}
    gameState
    moveAttempt
    playerState =
  let
    topX = width / 2 + left
    topY = top / 2
    sidebarX = width + left + 175
    sidebarY = top + 15
    topForm = HGfx.move (V2 topX topY) $ HGfx.text $ toMoveText helmColor gameState

    sbarTexts = sideBarTexts helmColor moveAttempt gameState playerState

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

boardForm :: Engine e => Image e
          -> Image e
          -> BoardView
          -> [BoardPosition]
          -> HGfx.Form e
boardForm lightSquare darkSquare BoardView{bbox, orient} pawnTries =
  let
    ssize = squareSize bbox
    imageDims = V2 ssize ssize
    pivot White = 0
    pivot Black = 1
    toPos x y = if orient == White
                then (chr (floor x + ord 'a'), 8 - floor y)
                else (chr (floor x + ord 'a'), floor y + 1)
    pawnTryForm = HGfx.move (V2 0 ssize/2) $ -- TODO attempt to vertically center text form inside group doesn't work, bug in Helm?
                    HGfx.text $
                    height 12 $
                    color (HelmColor.rgb 1 1 1) $
                    bold $
                    toText "pawn try"
    chooseImage x y = if floor (x + y) `mod` (2 :: Integer) == pivot orient
                      then lightSquare
                      else darkSquare
    mkForm x y =
      let
        baseForm = HGfx.image imageDims $ chooseImage x y
        isPawnTry = toPos x y `elem` pawnTries
      in
        if isPawnTry
        then HGfx.group [baseForm, pawnTryForm]
        else baseForm
  in
    HGfx.toForm $ HGfx.collage [HGfx.move (V2 hOff vOff) $ mkForm x y
                                | x <- [0..7] -- ^ x
                                , y <- [0..7]
                                , let hOff = x * ssize
                                , let vOff = y * ssize
                                ]

piecesForm :: Engine e => PlayerState -> GameState -> Options -> BoardView ->
    M.Map String (Image e) -> V2 Int -> HGfx.Form e
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

    pieces = [ (coordsToBoardPosition coords, piece) |
               (coords, square) <- assocs $ board gameState
             , let maybePiece = squareToMaybe square
             , isJust maybePiece
             , let piece@(Piece clr _) = fromJust maybePiece
             , case (isGameOver gameState, playerState, gameVariant, clr) of
                 (True, _ , _, _) -> True
                 (_, HotSeatBlank, _, _) -> False
                 (_, _, Chess, _) -> True
                 (_, HotSeatWait, Kriegspiel, White) -> currentPlayer gameState == Black
                 (_, HotSeatWait, Kriegspiel, Black) -> currentPlayer gameState == White
                 (_, _, Kriegspiel, _) -> currentPlayer gameState == clr
             ]
    sortedPieces = sortOn (sortF playerState . fst) pieces
    imageCollage = HGfx.collage $ map (uncurry pieceImage) sortedPieces
  in
    HGfx.toForm imageCollage

-- Private functions
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

toOffset :: BoardPosition
         -> Color -- ^ orientation of the board
         -> Double
         -> V2 Double
toOffset (file, rank) White ssize =
    (fromIntegral <$> V2 (ord file - ord 'a') (8 - rank)) * pure ssize
toOffset (file, rank) Black ssize =
    (fromIntegral <$> V2 (ord file - ord 'a') (rank - 1)) * pure ssize
