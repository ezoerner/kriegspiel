{-# LANGUAGE NamedFieldPuns #-}

module View where

import           Chess
import           Control.Applicative  (pure)
import           Control.Monad        (guard)
import           Data.Array           as A
import           Data.Char            (chr, ord, toLower)
import           Data.List            (foldl', map, sortOn)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromJust, isJust)
import           Data.Maybe.HT        (toMaybe)
import           Helm
import qualified Helm.Color           as HelmColor
import           Helm.Engine          (Engine)
import           Helm.Engine.SDL      (SDLEngine)
import qualified Helm.Graphics2D      as HGfx
import           Helm.Graphics2D.Text
import           Linear.V2            (V2 (V2))

import           Model
import           Options

-- for this game a bounding box is always square
data BoundingSquare = BSquare
    { width   :: !Double
    , topLeft :: !(V2 Double)
    } deriving (Show)

data View = View
    { windowDims     :: !(V2 Int)
    , bbox           :: !BoundingSquare
    , orient         :: !Color
    , coordsInMotion :: !(Maybe Coordinates)
    , playerState    :: !PlayerState
    }
    deriving (Show)

initialWindowDims :: V2 Int
initialWindowDims = V2 1000 640

data PlayerState =
    Playing |
    HotSeatWait |
    HotSeatBlank |
    PromotionPrompt Coordinates Coordinates
  deriving (Show, Eq)

initialView :: View
initialView = View
    { windowDims     = initialWindowDims
    , bbox           = calcBoardBBox initialWindowDims
    , orient         = White
    , coordsInMotion = Nothing
    , playerState    = Playing
    }

border :: Num a => V2 a
border = V2 100 100


calcBoardBBox :: (Integral a, Ord a) => V2 a -> BoundingSquare
calcBoardBBox windowSize =
    let constrainSquare (V2 x y) = x `min` y
        calcBoardSize = constrainSquare . subtract (2 * border)
    in  BSquare
            { width   = fromIntegral $ calcBoardSize windowSize
            , topLeft = border
            }

resize :: View -> V2 Int -> View
resize view windowDims =
    let bbox = calcBoardBBox windowDims
    in  view {windowDims, bbox}

rotateBoard :: View -> View
rotateBoard view@View{orient=Black} = view{orient=White}
rotateBoard view@View{orient=White} = view{orient=Black}

endTurnV :: View
         -> Color     -- ^ current player
         -> Options
         -> View
endTurnV view@View{orient} currPlayer Options{hotSeat,gameVariant} =
    view{orient=if hotSeat
                then currPlayer
                else orient
         , playerState=if hotSeat && gameVariant == Kriegspiel
                       then HotSeatWait
                       else Playing
         }

findPositionWithPiece :: Board -> View -> V2 Double -> Color -> Maybe Coordinates
findPositionWithPiece bord View { bbox, orient } point playerTurn =
    let maybeCoords = pointToCoords bbox point orient
    in  maybeCoords >>= \testCoords ->
            bord `pieceAt` printCoordinate testCoords >>= \(Piece clr _) ->
                guard (clr == playerTurn) >> return testCoords

pointToCoords :: BoundingSquare -> V2 Double -> Color -> Maybe Coordinates
pointToCoords bbox (V2 x y) playerOrient =
    let ssize = squareSize bbox
        tryCoords White = (floor (y / ssize), floor (x / ssize))
        tryCoords Black = (7 - floor (y / ssize), floor (x / ssize))
        thisTryCoords = tryCoords playerOrient
    in  isInsideBoard thisTryCoords `toMaybe` thisTryCoords

toBoardLocal :: V2 Double -> BoundingSquare -> V2 Double
toBoardLocal globalV2 bbox = globalV2 - topLeft bbox

sideBarTexts:: HelmColor.Color -- ^ the text color
    -> MoveAttempt
    -> GameState
    -> PlayerState
    -> [Text]
sideBarTexts helmColor moveAttempt gameState playerState =
    hotSeatTexts helmColor playerState (currentPlayer gameState)
        ++ checkText helmColor gameState
        ++ moveAttemptText helmColor moveAttempt
        ++ promptPromoteText helmColor playerState

hotSeatTexts :: HelmColor.Color -> PlayerState -> Color -> [Text]
hotSeatTexts helmColor playerState currPlayer =
    let prompt HotSeatWait =
            ["Type <Space> to blank screen", "for " ++ show currPlayer]
        prompt HotSeatBlank =
            ["Type <Space> to start turn", "for " ++ show currPlayer]
        prompt _ = []
    in  height 20 . color helmColor . toText <$> prompt playerState

promptPromoteText :: HelmColor.Color -> PlayerState -> [Text]
promptPromoteText helmColor playerState =
    let showPlayerState (PromotionPrompt _ _) =
            [ "Promote Pawn:"
            , "Type Q for Queen"
            , "B for Bishop"
            , "R for Rook"
            , "N for Knight"
            ]
        showPlayerState _ = []
    in  height 20 . color helmColor . toText <$> showPlayerState playerState

checkText :: HelmColor.Color -> GameState -> [Text]
checkText helmColor gameState =
    let showCheck LongDiagonal  = "Check on long diagonal"
        showCheck ShortDiagonal = "Check on short diagonal"
        showCheck KnightCheck   = "Check from a Knight"
        showCheck ckType        = show ckType ++ " check"
    in  height 30
        .   color helmColor
        .   toText
        .   showCheck
        <$> findChecks gameState

moveAttemptText :: HelmColor.Color -> MoveAttempt -> [Text]
moveAttemptText helmColor moveAttempt =
    let showLastMoveAttempt Successful = []
        showLastMoveAttempt (Illegal (Piece _ pieceType) from (Just to)) =
            [ "Move not legal: "
            , show pieceType ++ " " ++ printMove from to Nothing
            ]
        showLastMoveAttempt (Illegal (Piece _ pieceType) from Nothing) =
            [ "Not Allowed to move"
            , show pieceType
            , "  from " ++ printCoordinate from ++ " to off the board"
            ]
    in  height 30 . color helmColor . toText <$> showLastMoveAttempt moveAttempt

toMoveText :: HelmColor.Color -> GameState -> Text
toMoveText helmColor gameState =
    let currPlayer = currentPlayer gameState
        showToMove = case maybeGameOver gameState of
            Nothing       -> " To Move: " ++ show currPlayer
            Just gameOver -> show gameOver
    in  height 30 $ color helmColor $ toText showToMove

textOverlay
    :: HelmColor.Color
    -> View
    -> GameState
    -> MoveAttempt
    -> PlayerState
    -> HGfx.Form SDLEngine
textOverlay helmColor View { bbox = BSquare { width, topLeft = (V2 left top) } } gameState moveAttempt playerState
    = let
          topX     = width / 2 + left
          topY     = top / 2
          sidebarX = width + left + 175
          sidebarY = top + 15
          topForm  = HGfx.move (V2 topX topY) $ HGfx.text $ toMoveText
              helmColor
              gameState

          sbarTexts = sideBarTexts helmColor moveAttempt gameState playerState

          calcOffsets :: [V2 Double] -> Text -> [V2 Double]
          calcOffsets offs@(V2 x y : _) txt = V2 x (y + textHeight txt) : offs
          calcOffsets []                _   = []

          offsets :: [V2 Double]
          offsets = reverse $ foldl' calcOffsets [pure 0] sbarTexts

          toForm :: V2 Double -> Text -> HGfx.Form SDLEngine
          toForm offset txt = HGfx.move offset $ HGfx.text txt

          sidebarOffsetForm :: V2 Double -> Text -> HGfx.Form SDLEngine
          sidebarOffsetForm (V2 x y) =
              toForm (V2 (sidebarX + x) (sidebarY + y))
      in
          case sbarTexts of
              [] -> topForm
              [firstTxt] ->
                  HGfx.group [topForm, toForm (V2 sidebarX sidebarY) firstTxt]
              firstTxt : txts -> HGfx.group
                  ( topForm
                  : toForm (V2 sidebarX sidebarY) firstTxt
                  : (uncurry sidebarOffsetForm <$> zip (tail offsets) txts)
                  )

boardForm
    :: Engine e
    => Image e
    -> Image e
    -> View
    -> [Coordinates]
    -> HGfx.Form e
boardForm lightSquare darkSquare View { bbox = bbox@BSquare { width }, orient } pawnTries
    = let ssize     = squareSize bbox
          imageDims = V2 ssize ssize
          pawnTryForm =
            -- TODO attempt to center text form inside group
            -- doesn't work, bug in Helm?
              HGfx.move (V2 (ssize/2) (ssize/2))
                  $ HGfx.text
                  $ height 12
                  $ color (HelmColor.rgb 1 1 1)
                  $ bold
                  $ toText "pawn try"
          sqTypes = squareTypes pawnTries
          sqForm Light = HGfx.image imageDims lightSquare
          sqForm Dark  = HGfx.image imageDims darkSquare
          buildForm (sqColor, False) = sqForm sqColor
          buildForm (sqColor, True ) = HGfx.group [sqForm sqColor, pawnTryForm]
          mkSquareForm coords = buildForm (sqTypes M.! coords)
          mkRankLabel y =
              HGfx.text
                  $ height 15
                  $ color (HelmColor.rgb (33 / 255) (118 / 255) $ 199 / 255)
                  $ toText
                  $ show
                  $ if orient == White then 8 - y else y + 1
          mkFileLabel x =
              HGfx.text
                  $ height 15
                  $ color (HelmColor.rgb (33 / 255) (118 / 255) $ 199 / 255)
                  $ toText [chr $ ord 'a' + x]
      in  HGfx.toForm
          $  HGfx.collage
          $  [ HGfx.move (V2 hOff vOff) $ mkRankLabel (floor y :: Integer)
             | y <- [0 .. 7]
             , let vOff = y * ssize + (ssize / 2)
             , let hOff = -10
             ]
          ++ [ HGfx.move (V2 hOff vOff) $ mkFileLabel $ floor x
             | x <- [0 .. 7]
             , let hOff = x * ssize + (ssize / 2)
             , let vOff = width + 20
             ]
          ++ [ HGfx.move offset $ mkSquareForm coords
             | file <- [0 .. 7]
             , rank <- [0 .. 7]
             , let coords = (rank, file)
             , let offset = toOffset coords orient ssize
             ]

data SquareColor = Light | Dark
        deriving (Show, Eq)
type SquareType = (SquareColor, Bool) -- ^ bool is hasPawnTry

squareTypes
    :: [Coordinates]                -- ^ pawnTries
    -> M.Map Coordinates SquareType -- ^ square types
squareTypes pawnTries = M.fromList
    [ (coord, squareType)
    | rank <- [0 .. 7]
    , file <- [0 .. 7]
    , let coord       = (rank, file)
    , let hasPawnTry  = coord `elem` pawnTries
    , let squareColor = if (rank + file) `mod` 2 == 0 then Light else Dark
    , let squareType = (squareColor, hasPawnTry)
    ]

piecesForm
    :: Engine e
    => PlayerState
    -> GameState
    -> Options
    -> View
    -> M.Map String (Image e)
    -> V2 Int
    -> HGfx.Form e
piecesForm playerState gameState Options { gameVariant } View { bbox, orient, coordsInMotion } assets mousePos
    = let
          showColor clr = toLower (head $ show clr)
          showPieceType pieceType = toLower <$> show pieceType
          pieceName (Piece clr pieceType) =
              showColor clr : "_" ++ showPieceType pieceType
          chooseImage piece = assets M.! pieceName piece
          ssize     = squareSize bbox
          imageDims = pure ssize
          mkForm piece = HGfx.image imageDims $ chooseImage piece

          -- sort pieces so moving or promoting piece is on top
          sortF :: PlayerState -> Coordinates -> Bool
          sortF (PromotionPrompt _ toCoords) thisCoords =
              (do
                      p <- coordsInMotion
                      return $ p == thisCoords || toCoords == thisCoords
                  )
                  == Just True
          sortF _ thisCoords = Just thisCoords == coordsInMotion

          pieceImage coords piece
              | (PromotionPrompt fromCoords toCoords) <- playerState
              , coords == fromCoords
              = HGfx.move (toOffset toCoords orient ssize) $ mkForm piece
              | coordsInMotion == Just coords
              = HGfx.move
                      ( toBoardLocal (fromIntegral <$> mousePos) bbox
                      - imageDims
                      / 2
                      )
                  $ mkForm piece
              | otherwise
              = HGfx.move (toOffset coords orient ssize) $ mkForm piece

          pieces
              = [ (coords, piece)
                | (coords, square) <- assocs $ board gameState
                , let maybePiece = squareToMaybe square
                , isJust maybePiece
                , let piece@(Piece clr _) = fromJust maybePiece
                , case (isGameOver gameState, playerState, gameVariant, clr) of
                    (True, _           , _    , _) -> True
                    (_   , HotSeatBlank, _    , _) -> False
                    (_   , _           , Chess, _) -> True
                    (_, HotSeatWait, Kriegspiel, White) ->
                        currentPlayer gameState == Black
                    (_, HotSeatWait, Kriegspiel, Black) ->
                        currentPlayer gameState == White
                    (_, _, Kriegspiel, _) -> currentPlayer gameState == clr
                ]
          sortedPieces = sortOn (sortF playerState . fst) pieces
          imageCollage = HGfx.collage $ map (uncurry pieceImage) sortedPieces
      in
          HGfx.toForm imageCollage

-- Private functions

squareSize :: BoundingSquare -> Double
squareSize bbox = width bbox / 8

toOffset
    :: Coordinates
    -> Color -- ^ orientation of the board
    -> Double -- ^ square size
    -> V2 Double
toOffset (rank, file) White ssize =
    (fromIntegral <$> V2 file rank) * pure ssize
toOffset (rank, file) Black ssize =
    (fromIntegral <$> V2 file (7 - rank)) * pure ssize
