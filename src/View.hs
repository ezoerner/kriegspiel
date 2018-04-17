{-# LANGUAGE NamedFieldPuns #-}

module View where

import           Data.Maybe (fromMaybe, isJust)
import           Data.Char (toLower, ord, chr)
import qualified Data.Map.Strict as M
import           Helm
import           Helm.Engine (Engine)
import           Helm.Engine.SDL (SDLEngine)
import           Linear.V2 (V2(V2))
import           Data.List (map, sortOn)
import           Helm.Color
import qualified Helm.Graphics2D.Text as Text
import           Helm.Graphics2D

import           Model
import           Board
import           Options

overlay :: Color -> BoundingSquare -> GameState -> Form SDLEngine
overlay color BSquare{width, topLeft = (V2 left top)}
    GameState{next, prev, check} =
  let
    topX = width / 2 + left
    topY = top / 2
    sidebarX = width + left + 100
    sidebarY = top + 15
    textHeight = 30
    showPrev Successful = ""
    showPrev Illegal = "No"
    showPrev Impossible = "Hell, No!"
    showNext (Right player) = " To Move: " ++ show player
    showNext (Left gameOver) = show gameOver -- TO DO improve this
    showCheck (Just LongDiagonal) = "Check on long diagonal"
    showCheck (Just ShortDiagonal) = "Check on short diagonal"
    showCheck (Just KnightCheck) = "Check from a Knight"
    showCheck (Just ckType) = show ckType ++ " check"
    showCheck Nothing = ""
  in
    group [ move (V2 topX topY) $ text $ Text.height textHeight $
                Text.color color $
                Text.toText $ showNext next
          , move (V2 sidebarX sidebarY) $ text $ Text.height textHeight $
                Text.color color $ Text.toText $ showCheck (checkType <$> check)
          , move (V2 sidebarX $ sidebarY + textHeight) $ text $ Text.height textHeight $
                Text.color color $
                Text.toText $ showPrev prev
          ]

boardForm :: Engine e => Image e -> Image e -> Board -> Form e
boardForm lightSquare darkSquare Board{bbox, orient} =
  let
    ssize = squareSize bbox
    imageDims = V2 ssize ssize
    pivot White = 0
    pivot Black = 1
    chooseImage x y = if floor (x + y) `mod` (2 :: Integer) == pivot orient
                      then lightSquare
                      else darkSquare
    mkForm x y = image imageDims $ chooseImage x y
  in
    toForm $ collage [move (V2 hOff vOff) $ mkForm x y
                        | x <- [0..7]
                        , y <- [0..7]
                        , let hOff = x * ssize
                        , let vOff = y * ssize
                        ]

piecesForm :: Engine e => Model -> M.Map String (Image e) -> V2 Int -> Form e
piecesForm Model
    { gameState = GameState{next}
    , options = Options{gameVariant}
    , board = Board{positions, bbox, orient}
    } assets mousePos =
  let
    showPlayer player = toLower (head $ show player)
    showPieceType pieceType = toLower <$> show pieceType
    playerName piece = showPlayer $ player piece
    pieceName piece = showPieceType $ pieceType piece
    chooseImage piece = assets M.! (playerName piece : "_" ++ pieceName piece)
    ssize = squareSize bbox
    imageDims = pure ssize
    mkForm piece = image imageDims $ chooseImage piece

    pieceImage _ (Just piece@Piece{inMotion = True}) =
        move (toBoardLocal (fromIntegral <$> mousePos) bbox - imageDims / 2) $ mkForm piece
    pieceImage boardPosition (Just piece)  =
      move (toOffset boardPosition orient ssize) $ mkForm piece

    pieces = [((file, rank), maybePiece) |
      file <- ['a'..'h'],
      rank <- [1..8],
      let maybePiece = positions M.!? (file, rank),
      isJust maybePiece,
      case (gameVariant, maybePiece, next) of
        (Chess, _, _) -> True
        (Kriegspiel, Just piece, Right color) -> color == player piece
        (_, _, Left _) -> True]
    sortedPieces = sortOn (fmap inMotion . snd) pieces
    imageCollage = collage $ map (uncurry pieceImage) sortedPieces
  in
    toForm imageCollage
