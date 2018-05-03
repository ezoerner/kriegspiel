module Options where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

data GameVariant = Chess | Kriegspiel
  deriving (Eq, Show)

data Options = Options
    { hotSeat :: Bool
    , gameVariant :: GameVariant
    }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
    Options
        <$> switch
                (long "hotseat" <> help
                    "play in hot-seat mode (2 players sharing same screen)"
                )
        <*> variant

opts :: ParserInfo Options
opts = info
    (optionsParser <**> helper)
    (  fullDesc
    <> progDesc
           "Play Kriegspiel or Chess. https://en.wikipedia.org/wiki/Kriegspiel_(chess)"
    <> header "kriegspiel - play Kriegspiel or Chess."
    )

chessVariant :: Parser GameVariant
chessVariant =
    flag' Chess (long "chess" <> short 'c' <> help "Standard Chess (default)")

kriegspielVariant :: Parser GameVariant
kriegspielVariant =
    flag' Kriegspiel (long "kriegspiel" <> short 'k' <> help "Kriegspiel")

variant :: Parser GameVariant
variant = chessVariant <|> kriegspielVariant <|> pure Chess
