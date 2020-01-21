# kriegspiel

Kriegspiel is a chess variant where each player can see their own pieces, but not those of their opponent.

See https://en.wikipedia.org/wiki/Kriegspiel_(chess)

# Build

Install the `stack` build tool (see http://haskellstack.org) and run `stack install` in checkout directory

### Command Line Options

```
kriegspiel - play Kriegspiel or Chess.

Usage: kriegspiel [--hotseat] ([-c|--chess] | [-k|--kriegspiel])
  Play Kriegspiel or Chess. https://en.wikipedia.org/wiki/Kriegspiel_(chess)

Available options:
  --hotseat                play in hot-seat mode (2 players sharing same screen)
  -c,--chess               Standard Chess (default)
  -k,--kriegspiel          Kriegspiel
  -h,--help                Show this help text
```
