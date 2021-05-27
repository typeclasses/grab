import System.Environment
import System.Process

main =
  getEnv "ghc" >>= \ghc ->
    case ghc of
      "8.6.5"  -> callProcess "cabal" ["test", "all"
                  , "--constraint=containers == 0.6.0.1"
                  , "--constraint=criterion == 1.5.0.0"
                  , "--constraint=hedgehog == 1.0"
                  , "--constraint=text == 1.2.3.0"
                  ]
      "8.8.1"  -> callProcess "cabal" ["test", "all"]
      "8.10.4" -> callProcess "cabal" ["test", "all"]
      "9.0.1"  -> callProcess "cabal" ["test", "all"
                  , "--constraint=containers == 0.6.4.1"
                  , "--constraint=criterion == 1.5.9.0"
                  , "--constraint=hedgehog == 1.0.5"
                  , "--constraint=text == 1.2.4.1"
                  ]
