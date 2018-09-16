# Compile 

install haskell-platform
cabal update && cabal install lens mtl irc irc-client random-shuffle 
ghc -O3 --make Coinche.Simul -main-is Coinche.Simul

# Run 1000 games (random players vs AI)
./Coinche/Simul  1000
