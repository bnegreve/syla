# Compile 

install haskell-platform
cabal update && cabal install lens mtl irc irc-client random-shuffle optparse-applicative snap snap-templates aeson

# To test AIs, run compile and run Coinche.Main (cli interface)

ghc -O3 --make Coinche.Main -main-is Coinche.Main
./Coinche/Main --help


# To run the webserver, compile and run Coinche.WebServer.Http
ghc -O3 --make Coinche.WebServer.Http -main-is Coinche.WebServer.Http
./Coinche/WebServer/Http