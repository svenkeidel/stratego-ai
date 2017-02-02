#!/bin/sh

pack-sdf -i stratego-haskell.sdf -o stratego-haskell.def -Idef haskell-mix.def -Idef stratego-mix.def -Idef haskell.def --Include .
sdf2table -i stratego-haskell.def -m stratego-haskell -o stratego-haskell.tbl

./sdf2rtg -Xnativepath $PWD/ -m Haskell -i haskell.def -o haskell.rtg
./rtg2sig -i haskell.rtg -o haskell.str
rm haskell.rtg
