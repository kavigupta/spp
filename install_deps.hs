#!/usr/bin/runhaskell

import System.Process(system)

main = do
    system "autoreconf -i"
    system "runhaskell Setup configure"
    system "runhaskell Setup build"
    system "runhaskell Setup install"
