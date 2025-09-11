module Main where

import Interface
import System.IO(stdout, hSetBuffering, BufferMode(NoBuffering))

main = do 
    hSetBuffering stdout NoBuffering
    laçoMenuPrincipal
