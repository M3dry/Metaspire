module Main (main) where

import P2P (client)
import System.Environment (getArgs)

main :: IO ()
main = do
    [ip, port] <- take 2 <$> getArgs
    client ip port
