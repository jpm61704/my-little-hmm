module Main where

import Pretty
import BaumWelch
import Example

main :: IO ()
main = do
  let x1 = reestimate trial1 exampleHMM
  putStr $ showHMM x1
  let x2 = reestimate trial1 x1
  putStr $ showHMM x2
  return ()
