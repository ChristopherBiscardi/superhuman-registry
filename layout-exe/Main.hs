module Main where

import SR.Routes (api)
import Servant (layout)
import Data.Text.IO as TIO (putStr)

main :: IO ()
main = TIO.putStr $ layout api
