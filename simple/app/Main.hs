module Main where

import Lib
import EmailParse

main :: IO ()
main = do
  emailBody <- getContents
  resp <- emailAndResponse emailBody
  putStrLn resp
