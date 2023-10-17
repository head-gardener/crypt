module Interface.Common where

import Data.Cipher

type Handler = String -> IO ()

makeInterface :: IO () -> Handler -> Handler -> IO ()
makeInterface a e o = do
  _ <- a
  print "done"
