module Main where

import Kiwi

import qualified Kiwi.Decision as D
import Kiwi.Decision (Decision(..), Action(..))

import qualified Kiwi.Stimulus as S
import Kiwi.Stimulus (Stimulus(..))

import qualified Kiwi.Model as M
import Kiwi.Model (Model (..))


-- utilities
import GHC.Conc.Sync
import Language.Normandy
import Language.Normandy.Free (parsedToFree)
import Language.Normandy.Parser (parseNormandy)

import Control.Monad.Free (Free(..))
import Control.Concurrent (threadDelay)
-- debug
import Debug.Trace (trace, traceIO)

-- setup-loop style server
main :: IO ()
main = do
  putStrLn "Kiwi begin!"
  input <- atomically $ newTVar Nothing
  output <- atomically $ newTVar Nothing
  modelVar <- atomically $ newTVar M.empty
  putStrLn "Loading..."
  forkIO $ initLoop input output modelVar
  repl input output

repl :: TVar (Maybe (Stimulus)) -> TVar (Maybe Action) -> IO ()
repl input output =
  let handleOutput :: Maybe Action -> IO Bool
      handleOutput (Just Print{pSong = s}) = do
        putStrLn $ prettyPrint s
        return True
      handleOutput _ = return False
      waitForOutput :: IO ()
      waitForOutput = do
        o <- atomically $ readTVar output
        success <- handleOutput o
        atomically $ writeTVar output Nothing
        if success then return () else do { threadDelay 1000000; waitForOutput }
  in
    do
      r <- getLine
      let s = case parseNormandy "user input" 0 0 r of
                Left _      -> hole
                Right nvals -> foldr ((>>) . parsedToFree) (Pure ()) nvals
      atomically $ writeTVar input $ Just $ ForcedDecision $ Lyric {lSong = s}
      waitForOutput
      repl input output
