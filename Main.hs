{-# LANGUAGE TemplateHaskell #-}
module Main where
-- | Kiwi is an AI band mate and friend
-- | Kiwi's main IO loop has the following form:
-- | Loop:
-- | - Load mental model
-- | - Take in stimulus
-- | - Make a decision
-- | - Perform an action
-- | - Save mental model

import qualified Decision as D
import Decision (Decision(..), Action(..))

import qualified Stimulus as S
import Stimulus (Stimulus(..))

import qualified Model as M
import Model (Model (..))

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
  setup modelVar
  forkIO $ loop input output modelVar
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

-- initialize all endpoints, make sure everything is reachable
initLoop :: TVar (Maybe Stimulus) -> TVar (Maybe Action) -> TVar Model -> IO ()
initLoop input output modelVar = do
  setup modelVar
  loop input output modelVar

setup :: TVar (Model) -> IO ()
setup _ = return ()

-- main cognitive loop
loop :: TVar (Maybe Stimulus) -> TVar (Maybe Action) -> TVar Model -> IO ()
loop input output  modelVar = do
  model    <- load modelVar
  stimulus <- takeIn input
  decision <- make    stimulus model
  model'   <- perform decision model output
  result   <- save modelVar model'
  case result of
    Left () -> return ()
    Right e -> putError e
  yield
  loop input output modelVar


-- error msg wrapper, can be replaced
type ErrorMsg = String
putError :: ErrorMsg -> IO ()
putError = putStrLn
type Success  = ()

-- cognitive functions
load :: TVar (Model) -> IO Model
load = atomically . readTVar

takeIn :: TVar (Maybe Stimulus) -> IO (Maybe Stimulus)
takeIn v = do
  ms <- atomically $ readTVar v
  atomically $ writeTVar v Nothing
  return ms


make :: Maybe Stimulus -> Model  -> IO (Maybe Decision)
make Nothing _ = return Nothing
make (Just (ForcedDecision d)) _ = return $ Just d

perform :: Maybe Decision -> Model -> TVar (Maybe Action) -> IO Model
perform Nothing m _ = return m
perform (Just d) m output = do
  (a,m') <- D.doo d m
  atomically $ writeTVar output $ Just a
  return m'

save :: TVar Model -> Model -> IO (Either Success ErrorMsg)
save modelVar model  = do
  atomically $ writeTVar modelVar model
  return $ Left ()
