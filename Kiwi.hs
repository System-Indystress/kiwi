{-# LANGUAGE TemplateHaskell #-}
module Kiwi where
-- | Kiwi is an AI band mate and friend
-- | Kiwi's main IO loop has the following form:
-- | Loop:
-- | - Load mental model
-- | - Take in stimulus
-- | - Make a decision
-- | - Perform an action
-- | - Save mental model

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
