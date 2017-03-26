{-# LANGUAGE GADTs #-}
module Stimulus where

import Decision (Decision(..))

data Stimulus = ForcedDecision Decision
