{-# LANGUAGE GADTs #-}
module Kiwi.Stimulus where

import Kiwi.Decision (Decision(..))

data Stimulus = ForcedDecision Decision
