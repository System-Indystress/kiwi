{-#LANGUAGE ExistentialQuantification #-}
module Model where

import qualified Language.Normandy as N
import Language.Normandy (Story (..))
import Language.Thesaurus
import Language.Thesaurus.RogetLite
import qualified Data.Text as T
import Data.Text (Text (..))



data Model = forall t. (Thesaurus t) =>
  Model
  {
    currSong :: Story Text
  , currThes :: t
  , currMood :: Text
  }


empty = Model {
    currSong = N.hole
  , currThes = buildTh rogetLite
  , currMood = T.pack "😃"
}
