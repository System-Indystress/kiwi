module Decision where

import Model

import Language.Normandy
import Data.Text

import System.Random

data Decision = Lyric     {lSong :: Story Text}
              | Question  {}
              | Statement {}


data Action = Print {pSong :: Story Text}
            | Sing      {}
            | Respond   {}
            | VLog      {}

doo :: Decision -> Model -> IO (Action, Model)
doo (Lyric {lSong = s}) m@(Model {currThes = t}) = do -- random lyric case
  p <- randomRIO(1,100)
  seed1 <- randomRIO(1,1000)
  seed2 <- randomRIO(1,1000)
  let rFn = (\i -> i + p) -- pseudorandomizer to cover possible words
  let song = (chooseNIdeas 3 t rFn seed1) $ (chooseAnyTopic t rFn seed2 s)

  return $ ( Print {pSong = song}
           , m {currSong = song}
           )
doo _ m@(Model {currSong = c}) = return (Print {pSong = c}, m)
