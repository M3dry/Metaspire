module Main where

main :: IO ()
main = putStrLn "Metaspire"

-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeFamilies #-}

-- module Main where

-- import Apecs
-- import Raylib.Core qualified as RL
-- import Raylib.Core qualified as Rl
-- import Raylib.Core.Camera qualified as RL
-- import Raylib.Types (Vector3 (..))
-- import Raylib.Types qualified as RL
-- import Raylib.Util qualified as RL
-- import Raylib.Util.Colors qualified as RL
-- import Control.Monad (unless)

-- newtype Camera = Camera RL.Camera3D

-- instance Component Camera where type Storage Camera = Map Camera

-- makeWorld "World" [''Camera]

-- initialise :: System World RL.WindowResources
-- initialise = do
--     let camera = RL.Camera3D (Vector3 0 1 0) (Vector3 2 1 1) (Vector3 0 1 0) 70 RL.CameraPerspective

--     set global $ Camera camera
--     liftIO $ do
--         window <- RL.initWindow 1920 1200 "Metaspire"
--         RL.setTargetFPS 60
--         RL.setExitKey RL.KeyNull
--         return window

-- run :: System World ()
-- run = do
--     undefined

--     shouldClose <- liftIO RL.windowShouldClose
--     unless shouldClose run

-- terminate :: RL.WindowResources -> System World ()
-- terminate = liftIO . Rl.closeWindow

-- main :: IO ()
-- main =
--     initWorld
--         >>= runSystem
--             ( do
--                 window <- initialise
--                 run
--                 terminate window
--             )
