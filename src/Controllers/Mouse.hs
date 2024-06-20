module Controllers.Mouse (handleEvents) where

import SDL
import Data.List (foldl')

handleEvent :: Event -> Maybe (Int, Int) -> Maybe (Int, Int)
handleEvent event acc =
  case eventPayload event of
    MouseButtonEvent eventData -> handleMouseButtonEvent eventData acc
    _ -> acc

-- | Prise en compte des événements SDL2 pour mettre à jour l'état de la souris
handleEvents :: [Event] -> Maybe (Int, Int)
handleEvents events = foldl' (flip handleEvent) Nothing events

handleMouseButtonEvent :: MouseButtonEventData -> Maybe (Int, Int) -> Maybe (Int, Int)
handleMouseButtonEvent eventData acc =
  case eventData of
    MouseButtonEventData _ Pressed _ ButtonLeft _ (P (V2 x y)) ->
      Just (fromIntegral x, fromIntegral y)
    _ -> acc
