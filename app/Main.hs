{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless, when, void, forM, forM_, foldM)
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')
import Foreign.C.Types (CInt(..))
import Data.Word (Word8)
import Data.Text (pack)
import SDL
import SDL.Time (time, delay)
import qualified SDL.Font as TTF
import Linear (V2(..), V4(..))
import qualified Debug.Trace as T
import Controllers.Keyboard (Keyboard)
import qualified Controllers.Keyboard as K
import Controllers.Mouse (handleEvents)
import Model.Model (GameState)
import qualified Model.Model as M
import qualified Data.Map as Map
import Model.Shapes
import qualified View.TextureMap as TM
import View.TextureMap (TextureMap, TextureId(..))
import qualified View.SpriteMap as SM
import View.SpriteMap (SpriteMap, SpriteId(..))
import View.Sprite (Sprite)
import qualified View.Sprite as S
import Data.Time.Clock (getCurrentTime)

-- Définir la largeur et la hauteur de l'écran
screenWidth :: CInt
screenWidth = 1920

screenHeight :: CInt
screenHeight = 1080

-- Définir les coordonnées de chaque sprite à afficher
spriteWidth :: CInt
spriteWidth = 75

spriteHeight :: CInt
spriteHeight = 75

-- Définir la structure Button
data Button = Button
    { btnRect   :: Rectangle CInt
    , btnColor  :: V4 Word8
    , btnTextColor :: V4 Word8
    , btnLabel  :: String
    , btnAction :: IO ()
    , btnType   :: M.BuildType
    }

-- Charger un sprite
loadSprite :: Renderer -> FilePath -> TextureMap -> SpriteMap -> String -> IO (TextureMap, SpriteMap)
loadSprite rdr path tmap smap name = do
    tmap' <- TM.loadTexture rdr path (TextureId name) tmap
    let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId name) (S.mkArea 0 0 100 100)
    let smap' = SM.addSprite (SpriteId name) sprite smap
    return (tmap', smap')

-- Fonction pour créer un bouton avec une couleur, un label, une couleur de texte et une action
createButton :: Renderer -> TTF.Font -> (String, V4 Word8, V4 Word8, IO (), M.BuildType) -> V2 CInt -> IO Button
createButton renderer font (label, color, textColor, action, bType) (V2 x y) = do
    let rect = Rectangle (P (V2 x y)) (V2 100 30)  -- Taille du bouton réduite
    surface <- TTF.blended font textColor (pack label)  -- Texte avec la couleur spécifiée
    texture <- createTextureFromSurface renderer surface
    freeSurface surface
    return $ Button rect color textColor label action bType

-- Fonction pour dessiner un bouton
drawButton :: Renderer -> TTF.Font -> Button -> IO ()
drawButton renderer font (Button rect color textColor label _ _) = do
    rendererDrawColor renderer $= color
    fillRect renderer (Just rect)
    surface <- TTF.blended font textColor (pack label)  -- Texte avec la couleur spécifiée
    texture <- createTextureFromSurface renderer surface
    freeSurface surface
    TextureInfo _ _ textWidth textHeight <- queryTexture texture
    let Rectangle (P (V2 x y)) (V2 w h) = rect
    let textRect = Rectangle (P (V2 (x + (w - fromIntegral textWidth) `div` 2) (y + (h - fromIntegral textHeight) `div` 2))) (V2 (fromIntegral textWidth) (fromIntegral textHeight))
    copy renderer texture Nothing (Just textRect)
    destroyTexture texture

-- Fonction pour dessiner du texte
drawText :: Renderer -> TTF.Font -> V2 CInt -> String -> V4 Word8 -> IO ()
drawText renderer font (V2 x y) text color = do
    surface <- TTF.blended font color (pack text)  -- Texte avec la couleur spécifiée
    texture <- createTextureFromSurface renderer surface
    freeSurface surface
    TextureInfo _ _ textWidth textHeight <- queryTexture texture
    let textRect = Rectangle (P (V2 x y)) (V2 (fromIntegral textWidth) (fromIntegral textHeight))
    copy renderer texture Nothing (Just textRect)
    destroyTexture texture

-- Fonction principale
main :: IO ()
main = do
    SDL.initializeAll
    TTF.initialize
    window <- SDL.createWindow "Sims City" $ SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    font <- TTF.load "assets/AVELIRE.ttf" 18 -- Remplacez par le chemin vers votre fichier de police, taille de police à 18
    titleFont <- TTF.load "assets/AVELIRE.ttf" 24 -- Police plus grande pour le titre
    messageFont <- TTF.load "assets/AVELIRE.ttf" 36 -- Police pour les messages
    let initialMaps = (TM.createTextureMap, SM.createSpriteMap)
    spriteFiles <- return [ ("assets/background.bmp", "background")
                          , ("assets/road.bmp", "road")
                          , ("assets/water.bmp", "water")
                          , ("assets/ZR.bmp", "ZR")
                          , ("assets/ZI.bmp", "ZI")
                          , ("assets/ZC.bmp", "ZC")
                          , ("assets/Admin.bmp", "Admin")
                          , ("assets/maison.bmp", "maison")
                          , ("assets/epicerie.bmp", "epicerie")
                          , ("assets/wire.bmp", "wire")
                            , ("assets/energy.bmp", "energy")
                            , ("assets/atelier.bmp", "atelier")
                            , ("assets/cabane.bmp", "cabane")
                            , ("assets/commissariat.bmp", "commissariat")

                          ]
    (tmap, smap) <- foldM (\(tm, sm) (path, key) -> loadSprite renderer path tm sm key) initialMaps spriteFiles
    let gameState = M.initGameState
    let kbd = K.createKeyboard

    let buttonInfos = [ ("Residential $100", V4 34 139 34 255, V4 255 255 255 255, putStrLn "Residential selected", M.BuildZone (ZR (RectangleP (C 0 0) (fromIntegral spriteWidth) (fromIntegral spriteHeight)) []))
                  , ("Commercial $100", V4 255 165 0 255, V4 255 255 255 255, putStrLn "Commercial selected", M.BuildZone (ZC (RectangleP (C 0 0) (fromIntegral spriteWidth) (fromIntegral spriteHeight)) []))
                  , ("Industrial $100", V4 169 169 169 255, V4 255 255 255 255, putStrLn "Industrial selected", M.BuildZone (ZI (RectangleP (C 0 0) (fromIntegral spriteWidth) (fromIntegral spriteHeight)) []))
                  , ("Road $10", V4 105 105 105 255, V4 255 255 255 255, putStrLn "Road selected", M.BuildZone (Route (RectangleP (C 0 0) 30 30)))
                  , ("Energy $500", V4 255 0 0 255, V4 255 255 255 255, putStrLn "Energy selected", M.BuildZone (ZE (RectangleP (C 0 0) (fromIntegral spriteWidth) (fromIntegral spriteHeight))))
                  , ("Wire $5", V4 70 130 180 255, V4 255 255 255 255, putStrLn "Wire selected", M.BuildZone (Wire (RectangleP (C 0 0) (fromIntegral 30) (fromIntegral 30))))
                  , ("Police Station $100", V4 0 0 139 255, V4 255 255 255 255, putStrLn "Police Station selected", M.BuildZone (Admin (RectangleP (C 0 0) (fromIntegral spriteWidth) (fromIntegral spriteHeight)) []))]



    buttons <- forM (zip [0..] buttonInfos) $ \(i, info) -> 
        let row = i `div` 2
            col = i `mod` 2
            xPos = screenWidth - 250 + fromIntegral col * 110  -- Ajuster la position X pour ajouter du padding à droite
            yPos = 50 + fromIntegral row * 40  -- Ajuster la position Y pour laisser moins d'espace pour le titre
        in createButton renderer font info (V2 xPos yPos)


    -- Lancer la boucle de jeu
    gameLoop 60 renderer font titleFont messageFont tmap smap kbd gameState buttons

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TTF.Font -> TTF.Font -> TTF.Font -> TextureMap -> SpriteMap -> Keyboard -> GameState -> [Button] -> IO ()
gameLoop frameRate renderer font titleFont messageFont tmap smap kbd gameState buttons = do
    startTime <- time
    events <- pollEvents
    let kbd' = K.handleEvents events kbd
    let mouse = handleEvents events  

    clear renderer

    -- Afficher le fond
    mapM_ (\y -> mapM_ (\x -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "background") smap) (fromIntegral x) (fromIntegral y))) [0, spriteWidth .. screenWidth - 1]) [0, spriteHeight .. screenHeight - 1]

    -- Gestion des clics de souris
    let mouseClicks = [eventPayload e | e <- events, case eventPayload e of
                                              MouseButtonEvent _ -> True
                                              _ -> False]

    -- Mettre à jour le jeu en fonction des boutons cliqués
    let gameState' = foldl (\gs button -> 
                                if any (\e -> case e of 
                                                MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ (P (V2 x y))) -> 
                                                  pointInRect (V2 (fromIntegral x) (fromIntegral y)) (btnRect button)
                                                _ -> False) mouseClicks
                                then gs { M.selectedBuild = Just (btnType button) }
                                else gs) gameState buttons

    -- Appliquer la construction au coordonné cliqué après la sélection d'un bouton, en ignorant les clics sur les boutons
    gameState'' <- foldM (\gs e -> case e of
        MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ (P (V2 x y))) -> 
            let x' = fromIntegral x :: CInt
                y' = fromIntegral y :: CInt
            in if any (\button -> pointInRect (V2 x' y') (btnRect button)) buttons
            then return gs  -- Ignorer les clics sur les boutons
            else case M.selectedBuild gs of
                Just buildType -> do
                    let (width, height) = case buildType of
                            M.BuildZone (ZR (RectangleP _ w h) _) -> (w, h)
                            M.BuildZone (ZC (RectangleP _ w h) _) -> (w, h)
                            M.BuildZone (ZI (RectangleP _ w h) _) -> (w, h)
                            M.BuildZone (Route (RectangleP _ w h)) -> (w, h)
                            M.BuildZone (ZE (RectangleP _ w h)) -> (w, h)
                            M.BuildZone (Wire (RectangleP _ w h)) -> (w, h)
                            M.BuildZone (Admin (RectangleP _ w h) _) -> (w, h)
                        (centerX, centerY) = (x' - fromIntegral width `div` 2, y' - fromIntegral height `div` 2)
                        coord = C (fromIntegral centerX) (fromIntegral centerY)
                    result <- M.applyBuild gs buildType coord (fromIntegral width) (fromIntegral height)
                    case result of
                        Just newGameState -> return newGameState  -- Ne pas désélectionner pour permettre de placer plusieurs éléments
                        Nothing -> return gs
                Nothing -> return gs
        _ -> return gs) gameState' mouseClicks

    SM.displayVille renderer tmap smap gameState''

    -- Get current mouse position
    P (V2 mouseX mouseY) <- getAbsoluteMouseLocation

    let selectedBuildType = M.selectedBuild gameState

    -- Determine preview rectangle size and color
    let (previewWidth, previewHeight, previewColor) = case selectedBuildType of
            Just (M.BuildZone (ZR (RectangleP _ w h) _)) -> (w, h, V4 34 139 34 255)   -- Residential: Forest Green
            Just (M.BuildZone (ZC (RectangleP _ w h) _)) -> (w, h, V4 255 165 0 255)   -- Commercial: Orange
            Just (M.BuildZone (ZI (RectangleP _ w h) _)) -> (w, h, V4 169 169 169 255) -- Industrial: Dark Gray
            Just (M.BuildZone (Route (RectangleP _ w h))) -> (w, h, V4 105 105 105 255) -- Road: Slate Gray
            Just (M.BuildZone (ZE (RectangleP _ w h))) -> (w, h, V4 255 0 0 255)       -- Energy: Red
            Just (M.BuildZone (Wire (RectangleP _ w h))) -> (w, h, V4 70 130 180 255)  -- Wire: Steel Blue
            Just (M.BuildZone (Admin (RectangleP _ w h) _)) -> (w, h, V4 0 0 139 255)  -- Police Station: Dark Blue
            _ -> (fromIntegral spriteWidth, fromIntegral spriteHeight, V4 0 0 0 255)   -- Default: Black


    -- Preview of the selected zone following the mouse cursor, centered on the mouse position
    let previewRect = Rectangle (P (V2 (mouseX - fromIntegral previewWidth `div` 2) (mouseY - fromIntegral previewHeight `div` 2))) (V2 (fromIntegral previewWidth) (fromIntegral previewHeight))
    rendererDrawColor renderer $= previewColor  -- Set the color for the preview rectangle
    drawRect renderer (Just previewRect)
    
    -- Dessiner la boîte de couleur blanche derrière les boutons et l'argent
    let boxHeight = 50 + fromIntegral ((length buttons + 1) `div` 2) * 40 + 80
    let boxRect = Rectangle (P (V2 (screenWidth - 260) 5)) (V2 240 boxHeight)
    rendererDrawColor renderer $= V4 255 255 255 255  -- Couleur de la boîte (blanc opaque)
    fillRect renderer (Just boxRect)

    -- Dessiner le titre "Tools" centré en haut
    let titleText = "Tools"
    let titleWidth = 120  -- Largeur approximative du texte "Tools"
    let titleX = screenWidth - 260 + (240 - titleWidth) `div` 2
    drawText renderer titleFont (V2 titleX 10) titleText (V4 0 0 0 255)  -- Couleur du texte en noir

    -- Dessiner les boutons
    forM_ buttons $ \button -> drawButton renderer font button

  -- Afficher l'argent en dessous des boutons
    let buttonsHeight = 50 + fromIntegral ((length buttons + 1) `div` 2) * 40
    let moneyText = "Money: " ++ show (M.money gameState) ++ " $"
    drawText renderer font (V2 (screenWidth - 250) buttonsHeight) moneyText (V4 0 0 0 255)  -- Couleur du texte en noir

    -- Calculer la position Y pour le texte de la population
    let populationHeight = buttonsHeight + 30  -- Ajustez 30 selon l'espacement souhaité
    let populationText = "Population: " ++ show (M.getNumberOfVillagers gameState)
    drawText renderer font (V2 (screenWidth - 250) populationHeight) populationText (V4 0 0 0 255)  -- Couleur du texte en noir

    -- Calculer la position Y pour le texte de la pollution
    let pollutionHeight = populationHeight + 30  -- Ajustez 30 selon l'espacement souhaité
    let pollutionRate = M.getPollutionRate gameState
    let pollutionText = "Pollution: " ++ show pollutionRate
    let pollutionColor = M.getPollutionColor gameState pollutionRate
    drawText renderer font (V2 (screenWidth - 250) pollutionHeight) pollutionText pollutionColor  -- Couleur du texte basée sur la pollution

    -- Calculer la position X pour le texte de la sécurité pour qu'il soit à côté de la pollution
    let safetyX = screenWidth - 150  -- Ajustez cette valeur pour le positionnement horizontal
    let safetyRate = M.getSafetyRate gameState
    let safetyText = "Safety: " ++ show safetyRate 
    let safetyColor = M.getSafetyColor gameState safetyRate
    drawText renderer font (V2 safetyX pollutionHeight) safetyText safetyColor  -- Couleur du texte basée sur la sécurité
    
    -- Afficher les messages temporaires
    currentTime <- getCurrentTime
    case M.message gameState'' of
        Just msg | currentTime < M.msgEndTime msg -> do
            let messageWidth = 800 -- Largeur approximative du message
            let messageHeight = 50 -- Hauteur approximative du message
            let messageX = (screenWidth - messageWidth) `div` 2
            let messageY = 20
            let messageRect = Rectangle (P (V2 messageX messageY)) (V2 messageWidth messageHeight)
            rendererDrawColor renderer $= V4 255 255 255 255  -- Couleur de fond (blanc opaque)
            fillRect renderer (Just messageRect)
            drawText renderer messageFont (V2 messageX (messageY + 10)) (M.msgText msg) (V4 255 0 0 255)
        _ -> return ()

    present renderer
    endTime <- time
    let refreshTime = endTime - startTime
    let delayTime = max 0 (floor (((1.0 / frameRate) - refreshTime) * 1000))
    threadDelay (delayTime * 1000) -- microseconds
    finalGameState <- M.updateCity gameState''
    unless (K.keypressed KeycodeEscape kbd') $ gameLoop frameRate renderer font titleFont messageFont tmap smap kbd' finalGameState buttons

-- Fonction pour vérifier si un point est dans un rectangle
pointInRect :: V2 CInt -> Rectangle CInt -> Bool
pointInRect (V2 px py) (Rectangle (P (V2 rx ry)) (V2 rw rh)) =
    px >= rx && px <= (rx + rw) && py >= ry && py <= (ry + rh)

-- Fonction pour vérifier si un événement est un événement de fermeture de fenêtre
isQuitEvent :: EventPayload -> Bool
isQuitEvent QuitEvent = True
isQuitEvent _ = False