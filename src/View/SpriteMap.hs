module View.SpriteMap where

import Control.Monad.IO.Class (liftIO)
import View.Sprite 
import Data.Map.Strict (Map)
import qualified Data.Map as M
import qualified View.TextureMap as TM
import SDL.Video.Renderer (Renderer, Texture, Rectangle (..))
import Model.Shapes
import Model.Ville as V
import Model.Model

import qualified View.Sprite as S

newtype SpriteId = SpriteId String
  deriving (Eq, Ord)

instance Show SpriteId where
  show (SpriteId id) = show id

type SpriteMap = Map SpriteId Sprite

createSpriteMap :: SpriteMap
createSpriteMap = M.empty

addSprite :: SpriteId -> Sprite -> SpriteMap -> SpriteMap
addSprite sid spr tmap =
  M.insertWithKey (\_ _ _ -> error $ "addSprite - Sprite '" <> (show sid) <> "' already in sprite map.")
  sid spr tmap

fetchSprite :: SpriteId -> SpriteMap -> Sprite
fetchSprite sid smap = case M.lookup sid smap of
                         Nothing -> error $ "fetchSprite - No such Sprite: " <> (show sid)
                         Just spr -> spr

updateSprite :: (Sprite -> Sprite) -> SpriteId -> SpriteMap -> SpriteMap
updateSprite f sid smap = M.alter aux sid smap
  where aux Nothing = error $ "updateSprite - No such sprite '" <> (show sid) <> "' in sprite map."
        aux (Just old) = Just $ f old

changeSprite :: SpriteId -> Sprite -> SpriteMap -> SpriteMap
changeSprite sid spr smap = updateSprite (\_ -> spr) sid smap

removeSprite :: SpriteId -> SpriteMap -> SpriteMap
removeSprite sid smap = case M.lookup sid smap of
                          Nothing -> error $ "removeSprite - No such sprite '" <> (show sid) <> "' in sprite map."
                          Just _ -> M.delete sid smap

displayZone :: Renderer -> TM.TextureMap -> SpriteMap -> M.Map CitId Citoyen -> (ZonId, Zone) -> IO ()
displayZone renderer tmap smap citoyens (zonId, zone) = case zone of
  Route rect -> display "road" rect  -- Affiche les zones de type route
  Eau rect -> display "water" rect   -- Affiche les zones de type eau
  ZE form -> display "energy" form    -- Affiche les zones vides
  Wire form -> display "wire" form   -- Affiche les zones de type fil
  ZR form batiment -> displayAndHandleBuilding "ZR" form batiment -- Affiche les zones résidentielles et leurs bâtiments
  ZI form batiment -> displayAndHandleBuilding "ZI" form batiment -- Affiche les zones industrielles et leurs bâtiments
  ZC form batiment -> displayAndHandleBuilding "ZC" form batiment -- Affiche les zones commerciales et leurs bâtiments
  Admin form batiment -> displayAndHandleBuilding "Admin" form batiment -- Affiche les bâtiments administratifs
  where
    -- Affiche une zone en fonction de l'ID de texture et du rectangle
    display textureId rect = do
      let (n, s, w, e) = limites rect
      displaySpriteUsing textureId (n, s, w, e)

    -- Gère l'affichage des bâtiments dans une zone
    displayAndHandleBuilding :: String -> Forme -> [Batiment] -> IO ()
    displayAndHandleBuilding textureId form batiments = do
      let (n, s, w, e) = limites form
      displaySpriteUsing textureId (n, s, w, e)
      mapM_ (displayBatiment renderer tmap smap citoyens) batiments  -- Gère chaque bâtiment dans la liste

    -- Helper pour afficher un sprite en fonction des limites et de la texture
    displaySpriteUsing textureId (n, s, w, e) = do
      let areaWidth = e - w
      let areaHeight = s - n
      let sprite = S.defaultScale $ 
                   S.addImage S.createEmptySprite $ 
                   S.createImage (TM.TextureId textureId) (S.mkArea (fromIntegral w) (fromIntegral n) (fromIntegral areaWidth) (fromIntegral areaHeight))
      displaySprite renderer tmap (moveTo sprite (fromIntegral w) (fromIntegral n))

-- Fonction pour afficher un bâtiment
displayBatiment :: Renderer -> TM.TextureMap -> SpriteMap -> M.Map CitId Citoyen -> Batiment -> IO ()
displayBatiment renderer tmap smap citoyens batiment = case batiment of
  Cabane form coord _ _ _       -> displayBuilding "cabane" form coord
  Atelier form coord _ _ _       -> displayBuilding "atelier" form coord
  Epicerie form coord _ _ _      -> displayBuilding "epicerie" form coord
  Maison form coord _ _ _        -> displayBuilding "maison" form coord
  Commissariat form coord _ _ _  -> displayBuilding "commissariat" form coord
  where
    displayBuilding textureId form coord = do
      let (n, s, w, e) = limites form
      let areaWidth = e - w
      let areaHeight = s - n
      let sprite = S.defaultScale $
                   S.addImage S.createEmptySprite $
                   S.createImage (TM.TextureId textureId) (S.mkArea 0 0 (fromIntegral areaWidth) (fromIntegral areaHeight))
      -- Utilise les coordonnées de 'coord' pour positionner le sprite
      displaySprite renderer tmap (moveTo sprite (fromIntegral $ cx coord) (fromIntegral $ cy coord))

-- Utilisation de displayZone et displayBatiment
displayVille :: Renderer -> TM.TextureMap -> SpriteMap -> GameState -> IO ()
displayVille rdr tmap smap gameState = do
  let zon = V.viZones (ville gameState)
  let cit = V.viCit (ville gameState)
  mapM_ (displayZone rdr tmap smap cit) (M.toList zon)



