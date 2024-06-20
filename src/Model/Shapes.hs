module Model.Shapes where

newtype ZonId = ZonId Int deriving (Eq, Ord, Show)
newtype BatId = BatId Int deriving (Eq, Ord, Show)
newtype CitId = CitId String deriving (Eq, Ord, Show)

data Coord = C {cx :: Int, cy :: Int} 
            deriving (Show, Eq)

invariantCoord :: Coord -> Bool
invariantCoord (C x y) = x >= 0 && y >= 0

preCoord :: Int -> Int -> Bool
preCoord x y = x >= 0 && y >= 0

postCoord :: Int -> Int -> Coord -> Bool
postCoord x y (C cx cy) = cx == x && cy == y

data Forme = HSegment Coord Int
           | VSegment Coord Int
           | RectangleP Coord Int Int 
           deriving (Show, Eq)

invariantForme :: Forme -> Bool
invariantForme (HSegment _ l) = l > 0
invariantForme (VSegment _ l) = l > 0
invariantForme (RectangleP _ w h) = w > 0 && h > 0

preForme :: Forme -> Bool
preForme (HSegment _ l) = l > 0
preForme (VSegment _ l) = l > 0
preForme (RectangleP _ w h) = w > 0 && h > 0

postForme :: Forme -> Forme -> Bool
postForme _ (HSegment _ l) = l > 0
postForme _ (VSegment _ l) = l > 0
postForme _ (RectangleP _ w h) = w > 0 && h > 0

data Zone = Eau Forme
          | Route Forme
          | ZR Forme [Batiment]
          | ZI Forme [Batiment]
          | ZC Forme [Batiment]
          | Admin Forme [Batiment]
          | ZE Forme
          | Wire Forme
          deriving (Show)

invariantZone :: Zone -> Bool
invariantZone (Eau forme) = invariantForme forme
invariantZone (Route forme) = invariantForme forme
invariantZone (ZR forme batiments) = invariantForme forme && all invariantBatiment batiments
invariantZone (ZI forme batiments) = invariantForme forme && all invariantBatiment batiments
invariantZone (ZC forme batiments) = invariantForme forme && all invariantBatiment batiments
invariantZone (Admin forme batiments) = invariantForme forme && all invariantBatiment batiments
invariantZone (ZE forme) = invariantForme forme
invariantZone (Wire forme) = invariantForme forme

preZone :: Zone -> Bool
preZone (Eau forme) = invariantForme forme
preZone (Route forme) = invariantForme forme
preZone (ZR forme batiments) = invariantForme forme && all invariantBatiment batiments
preZone (ZI forme batiments) = invariantForme forme && all invariantBatiment batiments
preZone (ZC forme batiments) = invariantForme forme && all invariantBatiment batiments
preZone (Admin forme batiments) = invariantForme forme && all invariantBatiment batiments
preZone (ZE forme) = invariantForme forme
preZone (Wire forme) = invariantForme forme

postZone :: Zone -> Zone -> Bool
postZone _ zone = invariantZone zone

data Batiment = Cabane  Forme Coord ZonId BatId [CitId]
                | Atelier  Forme Coord ZonId BatId [CitId]
                | Epicerie  Forme Coord ZonId BatId [CitId]
                | Maison  Forme Coord ZonId BatId [CitId]
                | Commissariat  Forme Coord ZonId BatId [CitId]

invariantBatiment :: Batiment -> Bool
invariantBatiment (Cabane forme coord _ _ _) = invariantForme forme && invariantCoord coord
invariantBatiment (Atelier forme coord _ _ _) = invariantForme forme && invariantCoord coord
invariantBatiment (Epicerie forme coord _ _ _) = invariantForme forme && invariantCoord coord
invariantBatiment (Maison forme coord _ _ _) = invariantForme forme && invariantCoord coord
invariantBatiment (Commissariat forme coord _ _ _) = invariantForme forme && invariantCoord coord

preBatiment :: Batiment -> Bool
preBatiment (Cabane forme coord _ _ _) = invariantForme forme && invariantCoord coord
preBatiment (Atelier forme coord _ _ _) = invariantForme forme && invariantCoord coord
preBatiment (Epicerie forme coord _ _ _) = invariantForme forme && invariantCoord coord
preBatiment (Maison forme coord _ _ _) = invariantForme forme && invariantCoord coord
preBatiment (Commissariat forme coord _ _ _) = invariantForme forme && invariantCoord coord

postBatiment :: Batiment -> Batiment -> Bool
postBatiment _ batiment = invariantBatiment batiment


instance Show Batiment where
  show (Cabane forme coord zonid batid citIds) = "Cabane " ++ show forme ++ " " ++ show coord ++ " " ++ show zonid ++ " " ++ show batid ++ " " ++ show citIds 
  show (Atelier forme coord zonid batid citIds) = "Atelier " ++ show forme ++ " " ++ show coord ++ " " ++ show zonid ++ " " ++ show batid ++ " " ++ show citIds
  show (Epicerie forme coord zonid batid citIds) = "Epicerie " ++ show forme ++ " " ++ show coord ++ " " ++ show zonid ++ " " ++ show batid ++ " " ++ show citIds
  show (Commissariat forme coord zonid batid citIds) = "Commissariat " ++ show forme ++ " " ++ show coord  ++ " " ++ show zonid ++ " " ++ show batid ++ " " ++ show citIds
  show (Maison forme coord zonid batid citIds) = "Maison " ++ show forme ++ " " ++ show coord ++ " " ++ show zonid ++ " " ++ show batid ++ " " ++ show citIds


instance Eq Zone where
  (Eau forme1) == (Eau forme2) = forme1 == forme2
  (Route forme1) == (Route forme2) = forme1 == forme2
  (ZR forme1 _) == (ZR forme2 _) = forme1 == forme2 
  (ZI forme1 _) == (ZI forme2 _) = forme1 == forme2
  (ZC forme1 _) == (ZC forme2 _) = forme1 == forme2
  (ZE forme1) == (ZE forme2) = forme1 == forme2
  (Wire forme1) == (Wire forme2) = forme1 == forme2
  (Admin forme1 _) == (Admin forme2 _) = forme1 == forme2
  _ == _ = False

zoneForme :: Zone -> Forme
zoneForme (Eau forme) = forme
zoneForme (Route forme) = forme
zoneForme (ZR forme _) = forme
zoneForme (ZI forme _) = forme
zoneForme (ZC forme _) = forme
zoneForme (Admin forme _) = forme
zoneForme (ZE forme) = forme
zoneForme (Wire forme) = forme

zonesDijoints :: Zone -> Zone -> Bool
zonesDijoints zone1 zone2 = not (collision (zoneForme zone1) (zoneForme zone2))

limites :: Forme -> (Int, Int, Int, Int)
limites (HSegment (C x y) l) = (y, y, x, x + l - 1)
limites (VSegment (C x y) l) = (y, y - l - 1, x, x)
limites (RectangleP (C x y) w h) = (y, y + h - 1, x, x + w - 1)

preAppartient :: Coord -> Forme -> Bool
preAppartient (C x y) forme = x >= 0 && y >= 0

appartient :: Coord -> Forme -> Bool
appartient (C x y) forme = let (nord, sud, ouest, est) = limites forme
                           in y >= nord && y <= sud && x >= ouest && x <= est

postAppartient :: Coord -> Forme -> Bool -> Bool
postAppartient coord forme result =
  if result
  then coord `appartient` forme
  else not (coord `appartient` forme)

adjacent :: Coord -> Forme -> Bool
adjacent (C x y) forme = let (nord, sud, ouest, est) = limites forme
                         in (y == nord + 1 && x >= ouest && x <= est) ||
                            (y == sud - 1 && x >= ouest && x <= est) ||
                            (x == ouest + 1 && y >= nord && y <= sud) ||
                            (x == est - 1 && y >= nord && y <= sud)

postAdjacent :: Coord -> Forme -> Bool -> Bool
postAdjacent coord forme result =
  if result
  then coord `adjacent` forme
  else not (coord `adjacent` forme)

preFormesAdjacentes :: Forme -> Forme -> Bool
preFormesAdjacentes forme1 forme2 = True  -- Aucune précondition spécifique

-- Fonction pour déterminer si deux formes sont adjacentes avec une tolérance 
formesAdjacentes :: Forme -> Forme -> Bool
formesAdjacentes forme1 forme2 =
    let (minY1, maxY1, minX1, maxX1) = limites forme1
        (minY2, maxY2, minX2, maxX2) = limites forme2
        tolerance = 25
    in ((maxY1 + tolerance >= minY2 && maxY1 <= minY2) || (minY1 >= maxY2 && minY1 <= maxY2 + tolerance)) 
        && (minX1 <= maxX2 && maxX1 >= minX2)
        || ((maxX1 + tolerance >= minX2 && maxX1 <= minX2) || (minX1 >= maxX2 && minX1 <= maxX2 + tolerance)) 
        && (minY1 <= maxY2 && maxY1 >= minY2)

postFormesAdjacentes :: Forme -> Forme -> Bool -> Bool
postFormesAdjacentes forme1 forme2 result =
  if result
  then formesAdjacentes forme1 forme2
  else not (formesAdjacentes forme1 forme2)

preZonesAdjacentes :: Zone -> Zone -> Bool
preZonesAdjacentes zone1 zone2 = True  -- Aucune précondition spécifique

-- Fonction pour vérifier si deux zones sont adjacentes
zonesAdjacentes :: Zone -> Zone -> Bool
zonesAdjacentes zone1 zone2 = formesAdjacentes (zoneForme zone1) (zoneForme zone2)


postZonesAdjacentes :: Zone -> Zone -> Bool -> Bool
postZonesAdjacentes zone1 zone2 result =
  if result
  then zonesAdjacentes zone1 zone2
  else not (zonesAdjacentes zone1 zone2)

preZoneCollision :: Zone -> Zone -> Bool
preZoneCollision zone1 zone2 = True  -- Aucune précondition spécifique

zoneCollision :: Zone -> Zone -> Bool
zoneCollision zone1 zone2 = collision (zoneForme zone1) (zoneForme zone2)

postZoneCollision :: Zone -> Zone -> Bool -> Bool
postZoneCollision zone1 zone2 result =
  if result
  then zoneCollision zone1 zone2
  else not (zoneCollision zone1 zone2)

preCollision :: Forme -> Forme -> Bool
preCollision forme1 forme2 = True  -- Aucune précondition spécifique

-- c'est pas exact ça 
collision :: Forme -> Forme -> Bool
collision forme1 forme2 = 
  let (nord1, sud1, ouest1, est1) = limites forme1
      (nord2, sud2, ouest2, est2) = limites forme2
  in not ((est1 < ouest2) || (ouest1 > est2) || (sud1 < nord2) || (nord1 > sud2))

postCollision :: Forme -> Forme -> Bool -> Bool
postCollision forme1 forme2 result =
  if result
  then collision forme1 forme2
  else not (collision forme1 forme2)

preUpdateZoneCoord :: Zone -> Coord -> Int -> Int -> Bool
preUpdateZoneCoord _ (C x y) w h = x >= 0 && y >= 0 && w > 0 && h > 0

-- Helper functions to update coordinates
updateZoneCoord :: Zone -> Coord -> Int -> Int -> Zone
updateZoneCoord (ZR _ bs) coord w h = ZR (RectangleP coord w h) bs
updateZoneCoord (ZI _ bs) coord w h = ZI (RectangleP coord w h) bs
updateZoneCoord (ZC _ bs) coord w h = ZC (RectangleP coord w h) bs
updateZoneCoord (Route _) coord w h = Route (RectangleP coord w h)
updateZoneCoord (Eau _) coord w h = Eau (RectangleP coord w h)
updateZoneCoord (Admin _ b) coord w h = Admin (RectangleP coord w h) b
updateZoneCoord (ZE _) coord w h = ZE (RectangleP coord w h)
updateZoneCoord (Wire _) coord w h = Wire (RectangleP coord w h)

postUpdateZoneCoord :: Zone -> Coord -> Int -> Int -> Zone -> Bool
postUpdateZoneCoord _ (C x y) w h zoneUpdated =
  let formeUpdated = zoneForme zoneUpdated
      (nord, sud, ouest, est) = limites formeUpdated
  in nord == y && ouest == x && (est - ouest + 1) == w && (sud - nord + 1) == h
