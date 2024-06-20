module Model.Ville where

import qualified Data.Map as M  -- Utilisation d'un alias pour Data.Map
import Model.Shapes
import Data.List as List
import Data.Maybe (isJust, maybeToList, listToMaybe)
import qualified Data.Maybe as DM (mapMaybe)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Set as Set
import Data.Ord (comparing)

import qualified Data.PQueue.Min as PQueue
import qualified Debug.Trace as Debug (trace)

type Graph = M.Map ZonId [ZonId]

data Citoyen = Immigrant Coord (Int, Int, Int) Occupation CitId
             | Habitant Coord (Int, Int, Int) (BatId, Maybe BatId, Maybe BatId) Occupation CitId
             | Emigrant Coord Occupation CitId
              deriving (Eq)

invariantCitoyen :: Citoyen -> Bool
invariantCitoyen (Immigrant coord (argent, fatigue, faim) _ _) =
  invariantCoord coord && argent >= 0 && argent <= 100 && fatigue >= 0 && fatigue <= 100 && faim >= 0 && faim <= 100
invariantCitoyen (Habitant coord (argent, fatigue, faim) _ _ _) =
  invariantCoord coord && argent >= 0 && argent <= 100 && fatigue >= 0 && fatigue <= 100 && faim >= 0 && faim <= 100
invariantCitoyen (Emigrant coord _ _) =
  invariantCoord coord

preCitoyen :: Citoyen -> Bool
preCitoyen (Immigrant coord (argent, fatigue, faim) _ _) =
  invariantCoord coord && argent >= 0 && argent <= 100 && fatigue >= 0 && fatigue <= 100 && faim >= 0 && faim <= 100
preCitoyen (Habitant coord (argent, fatigue, faim) _ _ _) =
  invariantCoord coord && argent >= 0 && argent <= 100 && fatigue >= 0 && fatigue <= 100 && faim >= 0 && faim <= 100
preCitoyen (Emigrant coord _ _) =
  invariantCoord coord

postCitoyen :: Citoyen -> Citoyen -> Bool
postCitoyen _ (Immigrant coord (argent, fatigue, faim) _ _) =
  invariantCoord coord && argent >= 0 && argent <= 100 && fatigue >= 0 && fatigue <= 100 && faim >= 0 && faim <= 100
postCitoyen _ (Habitant coord (argent, fatigue, faim) _ _ _) =
  invariantCoord coord && argent >= 0 && argent <= 100 && fatigue >= 0 && fatigue <= 100 && faim >= 0 && faim <= 100
postCitoyen _ (Emigrant coord _ _) =
  invariantCoord coord

data Occupation = Travailler
                | Dormir
                | Shopping
                | Move Coord
                deriving (Show, Eq)

invariantOccupation :: Occupation -> Bool
invariantOccupation _ = True

preOccupation :: Occupation -> Bool
preOccupation _ = True

postOccupation :: Occupation -> Occupation -> Bool
postOccupation _ _ = True

data Ville = Ville
    { viZones :: M.Map ZonId Zone
    , viCit :: M.Map CitId Citoyen
    }
    deriving (Show)

instance Eq Ville where
    (Ville zones1 cits1) == (Ville zones2 cits2) = 
        zones1 == zones2 && cits1 == cits2

type VilleInvariant = Ville -> Bool
villeInvariant :: VilleInvariant
villeInvariant = prop_ville_sansCollision 

-- Extraire les IDs de route dans la ville
extraireRoutes :: Ville -> [ZonId]
extraireRoutes ville = [zid | (zid, zone) <- M.toList (viZones ville), isRoute zone]
  where
    isRoute (Route _) = True
    isRoute _ = False

-- Crée un graphe de la ville en connectant les zones adjacentes aux routes
createGraph :: Ville -> Graph
createGraph ville = M.fromList [(zid, getAdjacentRouteIds ville zid) | (zid, zone) <- M.toList (viZones ville), isRoute zone]
  where
    isRoute (Route _) = True
    isRoute _ = False

-- Retourne les ZonId adjacents à une zone donnée par son ZonId
getAdjacentRouteIds :: Ville -> ZonId -> [ZonId]
getAdjacentRouteIds ville zid = [zid1
                                | (zid1, zone1) <- M.toList (viZones ville)
                                , isRoute zone1
                                , zonesAdjacentes (viZones ville M.! zid) zone1
                                , zid1 /= zid]
  where
    isRoute (Route _) = True
    isRoute _ = False

-- Vérifie si le graphe est connecté
isConnected :: Graph -> Bool
isConnected graph
    | M.null graph = True
    | otherwise = Set.size (reachableNodes (head $ M.keys graph) graph Set.empty) == M.size graph

-- Renvoie les nœuds atteignables dans un graphe
reachableNodes :: ZonId -> Graph -> Set.Set ZonId -> Set.Set ZonId
reachableNodes node graph visited
    | Set.member node visited = visited
    | otherwise = List.foldr (\n v -> reachableNodes n graph v) (Set.insert node visited) (M.findWithDefault [] node graph)

-- Propriété vérifiant que les routes sont connexes
prop_routes_connexes :: Ville -> Bool
prop_routes_connexes = isConnected . createGraph

-- Propriété vérifiant que la ville n'a pas de collisions
prop_ville_sansCollision :: Ville -> Bool
prop_ville_sansCollision ville = prop_zones_dijoints ville && prop_zone_adjacent_route ville && prop_routes_connexes ville && prop_wire_wireorelectrique ville

-- Affiche la ville
showVille :: Ville -> String
showVille ville = unlines $ map showZone (M.toList (viZones ville)) ++ lines (showCitoyens (viCit ville))
  where
    showZone (zid, zone) = "Zone ID: " ++ show zid ++ ", " ++ show zone

-- Affiche les citoyens
showCitoyens :: M.Map CitId Citoyen -> String
showCitoyens citoyens = unlines $ map show (M.toList citoyens)

-- Instance de Show pour Citoyen
instance Show Citoyen where
  show (Immigrant coord etat occupation cid) =
    "Immigrant { Coord: " ++ show coord ++
    ", Etat: " ++ show etat ++
    ", Occupation: " ++ show occupation ++
    ", CitId: " ++ show cid ++ " }"
  show (Habitant coord etat (residenceId, workId, shopId) occupation cid) =
    "Habitant { Coord: " ++ show coord ++
    ", Etat: " ++ show etat ++
    ", Résidence: " ++ show residenceId ++
    ", Travail: " ++ show workId ++
    ", Magasin: " ++ show shopId ++
    ", Occupation: " ++ show occupation ++
    ", CitId: " ++ show cid ++ " }"
  show (Emigrant coord occupation cid) =
    "Emigrant { Coord: " ++ show coord ++
    ", Occupation: " ++ show occupation ++
    ", CitId: " ++ show cid ++ " }"

-- Ajout des bâtiments aux zones alimentées
addBuildingsToPoweredZones :: Ville -> Ville
addBuildingsToPoweredZones ville = ville { viZones = M.mapWithKey addBuildingToZone (viZones ville) }
  where
    addBuildingToZone :: ZonId -> Zone -> Zone
    addBuildingToZone zid zone =
      if isPowered ville zone
        then case zone of
          ZR forme buildings -> ZR forme (addBuilding buildings zid forme createMaison)
          ZI forme buildings -> ZI forme (addBuilding buildings zid forme createAtelier)
          ZC forme buildings -> ZC forme (addBuilding buildings zid forme createEpicerie)
          Admin forme _ -> Admin forme ([createCommissariat zid (centerCoord forme) (commissariatWidth, commissariatHeight)])
          _ -> zone
      else zone

    addBuilding :: [Batiment] -> ZonId -> Forme -> (ZonId -> Coord -> Batiment) -> [Batiment]
    addBuilding buildings zid forme@(RectangleP (C x y) w h) constructor
      | length buildings < maxBuildings = buildings ++ [newBuilding]
      | otherwise = buildings
      where
        maxBuildings = 4  -- Nombre maximum de bâtiments autorisés dans une zone
        newBuilding = constructor zid newCoord
        newCoord = calcNewCoord (length buildings) forme
        
        calcNewCoord :: Int -> Forme -> Coord
        calcNewCoord index (RectangleP (C x y) width height) =
          let buildingWidth = 30
              buildingHeight = 30
              maxPerRow = width `div` buildingWidth
              newX = x + (index `mod` maxPerRow) * buildingWidth + 5
              newY = y + (index `div` maxPerRow) * buildingHeight + 5
          in C newX newY

    centerCoord :: Forme -> Coord
    centerCoord (RectangleP (C x y) width height) =
      let newX = x + (width `div` 2) - (commissariatWidth `div` 2)
          newY = y + (height `div` 2) - (commissariatHeight `div` 2)
      in C newX newY
    
    commissariatWidth :: Int
    commissariatWidth = 60

    commissariatHeight :: Int
    commissariatHeight = 60

    createMaison :: ZonId -> Coord -> Batiment
    createMaison zid coord = Maison (RectangleP coord 30 30) coord zid (BatId (hashCoord coord)) []

    createAtelier :: ZonId -> Coord -> Batiment
    createAtelier zid coord = Atelier (RectangleP coord 30 30) coord zid (BatId (hashCoord coord)) []

    createEpicerie :: ZonId -> Coord -> Batiment
    createEpicerie zid coord = Epicerie (RectangleP coord 30 30) coord zid (BatId (hashCoord coord)) []

    createCommissariat :: ZonId -> Coord -> (Int, Int) -> Batiment
    createCommissariat zid coord (width, height) = Commissariat (RectangleP coord width height) coord zid (BatId (hashCoord coord)) []

    hashCoord :: Coord -> Int
    hashCoord (C x y) = x * 100 + y

preIsPowered :: Ville -> Zone -> Bool
-- Vérifie que la zone existe dans la ville
preIsPowered ville zone = zone `elem` M.elems (viZones ville) || isWire zone || isZE zone
  where
    isZE (ZE _) = True
    isZE _ = False

-- Vérifie si une zone est alimentée
isPowered :: Ville -> Zone -> Bool
isPowered _ (Wire _) = True
isPowered _ (ZE _) = True 
isPowered ville zone = any isWire (getAdjacent ville zone)

postIsPowered :: Ville -> Zone -> Bool -> Bool
-- Vérifie que le résultat est un Booléen valide et qu'il correspond à l'état attendu
postIsPowered ville zone result =
  if result
  then any isWire (getAdjacent ville zone) || isWire zone || isZE zone
  else not (any isWire (getAdjacent ville zone))
  where
    isZE (ZE _) = True
    isZE _ = False

preIsWire :: Zone -> Bool
-- Aucune précondition spécifique, toute Zone est valide
preIsWire _ = True

-- Vérifie si une zone est un fil ou une zone d'énergie
isWire :: Zone -> Bool
isWire (Wire _) = True
isWire _ = False

postIsWire :: Zone -> Bool -> Bool
postIsWire zone result =
  if result
  then case zone of
         Wire _ -> True
         _ -> False
  else case zone of
         Wire _ -> False
         _ -> True

-- Propriété vérifiant que les fils ou zones d'énergie sont connectés
prop_wire_wireorelectrique :: Ville -> Bool
prop_wire_wireorelectrique ville = 
  Prelude.foldr (&&) True allComparisons
  where
    allComparisons = [besideWireOrZE zone1 $ getAdjacent ville zone1
                     | (zid1, zone1) <- M.toList (viZones ville)
                     , isWire zone1
                     ]

    besideWireOrZE :: Zone -> [Zone] -> Bool
    besideWireOrZE _ zonesList = contientWireOrZE zonesList

    contientWireOrZE :: [Zone] -> Bool
    contientWireOrZE zones = any isWireOrZE zones

    isWireOrZE :: Zone -> Bool
    isWireOrZE (Wire _) = True
    isWireOrZE (ZE _) = True
    isWireOrZE _ = False

-- Propriété vérifiant que les zones sont disjointes
prop_zones_dijoints :: Ville -> Bool
prop_zones_dijoints ville = 
  Prelude.foldr (&&) True allComparisons
  where
    allComparisons = [not (zoneCollision zone1 zone2)
                     | (zid1, zone1) <- M.toList (viZones ville)
                     , (zid2, zone2) <- M.toList (viZones ville)
                     , zid1 /= zid2
                     ]

-- Propriété vérifiant que les zones sont adjacentes à une route
prop_zone_adjacent_route :: Ville -> Bool
prop_zone_adjacent_route ville =
    Prelude.foldr (&&) True allComparisons
  where
    allComparisons = [besideRoad zone1 $ getAdjacent ville zone1
                     | (zid1, zone1) <- M.toList (viZones ville)
                     ]

-- Vérifie si une liste de zones contient une route
contientRoute :: [Zone] -> Bool
contientRoute zones = any isRoute zones
  where
    isRoute (Route _) = True
    isRoute _ = False

-- Vérifie si une zone est à côté d'une route
besideRoad :: Zone -> [Zone] -> Bool
besideRoad (Route _) _ = True
besideRoad (Eau _) _ = True
besideRoad (ZE _) _ = True
besideRoad (Wire _) _ = True
besideRoad _ zonesList = contientRoute zonesList

-- Obtient les zones adjacentes à une zone donnée
getAdjacent :: Ville -> Zone -> [Zone]
getAdjacent ville zone = [zone1
                         | (_, zone1) <- M.toList (viZones ville)
                         , zonesAdjacentes zone zone1
                         ]

-- Pré-construction d'une zone
preConstruit :: Ville -> Zone -> ZonId -> Bool
preConstruit ville zone zonId = 
    let updatedVille = ville { viZones = M.insert zonId zone (viZones ville) }
    in  prop_ville_sansCollision updatedVille

-- Construction d'une zone
construit :: Ville -> Zone -> ZonId -> Ville
construit ville zone zonId = Ville { viZones = M.insert zonId zone (viZones ville), viCit = viCit ville }

-- Post-construction d'une zone
postConstruit :: Ville -> Zone -> ZonId -> Ville -> Bool
postConstruit villePre zone zonId villePost =
    villeInvariant villePost &&
    isJust (M.lookup zonId (viZones villePost))

-- Obtient la forme d'un bâtiment
getForm :: Batiment -> Forme
getForm (Cabane form _ _ _ _) = form
getForm (Atelier form _ _ _ _) = form
getForm (Epicerie form _ _ _ _) = form
getForm (Commissariat form _ _ _ _) = form
getForm (Maison form _ _ _ _) = form

preAddBatiment :: Batiment -> Ville -> ZonId -> Bool
preAddBatiment batiment ville zonId =
  case M.lookup zonId (viZones ville) of
    Just zone -> canAddBuilding batiment zone
    Nothing -> False

-- Ajoute un bâtiment à une zone de la ville
addBatiment :: Batiment -> Ville -> ZonId -> Ville
addBatiment batiment ville zonId = ville { viZones = M.adjust (insertBatiment batiment) zonId (viZones ville) }
  where
    insertBatiment :: Batiment -> Zone -> Zone
    insertBatiment b (ZR form bs) = ZR form (b:bs)
    insertBatiment b (ZI form bs) = ZI form (b:bs)
    insertBatiment b (ZC form bs) = ZC form (b:bs)
    insertBatiment _ zone = zone

postAddBatiment :: Batiment -> Ville -> ZonId -> Ville -> Bool
postAddBatiment batiment _ zonId newVille =
  isJust $ find (\b -> getBatimentId b == getBatimentId batiment) (getBatiments (viZones newVille M.! zonId))

-- Génère le prochain ID de zone
nextZonId :: Ville -> ZonId
nextZonId ville = ZonId (M.size (viZones ville) + 1)

-- Vérifie si la forme d'un bâtiment est dans les limites d'une zone
isWithin :: Forme -> Forme -> Bool
isWithin buildingForm zoneForm =
  let (minY1, maxY1, minX1, maxX1) = limites buildingForm
      (minY2, maxY2, minX2, maxX2) = limites zoneForm
  in (maxY1 <= maxY2 && minY1 >= minY2) && (maxX1 <= maxX2 && minX1 >= minX2)

-- Vérifie si deux formes se chevauchent
isOverlapping :: Forme -> Forme -> Bool
isOverlapping f1 f2 =
  let (minY1, maxY1, minX1, maxX1) = limites f1
      (minY2, maxY2, minX2, maxX2) = limites f2
  in not (maxY1 < minY2 || minY1 > maxY2 || maxX1 < minX2 || minX1 > maxX2)

-- Vérifie si un bâtiment peut être ajouté à une zone
canAddBuilding :: Batiment -> Zone -> Bool
canAddBuilding (Cabane form _ _ _ _) (ZR zoneForm buildings) =
    isWithin form zoneForm && not (any (isOverlapping form . getForm) buildings)
canAddBuilding _ _ = False

invariantcreateImmigrant:: Ville -> Bool
invariantcreateImmigrant ville =
  let poweredZones = filter (isPowered ville) (map snd (M.toList (viZones ville)))
      residentialZones = countZones isResidentialZone poweredZones
      commercialZones = countZones isCommercialZone poweredZones
      industrialZones = countZones isIndustrialZone poweredZones
  in residentialZones > 0 && residentialZones <= commercialZones + industrialZones
  where
    countZones predicate zones = length [zone | zone <- zones, predicate zone]
    isResidentialZone (ZR _ _) = True
    isResidentialZone _ = False
    isCommercialZone (ZC _ _) = True
    isCommercialZone _ = False
    isIndustrialZone (ZI _ _) = True
    isIndustrialZone _ = False

-- Crée un citoyen immigrant
createImmigrant :: Coord -> CitId -> Citoyen
createImmigrant coord cid = Immigrant coord (1000, 100, 100) (Move coord) cid

preUpdateCitizens :: Ville -> Bool
preUpdateCitizens _ = True  -- Aucune précondition spécifique

-- Met à jour l'occupation et la position des citoyens
updateCitizens :: Ville -> Ville
updateCitizens ville = ville { viCit = M.map (mettreAJourPosition ville) (viCit ville) }

postUpdateCitizens :: Ville -> Ville -> Bool
postUpdateCitizens _ newVille =
  all (invariantCitoyen . snd) (M.toList (viCit newVille))

-- Détermine la prochaine occupation d'un citoyen
nextOccupation :: Citoyen -> Maybe Occupation
nextOccupation citoyen@(Habitant _ (_, fatigue, faim) _ _ _) 
  | faim < 30 = Just Shopping
  | fatigue < 30 = Just Dormir
  | otherwise = Just Travailler
nextOccupation _ = Nothing

preMettreAJourPosition :: Ville -> Citoyen -> Bool
preMettreAJourPosition _ _ = True  -- Aucune précondition spécifique

-- Met à jour la position des citoyens
mettreAJourPosition :: Ville -> Citoyen -> Citoyen
mettreAJourPosition ville citoyen@(Habitant coord (argent, fatigue, faim) batiments occupation cid) =
  let citoyenUpdated = if not (isAtHome ville citoyen)
                       then setCitoyenEtat citoyen (argent, max 0 (fatigue - 1), max 0 (faim - 1))
                       else citoyen
  in case getOccupation citoyenUpdated of
       Move destination ->
         let updatedCitoyen = setCitoyenCoord citoyenUpdated destination
             nextOcc = nextOccupation citoyenUpdated
         in case nextOcc of
              Just occ -> setOccupation updatedCitoyen occ
              Nothing -> updatedCitoyen
       Travailler -> 
         if faim < 20 
         then setOccupation citoyenUpdated (Move (coordOfShop ville citoyen))
         else if fatigue < 20 
         then setOccupation citoyenUpdated (Move (coordOfResidence ville citoyen))
         else 
           let newArgent = argent + 200
               newFatigue = max 0 (fatigue - 10)
               newFaim = max 0 (faim - 5)
           in setCitoyenEtat (setOccupation citoyenUpdated Travailler) (newArgent, newFatigue, newFaim)
       Dormir -> 
         if fatigue >= 80 
         then setOccupation citoyenUpdated (Move (coordOfWork ville citoyen))
         else 
           let newFatigue = min 100 (fatigue + 20)
           in setCitoyenEtat (setOccupation citoyenUpdated Dormir) (argent, newFatigue, faim)
       Shopping -> 
         if faim >= 80 
         then setOccupation citoyenUpdated (Move (coordOfWork ville citoyen))
         else 
           let newFaim = min 100 (faim + 20)
           in setCitoyenEtat (setOccupation citoyenUpdated Shopping) (argent, fatigue, newFaim)
mettreAJourPosition _ citoyen = citoyen

postMettreAJourPosition :: Ville -> Citoyen -> Citoyen -> Bool
postMettreAJourPosition ville citoyen updatedCitoyen =
  citoyenCoord updatedCitoyen == citoyenCoord citoyen &&
  getOccupation updatedCitoyen == getOccupation citoyen

-- Helper pour obtenir les coordonnées d'un magasin
coordOfShop :: Ville -> Citoyen -> Coord
coordOfShop ville (Habitant _ _ (_, _, Just shopId) _ _) = getCoordOfBuildingById ville shopId
coordOfShop _ _ = error "Only residents with shops have shop coordinates"

-- Helper pour obtenir les coordonnées d'un bâtiment à partir de son ID
getCoordOfBuildingById :: Ville -> BatId -> Coord
getCoordOfBuildingById ville batId = 
  case findBuilding batId (viZones ville) of
    Just building -> coordOfBuilding building
    Nothing -> error $ "Building with BatId " ++ show batId ++ " not found"

-- Vérifie si un citoyen est chez lui
isAtHome :: Ville -> Citoyen -> Bool
isAtHome ville (Habitant coord _ (residenceId, _, _) _ _) = coord == coordOfResidenceId residenceId ville
isAtHome _ _ = False

-- Obtient les coordonnées de la résidence d'un citoyen
coordOfResidence :: Ville -> Citoyen -> Coord
coordOfResidence ville (Habitant _ _ (residenceId, _, _) _ _) = coordOfResidenceId residenceId ville
coordOfResidence _ _ = error "Only residents have residences"

-- Obtient les coordonnées du lieu de travail d'un citoyen
coordOfWork :: Ville -> Citoyen -> Coord
coordOfWork ville (Habitant _ _ (_, Just workId, _) _ _) = coordOfWorkId workId ville
coordOfWork _ _ = error "Only employed residents have workplaces"

-- Définit l'état d'un citoyen
setCitoyenEtat :: Citoyen -> (Int, Int, Int) -> Citoyen
setCitoyenEtat (Habitant coord _ batiments occupation cid) newEtat = Habitant coord newEtat batiments occupation cid
setCitoyenEtat citoyen _ = citoyen

-- Obtient les coordonnées de la résidence par son ID
coordOfResidenceId :: BatId -> Ville -> Coord
coordOfResidenceId batId ville = getCoordOfBuildingById ville batId

-- Obtient les coordonnées du lieu de travail par son ID
coordOfWorkId :: BatId -> Ville -> Coord
coordOfWorkId batId ville = getCoordOfBuildingById ville batId

-- Helper pour trouver un bâtiment par son BatId
findBuilding :: BatId -> M.Map ZonId Zone -> Maybe Batiment
findBuilding batId zones = listToMaybe [b | (_, zone) <- M.toList zones, b <- getBatiments zone, getBatimentId b == batId]

-- Helper pour obtenir la liste des bâtiments dans une zone
getBatiments :: Zone -> [Batiment]
getBatiments (ZR _ batiments) = batiments
getBatiments (ZI _ batiments) = batiments
getBatiments (ZC _ batiments) = batiments
getBatiments _ = []

-- Helper pour obtenir les coordonnées d'un bâtiment
coordOfBuilding :: Batiment -> Coord
coordOfBuilding (Cabane _ coord _ _ _) = coord
coordOfBuilding (Maison _ coord _ _ _) = coord
coordOfBuilding (Atelier _ coord _ _ _) = coord
coordOfBuilding (Epicerie _ coord _ _ _) = coord
coordOfBuilding (Commissariat _ coord _ _ _) = coord

-- Helper pour obtenir les coordonnées d'un citoyen
citoyenCoord :: Citoyen -> Coord
citoyenCoord (Immigrant coord _ _ _) = coord
citoyenCoord (Habitant coord _ _ _ _) = coord
citoyenCoord (Emigrant coord _ _) = coord

-- Helper pour définir les coordonnées d'un citoyen
setCitoyenCoord :: Citoyen -> Coord -> Citoyen
setCitoyenCoord (Immigrant _ etat occupation cid) newCoord = Immigrant newCoord etat occupation cid
setCitoyenCoord (Habitant _ etat batiments occupation cid) newCoord = Habitant newCoord etat batiments occupation cid
setCitoyenCoord (Emigrant _ occupation cid) newCoord = Emigrant newCoord occupation cid

-- Helper pour obtenir l'ID d'un citoyen
getCitId :: Citoyen -> CitId
getCitId (Immigrant _ _ _ cid) = cid
getCitId (Habitant _ _ _ _ cid) = cid
getCitId (Emigrant _ _ cid) = cid

-- Helper pour obtenir l'ID d'un bâtiment
getBatimentId :: Batiment -> BatId
getBatimentId (Cabane _ _ _ batId _) = batId
getBatimentId (Atelier _ _ _ batId _) = batId
getBatimentId (Epicerie _ _ _ batId _) = batId
getBatimentId (Commissariat _ _ _ batId _) = batId
getBatimentId (Maison _ _ _ batId _) = batId

-- Helper pour obtenir l'occupation d'un citoyen
getOccupation :: Citoyen -> Occupation
getOccupation (Immigrant _ _ occupation _) = occupation
getOccupation (Habitant _ _ _ occupation _) = occupation
getOccupation (Emigrant _ occupation _) = occupation

preSetOccupation :: Citoyen -> Occupation -> Bool
preSetOccupation _ _ = True  -- Aucune précondition spécifique

-- Helper pour définir l'occupation d'un citoyen
setOccupation :: Citoyen -> Occupation -> Citoyen
setOccupation (Immigrant coord etat _ cid) newOccupation = Immigrant coord etat newOccupation cid
setOccupation (Habitant coord etat batiments _ cid) newOccupation = Habitant coord etat batiments newOccupation cid
setOccupation (Emigrant coord _ cid) newOccupation = Emigrant coord newOccupation cid

postSetOccupation :: Citoyen -> Occupation -> Citoyen -> Bool
postSetOccupation _ newOccupation citoyen =
  getOccupation citoyen == newOccupation

-- Helper pour obtenir les coordonnées d'une zone
coordZone :: Ville -> ZonId -> Coord
coordZone ville zid = case M.lookup zid (viZones ville) of
  Just (Route (RectangleP coord _ _)) -> coord
  _ -> error "Zone non trouvée ou non routière"

-- Obtient l'ID de la zone à une coordonnée donnée
getZoneIdAtCoord :: Ville -> Coord -> Maybe ZonId
getZoneIdAtCoord ville coord =
  let zones = M.toList (viZones ville)
      matchingZones = filter (\(_, zone) -> appartient coord (zoneForme zone)) zones
  in case matchingZones of
       ((zid, _):_) -> Just zid
       [] -> Nothing

-- Helper functions for A* algorithm
type Cost = Int
type Path = [ZonId]

preAStar :: Ville -> ZonId -> ZonId -> Bool
preAStar ville start goal =
  isJust (M.lookup start (viZones ville)) && isJust (M.lookup goal (viZones ville))

-- Algorithme A* pour trouver le chemin optimal entre deux zones
aStar :: Ville -> ZonId -> ZonId -> Maybe Path
aStar ville start goal =
    let graph = createGraph ville
        heuristic a b = manhattanDistance (coordZone ville a) (coordZone ville b)
        manhattanDistance (C x1 y1) (C x2 y2) = abs (x1 - x2) + abs (y1 - y2)
    in search graph heuristic start goal

postAStar :: Ville -> ZonId -> ZonId -> Maybe Path -> Bool
postAStar _ _ _ result =
  case result of
    Just path -> not (null path)
    Nothing -> True

-- Recherche dans le graphe avec l'algorithme A*
search :: Graph -> (ZonId -> ZonId -> Cost) -> ZonId -> ZonId -> Maybe Path
search graph heuristic start goal = go Set.empty (PQueue.singleton (start, 0)) (M.singleton start (0, [start]))
  where
    go :: Set.Set ZonId -> PQueue.MinQueue (ZonId, Cost) -> M.Map ZonId (Cost, [ZonId]) -> Maybe Path
    go _ open cameFrom | PQueue.null open = Nothing
    go closed open cameFrom =
      let ((current, _), open') = PQueue.deleteFindMin open
      in if current == goal
         then Just $ reverse $ snd $ cameFrom M.! current
         else
           let neighbors = graph M.! current
               newClosed = Set.insert current closed
               processNeighbor (cFrom, openQueue) neighbor =
                 let newCost = (fst $ M.findWithDefault (maxBound, []) current cFrom) + 1
                     oldCost = fst $ M.findWithDefault (maxBound, []) neighbor cFrom
                 in if Set.member neighbor newClosed || newCost >= oldCost
                    then (cFrom, openQueue)
                    else ( M.insert neighbor (newCost, current : snd (cFrom M.! current)) cFrom
                         , PQueue.insert (neighbor, newCost + heuristic neighbor goal) openQueue
                         )
               (newCameFrom, newOpen) = foldl processNeighbor (cameFrom, open') neighbors
           in go newClosed newOpen newCameFrom


-- Exemple de fonction pour obtenir le niveau de pollution d'une zone
pollution_zone :: Zone -> Int
pollution_zone (ZI _ _) = 10
pollution_zone (ZE _ ) = 50
pollution_zone _        = 0

-- Calcule la distance de Manhattan entre deux coordonnées
manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (C x1 y1) (C x2 y2) = abs (x2 - x1) + abs (y2 - y1)

-- Fonction pour calculer le taux de pollution
taux_pollution :: Ville -> Int
taux_pollution ville = foldl' (+) 0 allPollutions
  where
    zones = M.toList (viZones ville)
    residentialZones = [zone | (_, zone) <- zones, isResidentialZone zone]
    commercialZones = [zone | (_, zone) <- zones, isCommercialZone zone]
    allPollutions = [pollutionIncrement zone | (_, zone) <- zones]
    
    -- Vérifie si une zone est une zone industrielle ou de production d'énergie
    isIndustrialZone (ZI _ _) = True
    isIndustrialZone (ZE _) = True
    isIndustrialZone _ = False

    -- Vérifie si une zone est une zone résidentielle
    isResidentialZone (ZR _ _) = True
    isResidentialZone _ = False

    -- Vérifie si une zone est une zone commerciale
    isCommercialZone (ZC _ _) = True
    isCommercialZone _ = False

    -- Incrémente la pollution en fonction de la proximité des zones résidentielles ou commerciales
    pollutionIncrement zone
      | isIndustrialZone zone && isPowered ville zone = 
          let zoneCoord = zoneCoordonnees zone
              nearbyResidentialOrCommercial = any (\rz -> manhattanDistance zoneCoord (zoneCoordonnees rz) <= 500) (residentialZones ++ commercialZones)
          in if nearbyResidentialOrCommercial then pollution_zone zone else 0
      | otherwise = 0

-- Fonction pour calculer le taux de sécurité
taux_securite :: Ville -> Int
taux_securite ville = foldl' (+) 0 allSafetyIncrements
  where
    zones = M.toList (viZones ville)
    residentialZones = [zone | (_, zone) <- zones, isResidentialZone zone]
    commercialZones = [zone | (_, zone) <- zones, isCommercialZone zone]
    allSafetyIncrements = [safetyIncrement zone | (_, zone) <- zones]
    
    -- Vérifie si une zone est une zone administrative
    isAdminZone (Admin _ _) = True
    isAdminZone _ = False

    -- Vérifie si une zone est une zone résidentielle
    isResidentialZone (ZR _ _) = True
    isResidentialZone _ = False

    -- Vérifie si une zone est une zone commerciale
    isCommercialZone (ZC _ _) = True
    isCommercialZone _ = False

    -- Incrémente la sécurité en fonction de la proximité des zones résidentielles ou commerciales
    safetyIncrement zone
      | isAdminZone zone && isPowered ville zone =
          let zoneCoord = zoneCoordonnees zone
              nearbyResidentialOrCommercial = any (\rz -> manhattanDistance zoneCoord (zoneCoordonnees rz) <= 500) (residentialZones ++ commercialZones)
          in if nearbyResidentialOrCommercial then 1 else 0
      | otherwise = 0

-- Invariant pour créer un immigrant
invariant_creer_imigrant :: Ville -> Bool
invariant_creer_imigrant ville =
  let poweredZones = filter (isPowered ville) (map snd (M.toList (viZones ville)))
      residentialZones = countZones isResidentialZone poweredZones
      commercialZones = countZones isCommercialZone poweredZones
      industrialZones = countZones isIndustrialZone poweredZones
  in residentialZones > 0 && residentialZones <= commercialZones + industrialZones
  where
    countZones predicate zones = length [zone | zone <- zones, predicate zone]
    isResidentialZone (ZR _ _) = True
    isResidentialZone _ = False
    isCommercialZone (ZC _ _) = True
    isCommercialZone _ = False
    isIndustrialZone (ZI _ _) = True
    isIndustrialZone _ = False

-- Helper pour obtenir les coordonnées d'une zone
zoneCoordonnees :: Zone -> Coord
zoneCoordonnees (ZR (RectangleP coord _ _) _) = coord
zoneCoordonnees (ZI (RectangleP coord _ _) _) = coord
zoneCoordonnees (ZE (RectangleP coord _ _)) = coord
zoneCoordonnees (ZC (RectangleP coord _ _) _) = coord
zoneCoordonnees (Route (RectangleP coord _ _)) = coord
zoneCoordonnees (Eau (RectangleP coord _ _)) = coord
zoneCoordonnees (Admin (RectangleP coord _ _) _) = coord

-- Trouve une zone résidentielle avec de l'espace
findResidentialZone :: Ville -> Maybe (ZonId, Batiment)
findResidentialZone ville = 
    let zones = [(zid, b) | (zid, ZR _ batiments) <- M.toList (viZones ville), b <- batiments, hasSpace b]
    in listToMaybe zones
  where
    hasSpace (Maison _ _ _ _ residents) = length residents < 10
    hasSpace (Cabane _ _ _ _ residents) = length residents < 5
    hasSpace _ = False

-- Trouve une zone industrielle avec de l'espace
findIndustrialZone :: Ville -> Maybe (ZonId, Batiment)
findIndustrialZone ville = 
    let zones = [(zid, b) | (zid, ZI _ batiments) <- M.toList (viZones ville), b <- batiments, hasSpace b]
    in listToMaybe zones
  where
    hasSpace (Atelier _ _ _ _ workers) = length workers < maxWorkers
    hasSpace _ = False
    maxWorkers = 10  -- Nombre maximum de travailleurs par bâtiment

-- Trouve une zone commerciale avec de l'espace
findCommercialZone :: Ville -> Maybe (ZonId, Batiment)
findCommercialZone ville = 
    let zones = [(zid, b) | (zid, ZC _ batiments) <- M.toList (viZones ville), b <- batiments, hasSpace b]
    in listToMaybe zones
  where
    hasSpace (Epicerie _ _ _ _ customers) = length customers < maxCustomers
    hasSpace _ = False
    maxCustomers = 5  -- Nombre maximum de clients par bâtiment

preAssignImmigrantToZones :: Ville -> Citoyen -> Bool
preAssignImmigrantToZones _ (Immigrant _ _ _ _) = True
preAssignImmigrantToZones _ _ = False

-- Assigne un immigrant à des zones résidentielles, industrielles et commerciales
assignImmigrantToZones :: Ville -> Citoyen -> Maybe Citoyen
assignImmigrantToZones ville citoyen@(Immigrant coord etat occupation cid) = 
  case (findResidentialZone ville, findIndustrialZone ville, findCommercialZone ville) of
    (Just (residenceZid, residence), Just (workZid, work), Just (shopZid, shop)) ->
      let residenceId = getBatimentId residence
          workId = getBatimentId work
          shopId = getBatimentId shop
      in Just $ Habitant coord etat (residenceId, Just workId, Just shopId) occupation cid
    _ -> Just citoyen

assignImmigrantToZones _ citoyen = Just citoyen

postAssignImmigrantToZones :: Ville -> Citoyen -> Maybe Citoyen -> Bool
postAssignImmigrantToZones ville _ (Just (Habitant coord etat (residenceId, Just workId, Just shopId) occupation cid)) =
  isJust (findBuilding residenceId (viZones ville)) &&
  isJust (findBuilding workId (viZones ville)) &&
  isJust (findBuilding shopId (viZones ville))
postAssignImmigrantToZones _ _ (Just citoyen@(Immigrant _ _ _ _)) = True
postAssignImmigrantToZones _ _ _ = False

preCalculateRent :: Batiment -> Bool
preCalculateRent _ = True  -- Aucune précondition spécifique

-- Calcule le loyer d'un bâtiment
calculateRent :: Batiment -> Int
calculateRent (Cabane _ _ _ _ _) = 10
calculateRent (Maison _ _ _ _ _) = 50
calculateRent _ = 0

postCalculateRent :: Batiment -> Int -> Bool
postCalculateRent _ rent =
  rent >= 0

-- Calcule le seuil de pollution en fonction du nombre de citoyens
calculatePollutionThreshold :: Int -> Int
calculatePollutionThreshold numberOfCitizens = max 100 (numberOfCitizens `div` 1000)

-- Calcule le seuil de sécurité en fonction du nombre de citoyens
calculateSafetyThreshold :: Int -> Int
calculateSafetyThreshold numberOfCitizens
  | numberOfCitizens < 1000 = 1 
  | otherwise =  max 2 (numberOfCitizens `div` 1000)
