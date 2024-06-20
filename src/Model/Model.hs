module Model.Model where

import SDL
import qualified Debug.Trace as Debug (trace)
import qualified Data.Map as M
import Model.Ville as V
import Model.Shapes
import Data.IORef
import Data.Maybe (listToMaybe)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Word (Word8)
import System.Random (randomRIO)

-- Global counter for generating unique citizen IDs
{-# NOINLINE citizenCounter #-}
citizenCounter :: IORef Int
citizenCounter = unsafePerformIO (newIORef 0)

data BuildType = BuildZone Zone | BuildBatiment Batiment ZonId deriving Show

data Message = Message
  { msgText :: String
  , msgEndTime :: UTCTime
  }

data GameState = GameState
  { ville :: V.Ville
  , money :: Integer  
  , buildMode :: BuildMode
  , selectedBuild :: Maybe BuildType
  , message :: Maybe Message  
  , pollution :: Int
  , safety :: Int
  , tickCount :: Int
  }

data BuildMode = Build | Move | Destroy deriving Show

-- Initial game state
initGameState :: GameState
initGameState = GameState
  { ville = V.Ville (M.fromList [
                          (ZonId 1, Eau (RectangleP (C 1000 200) 75 75)),
                          (ZonId 2, Eau (RectangleP (C 600 465) 75 75)),
                          (ZonId 3, Eau (RectangleP (C 1400 1800) 75 75)),
                          (ZonId 4, Eau (RectangleP (C 300 700) 75 75)),
                          (ZonId 5, Eau (RectangleP (C 1200 400) 75 75)),
                          (ZonId 6, Eau (RectangleP (C 800 1200) 75 75)),
                          (ZonId 7, Eau (RectangleP (C 500 1600) 75 75)),
                          (ZonId 8, Eau (RectangleP (C 2000 300) 75 75)),
                          (ZonId 9, Eau (RectangleP (C 700 1500) 75 75)),
                          (ZonId 10, Eau (RectangleP (C 1600 900) 75 75))
                        ]) 
                    M.empty
  , money = 10000  
  , buildMode = Build
  , selectedBuild = Nothing
  , message = Nothing
  , pollution = 0
  , safety = 0
  , tickCount = 0
  }

-- Generate unique citizen ID
generateUniqueCitizenId :: IO CitId
generateUniqueCitizenId = do
  currentCount <- readIORef citizenCounter
  let newCount = currentCount + 1
  writeIORef citizenCounter newCount
  return $ CitId ("citizen" ++ show newCount)

-- Function to modify money, e.g., add money
addMoney :: Integer -> GameState -> GameState
addMoney amount game = game { money = money game + amount }

-- Similarly for spending money
spendMoney :: Integer -> GameState -> GameState
spendMoney amount game
  | amount > money game = error "Not enough money"
  | otherwise = game { money = money game - amount }

-- Function to get the cost of a build type
buildCost :: BuildType -> Integer
buildCost (BuildZone (ZR _ _)) = 100
buildCost (BuildZone (ZC _ _)) = 100
buildCost (BuildZone (ZI _ _)) = 100
buildCost (BuildZone (Route _)) = 10
buildCost (BuildZone (Eau _)) = 10
buildCost (BuildZone (ZE _ )) = 500
buildCost (BuildZone (Admin _ _)) = 10
buildCost (BuildZone (Wire _)) = 10
buildCost (BuildBatiment _ _) = 50 
buildCost _ = 0

-- Function to get the number of villagers
getNumberOfVillagers :: GameState -> Int
getNumberOfVillagers gameState = M.size (viCit (ville gameState))

-- Print current money
printMoney :: GameState -> IO ()
printMoney game = putStrLn $ "Current money: " ++ show (money game)

-- Auxiliary function to add citizens to the game state
addCitizensToState :: GameState -> [Citoyen] -> GameState
addCitizensToState gameState newCitizens =
  gameState { ville = (ville gameState) { viCit = M.union (viCit (ville gameState)) (M.fromList $ map (\c -> (V.getCitId c, c)) newCitizens) } }

-- Apply building to the city (unchanged)
applyBuild :: GameState -> BuildType -> Coord -> Int -> Int -> IO (Maybe GameState)
applyBuild gameState buildType coord width height = do
  currentTime <- getCurrentTime
  let cost = buildCost buildType
      newMessage text = Just (Message text (addUTCTime 1 currentTime))
      currentVille = ville gameState
      maybeZonId = getZoneIdAtCoord currentVille coord

  if money gameState < cost
     then return $ Just gameState { message = newMessage "Not enough money" }
     else case buildType of
            BuildZone zone -> 
              let newZone = updateZoneCoord zone coord width height
                  newVille = V.construit currentVille newZone (V.nextZonId currentVille)
              in if V.prop_ville_sansCollision newVille
                 then return $ Just gameState { ville = newVille, money = money gameState - cost, message = newMessage "Zone construite avec succès" }
                 else return $ Just gameState { message = newMessage ("Construction failed: Zone collision or adjacency issues with " ++ show newZone) }

-- Update city with buildings
updateCityWithBuildings :: GameState -> GameState
updateCityWithBuildings gameState =
  let updatedVille = V.addBuildingsToPoweredZones (ville gameState)
      newPollution = V.taux_pollution updatedVille
      newSafety = V.taux_securite updatedVille
  in
    gameState { ville = updatedVille, pollution = newPollution, safety = newSafety }

-- Get pollution rate
getPollutionRate :: GameState -> Int
getPollutionRate gameState = taux_pollution (ville gameState)

-- Get safety rate
getSafetyRate :: GameState -> Int
getSafetyRate gameState = taux_securite (ville gameState)

-- Get the number of immigrants
getNumberOfImmigrants :: GameState -> Int
getNumberOfImmigrants gameState = length [c | c@(Immigrant _ _ _ _) <- M.elems (viCit (ville gameState))]

-- Determine the color of the text based on the pollution level
getPollutionColor :: GameState -> Int -> V4 Word8
getPollutionColor gameState pollution =
  let numberOfCitizens = getNumberOfVillagers gameState
      pollutionThreshold = calculatePollutionThreshold numberOfCitizens
  in if pollution < pollutionThreshold
     then V4 0 255 0 255   
     else if pollution < 2 * pollutionThreshold
          then V4 255 255 0 255  
          else V4 255 0 0 255    

-- Determine the color of the text based on the safety level
getSafetyColor :: GameState -> Int -> V4 Word8
getSafetyColor gameState safety =
  let numberOfCitizens = getNumberOfVillagers gameState
      safetyThreshold = calculateSafetyThreshold numberOfCitizens
  in if safety < safetyThreshold
     then V4 255 0 0 255     
     else if safety < 2 * safetyThreshold
          then V4 255 255 0 255   
          else V4 0 255 0 255    

-- Check if touch event is within a specified area
checkTouch :: Maybe (Int, Int) -> GameState -> Bool
checkTouch Nothing _ = False
checkTouch (Just (mouseX, mouseY)) _ =
  mouseX >= 1850 && mouseX <= 1850 + 100 && mouseY >= 140 && mouseY <= 140 + 100

-- Generate immigrants based on certain criteria
generateImmigrantsIfCriteriaMet :: GameState -> IO GameState
generateImmigrantsIfCriteriaMet gameState = do
  let numberOfCitizens = getNumberOfVillagers gameState
  let pollutionThreshold = calculatePollutionThreshold numberOfCitizens
  let safetyThreshold = calculateSafetyThreshold numberOfCitizens
  let pollutionRate = getPollutionRate gameState
  let safetyRate = getSafetyRate gameState
  
  randomValue <- randomRIO (1 :: Int, 100)
  
  if pollutionRate < pollutionThreshold && safetyRate >= safetyThreshold && invariant_creer_imigrant (ville gameState) && randomValue == 1 && getNumberOfImmigrants gameState < 100
    then do
      let coord = C 0 0
      randomValueIm <- randomRIO (1 :: Int, 20)
      immigrantIds <- generateImmigrantsForZone coord randomValueIm
      let immigrants = map (\cid -> createImmigrant coord cid) immigrantIds
      let updatedGameState = addCitizensToState gameState immigrants
      return updatedGameState
    else return gameState

-- Generate IDs for immigrants
generateImmigrantsForZone :: Coord -> Int -> IO [CitId]
generateImmigrantsForZone coord capacity = do
  mapM (\_ -> generateUniqueCitizenId) [1..capacity]

-- Update immigrants in the game state
updateImmigrants :: GameState -> GameState
updateImmigrants gameState = 
  let updatedVille = (ville gameState) { viCit = M.mapMaybe (assignIfImmigrant (ville gameState)) (viCit (ville gameState)) }
  in gameState { ville = updatedVille }
  where
    assignIfImmigrant :: Ville -> Citoyen -> Maybe Citoyen
    assignIfImmigrant ville citoyen@(Immigrant _ _ _ _) = assignImmigrantToZones ville citoyen
    assignIfImmigrant _ citoyen = Just citoyen

-- Apply rent to all inhabitants
applyRent :: GameState -> GameState
applyRent gameState =
  let (updatedVille, totalRent) = collectRent (ville gameState)
  in gameState { ville = updatedVille, money = money gameState + totalRent }
  where
    collectRent :: Ville -> (Ville, Integer)
    collectRent ville = 
      let (totalRent, updatedCitizens) = M.mapAccumWithKey applyRentToCitizen 0 (viCit ville)
      in (ville { viCit = updatedCitizens }, totalRent)

    applyRentToCitizen :: Integer -> CitId -> Citoyen -> (Integer, Citoyen)
    applyRentToCitizen totalRent cid citoyen@(Habitant coord (argent, fatigue, faim) (residenceId, workId, shopId) occupation cid') =
      case findResidence residenceId (viZones (ville gameState)) of
        Just residence ->
          let loyer = calculateRent residence
              newArgent = argent - loyer
              (newTotalRent, updatedCitizen) = 
                if newArgent < 0 
                then (totalRent + fromIntegral argent, Emigrant coord occupation cid')
                else (totalRent + fromIntegral loyer, Habitant coord (newArgent, fatigue, faim) (residenceId, workId, shopId) occupation cid')
          in (newTotalRent, updatedCitizen)
        Nothing -> (totalRent, citoyen)
    applyRentToCitizen totalRent _ citoyen = (totalRent, citoyen)

    findResidence :: BatId -> M.Map ZonId Zone -> Maybe Batiment
    findResidence bid zones = listToMaybe [b | (_, zone) <- M.toList zones, b <- getBatiments zone, getBatimentId b == bid]

    getBatiments :: Zone -> [Batiment]
    getBatiments (ZR _ batiments) = batiments
    getBatiments (ZI _ batiments) = batiments
    getBatiments (ZC _ batiments) = batiments
    getBatiments _ = []

-- Remove emigrants from the list of citizens
removeEmigrants :: GameState -> GameState
removeEmigrants gameState =
  let updatedCitizens = M.filter (not . isEmigrant) (viCit (ville gameState))
  in gameState { ville = (ville gameState) { viCit = updatedCitizens } }
  where
    isEmigrant :: Citoyen -> Bool
    isEmigrant (Emigrant _ _ _) = True
    isEmigrant _ = False

-- Update citizens in the game state every X ticks
updateCitizensInGameState :: GameState -> GameState
updateCitizensInGameState gameState =
  let tickInterval = 100
      shouldUpdateCitizens = tickCount gameState `mod` tickInterval == 0
      ville' = ville gameState
      updatedCitizens = if shouldUpdateCitizens
                        then M.map (mettreAJourPosition ville') (viCit ville')
                        else viCit ville'
      ville'' = ville' { viCit = updatedCitizens }
  in gameState { ville = ville'' }

-- Update the city
updateCity :: GameState -> IO GameState
updateCity gameState = do
  let gameStateWithBuildings = updateCityWithBuildings gameState
  gameStateWithImmigrants <- generateImmigrantsIfCriteriaMet gameStateWithBuildings
  let gameStateWithUpdatedImmigrants = updateImmigrants gameStateWithImmigrants
  
  -- Update citizens every X ticks
  let gameStateWithUpdatedPositions = updateCitizensInGameState gameStateWithUpdatedImmigrants

  -- Apply rent every X ticks
  let tickInterval = 100  
  let shouldApplyRent = tickCount gameStateWithUpdatedPositions `mod` tickInterval == 0
  let gameStateWithRentApplied = if shouldApplyRent
                                 then applyRent gameStateWithUpdatedPositions
                                 else gameStateWithUpdatedPositions
  let finalGameState = removeEmigrants gameStateWithRentApplied

  -- Increment tick count
  let incrementedTickCountGameState = finalGameState { tickCount = tickCount finalGameState + 1 }

  return incrementedTickCountGameState
