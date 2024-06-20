module VilleSpec where

import Model.Ville
import Test.Hspec
import Test.QuickCheck
import Model.Shapes
import qualified Data.Map as Map
import qualified Data.Set as Set
import ShapesSpec (makeAdjacent)

-- Instances Arbitrary pour les types nécessaires
instance Arbitrary Coord where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ C x y

instance Arbitrary Forme where
  arbitrary = do
    coord <- arbitrary
    width <- arbitrary
    height <- arbitrary
    return $ RectangleP coord width height

instance Arbitrary Zone where
  arbitrary = oneof [ Route <$> arbitrary
                    , Eau <$> arbitrary
                    , ZE <$> arbitrary
                    , ZR <$> arbitrary <*> arbitrary
                    , ZC <$> arbitrary <*> arbitrary
                    , ZI <$> arbitrary <*> arbitrary
                    , Admin <$> arbitrary <*> arbitrary
                    ]

instance Arbitrary BatId where
  arbitrary = BatId <$> arbitrary

instance Arbitrary ZonId where
  arbitrary = ZonId <$> arbitrary

instance Arbitrary CitId where
  arbitrary = CitId <$> arbitrary

instance Arbitrary Batiment where
  arbitrary = oneof [ Cabane <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    , Maison <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    , Atelier <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    , Epicerie <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    , Commissariat <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    ]

instance Arbitrary Occupation where
  arbitrary = oneof [ pure Travailler
                    , pure Dormir
                    , pure Shopping
                    , Move <$> arbitrary
                    ]

instance Arbitrary Citoyen where
  arbitrary = oneof [ Immigrant <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    , Habitant <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    , Emigrant <$> arbitrary <*> arbitrary <*> arbitrary
                    ]

instance Arbitrary Ville where
  arbitrary = do
    zones <- arbitrary
    citoyens <- arbitrary
    return $ Ville zones citoyens
makeExtraireRoutes :: Spec
makeExtraireRoutes = do
    describe "extraireRoutes" $ do
      it "returns the correct routes" $ do
        let ville = Ville (Map.fromList [(ZonId 1, Route (RectangleP (C 0 0) 10 20)), (ZonId 2, Eau (RectangleP (C 0 0) 10 20)), (ZonId 4, Route (RectangleP (C 0 0) 10 20))]) Map.empty
        extraireRoutes ville `shouldBe` [ZonId 1, ZonId 4]

makeContientRoute :: Spec
makeContientRoute = do
    describe "contientRoute" $ do
      it "returns True if a Ville contains a Route" $ do
        let zones = [Route (RectangleP (C 0 0) 10 20), Eau (RectangleP (C 0 0) 10 20)]
        contientRoute zones `shouldBe` True
      it "returns False if a Ville does not contain a Route" $ do
        let zones = [Eau (RectangleP (C 0 0) 10 20), ZR (RectangleP (C 0 0) 10 20) []]
        contientRoute zones `shouldBe` False

makeBesideRoad :: Spec
makeBesideRoad = do
    describe "besideRoad" $ do
      it "returns True if a Zone is beside a Route" $ do
        let zone = Route (RectangleP (C 0 0) 10 20)
            zonesList = [Route (RectangleP (C 0 0) 10 20), Eau (RectangleP (C 0 0) 10 20)]
        besideRoad zone zonesList `shouldBe` True
      it "returns False if a Zone is not beside a Route" $ do
        let zone = ZR (RectangleP (C 0 0) 10 20) []
            zonesList = [Eau (RectangleP (C 0 0) 10 20), ZR (RectangleP (C 0 0) 10 20) []]
        besideRoad zone zonesList `shouldBe` False

makeGetAdjacent :: Spec
makeGetAdjacent = do
    describe "getAdjacent" $ do
      it "returns the correct adjacent Zones" $ do
        let ville = Ville (Map.fromList [(ZonId 1, Route (RectangleP (C 0 0) 10 20)), (ZonId 2, Eau (RectangleP (C 10 0) 10 20)), (ZonId 4, Route (RectangleP (C 0 0) 10 20))]) Map.empty
            zone = Route (RectangleP (C 0 0) 10 10)
        getAdjacent ville zone `shouldBe` [Eau (RectangleP (C 10 0) 10 20)]
      it "returns an empty list if there are no adjacent Zones" $ do
        let ville = Ville (Map.fromList [(ZonId 1, Route (RectangleP (C 0 0) 10 20)), (ZonId 2, Eau (RectangleP (C 0 0) 10 20)), (ZonId 4, Route (RectangleP (C 0 0) 10 20))]) Map.empty
            zone = Route (RectangleP (C 0 0) 10 10)
        getAdjacent ville zone `shouldBe` []

makePropRoutesConnexes :: Spec
makePropRoutesConnexes = do
    describe "propRoutesConnexes" $ do
      it "checks if streets are connexes" $ do
        let ville = Ville (Map.fromList [(ZonId 1, Route (RectangleP (C 0 0) 10 1)), 
                                         (ZonId 2, Route (RectangleP (C 10 0) 10 1)), 
                                         (ZonId 3, Route (RectangleP (C 20 0) 1 10)), 
                                         (ZonId 4, Route (RectangleP (C 20 10) 1 10))]) Map.empty
        prop_routes_connexes ville `shouldBe` True
      it "checks if streets are not connexes" $ do
        let ville = Ville (Map.fromList [(ZonId 1, Route (RectangleP (C 0 0) 10 1)), 
                                         (ZonId 2, Route (RectangleP (C 36 0) 10 1))]) Map.empty
        prop_routes_connexes ville `shouldBe` False

makegetAdjacentRouteIds :: Spec
makegetAdjacentRouteIds = do
    describe "getAdjacentRouteIds with ZonId" $ do
      it "returns the correct adjacent ZonIds" $ do
        let ville = Ville (Map.fromList [(ZonId 1, Route (RectangleP (C 0 0) 10 1)), (ZonId 2, Route (RectangleP (C 10 0) 10 1)), (ZonId 3, Route (RectangleP (C 10 10) 10 1)), (ZonId 4, Route (RectangleP (C 20 10) 10 1))]) Map.empty
        getAdjacentRouteIds ville (ZonId 1) `shouldBe` [ZonId 2]

makeGraph :: Spec
makeGraph = do
    describe "createGraph" $ do
      it "checks if the constructed graph is correct" $ do
        let ville = Ville (Map.fromList [(ZonId 1, Route (RectangleP (C 0 0) 10 1)), (ZonId 2, Route (RectangleP (C 10 0) 1 10)), (ZonId 3, Route (RectangleP (C 10 10) 10 1)), (ZonId 4, Route (RectangleP (C 20 10) 10 1))]) Map.empty
            graph = Map.fromList [(ZonId 1, [ZonId 2]), (ZonId 2, [ZonId 1, ZonId 3]), (ZonId 3, [ZonId 2, ZonId 4]), (ZonId 4, [ZonId 3])]
        createGraph ville `shouldBe` graph

makeIsWithin :: Spec
makeIsWithin = do
  describe "isWithin" $ do
    it "returns True if a building form is within the boundary of a zone" $ do
      let buildingForm = RectangleP (C 0 0) 5 5
          zoneForm = RectangleP (C 0 0) 10 10
      isWithin buildingForm zoneForm `shouldBe` True
    it "returns True if a building form is within the boundary of a zone 2" $ do
      let buildingForm = RectangleP (C 3 3) 5 5
          zoneForm = RectangleP (C 0 0) 10 10
      isWithin buildingForm zoneForm `shouldBe` True
    it "returns False if a building form is not within the boundary of a zone" $ do
      let buildingForm = RectangleP (C 0 0) 15 15
          zoneForm = RectangleP (C 0 0) 10 10
      isWithin buildingForm zoneForm `shouldBe` False

makeIsOverlapping :: Spec
makeIsOverlapping = do
  describe "isOverlapping" $ do
    it "returns True if two forms are overlapping" $ do
      let f1 = RectangleP (C 0 0) 10 10
          f2 = RectangleP (C 5 5) 10 10
      isOverlapping f1 f2 `shouldBe` True
    it "returns False if two forms are not overlapping" $ do
      let f1 = RectangleP (C 0 0) 10 10
          f2 = RectangleP (C 15 15) 10 10
      isOverlapping f1 f2 `shouldBe` False

makeIsPowered :: Spec
makeIsPowered = do
  describe "isPowered" $ do
    it "returns True if a zone is powered by an adjacent Wire zone" $ do
      let ville = Ville (Map.fromList [(ZonId 1, Wire (RectangleP (C 0 0) 10 10)), 
                                       (ZonId 2, ZR (RectangleP (C 10 0) 10 10) [])]) Map.empty
      isPowered ville (ZR (RectangleP (C 10 0) 10 10) []) `shouldBe` True
    it "returns False if a zone is not powered" $ do
      let ville = Ville (Map.fromList [(ZonId 1, Wire (RectangleP (C 0 0) 10 10)), 
                                       (ZonId 2, ZR (RectangleP (C 50 0) 10 10) [])]) Map.empty
      isPowered ville (ZR (RectangleP (C 50 0) 10 10) []) `shouldBe` False

makeCanAddBuilding :: Spec
makeCanAddBuilding = do
  describe "canAddBuilding" $ do
    it "returns True if a building can be added within the zone boundaries and does not overlap" $ do
      let building = Cabane (RectangleP (C 1 1) 2 2) (C 1 1) (ZonId 1) (BatId 1) []
          zone = ZR (RectangleP (C 0 0) 10 10) []
      canAddBuilding building zone `shouldBe` True
    it "returns False if a building overlaps with another building in the zone" $ do
      let existingBuilding = Cabane (RectangleP (C 1 1) 3 3) (C 1 1) (ZonId 1) (BatId 1) []
          newBuilding = Cabane (RectangleP (C 2 2) 2 2) (C 2 2) (ZonId 1) (BatId 2) []
          zone = ZR (RectangleP (C 0 0) 10 10) [existingBuilding]
      canAddBuilding newBuilding zone `shouldBe` False
    it "returns False if a building is outside the zone boundaries" $ do
      let building = Cabane (RectangleP (C 9 9) 2 2) (C 9 9) (ZonId 1) (BatId 1) []
          zone = ZR (RectangleP (C 0 0) 10 10) []
      canAddBuilding building zone `shouldBe` False

makeUpdateCitizens :: Spec
makeUpdateCitizens = do
  describe "updateCitizens" $ do
    it "updates the position of citizens based on their occupation" $ do
      let initialVille = Ville (Map.fromList [(ZonId 1, ZR (RectangleP (C 0 0) 100 100) [Maison (RectangleP (C 10 10) 30 30) (C 10 10) (ZonId 1) (BatId 1) []])]) 
                               (Map.fromList [(CitId "cit1", Habitant (C 0 0) (100, 100, 100) (BatId 1, Nothing, Nothing) (Move (C 5 5)) (CitId "cit1"))])
          expectedVille = Ville (Map.fromList [(ZonId 1, ZR (RectangleP (C 0 0) 100 100) [Maison (RectangleP (C 10 10) 30 30) (C 10 10) (ZonId 1) (BatId 1) []])])
                                (Map.fromList [(CitId "cit1", Habitant (C 5 5) (100, 99, 99) (BatId 1, Nothing, Nothing) (Travailler) (CitId "cit1"))])
      updateCitizens initialVille `shouldBe` expectedVille

makePollutionZone :: Spec
makePollutionZone = do
  describe "pollution_zone" $ do
    it "returns the correct pollution level for industrial zones" $ do
      let zone = ZI (RectangleP (C 0 0) 10 10) []
      pollution_zone zone `shouldBe` 10
    it "returns the correct pollution level for energy production zones" $ do
      let zone = ZE (RectangleP (C 0 0) 10 10)
      pollution_zone zone `shouldBe` 50
    it "returns 0 pollution for non-industrial or non-energy zones" $ do
      let zone = ZR (RectangleP (C 0 0) 10 10) []
      pollution_zone zone `shouldBe` 0

makeCoordOfShop :: Spec
makeCoordOfShop = do
  describe "coordOfShop" $ do
    it "returns the correct coordinates of a shop for a resident" $ do
      let shop = Epicerie (RectangleP (C 10 10) 10 10) (C 10 10) (ZonId 1) (BatId 1) []
          ville = Ville (Map.fromList [(ZonId 1, ZC (RectangleP (C 10 10) 10 10) [shop])]) Map.empty
          resident = Habitant (C 0 0) (100, 100, 100) (BatId 1, Nothing, Just (BatId 1)) Shopping (CitId "cit1")
      coordOfShop ville resident `shouldBe` (C 10 10)

makeReachableNodes :: Spec
makeReachableNodes = do
  describe "reachableNodes" $ do
    it "returns the set of reachable nodes from a given node in a graph" $ do
      let graph = Map.fromList [(ZonId 1, [ZonId 2]), (ZonId 2, [ZonId 3]), (ZonId 3, [ZonId 4]), (ZonId 4, [ZonId 1])]
      reachableNodes (ZonId 1) graph Set.empty `shouldBe` Set.fromList [ZonId 1, ZonId 2, ZonId 3, ZonId 4]
    it "returns an empty set if the graph is empty" $ do
      let graph = Map.empty
      reachableNodes (ZonId 1) graph Set.empty `shouldBe` Set.fromList [ZonId 1]

makeAStar :: Spec
makeAStar = do
  describe "aStar" $ do
    it "finds the shortest path between two connected nodes" $ do
      let ville = Ville (Map.fromList [(ZonId 1, Route (RectangleP (C 0 0) 10 1)), 
                                       (ZonId 2, Route (RectangleP (C 10 0) 1 10)), 
                                       (ZonId 3, Route (RectangleP (C 10 10) 10 1)), 
                                       (ZonId 4, Route (RectangleP (C 20 10) 10 1))]) Map.empty
      aStar ville (ZonId 1) (ZonId 4) `shouldBe` Just [ZonId 1, ZonId 1, ZonId 2, ZonId 3]
    it "returns Nothing if there is no path between two nodes" $ do
      let ville = Ville (Map.fromList [(ZonId 1, Route (RectangleP (C 0 0) 10 1)), 
                                       (ZonId 2, Route (RectangleP (C 36 0) 1 10))]) Map.empty
      aStar ville (ZonId 1) (ZonId 2) `shouldBe` Nothing

makeInvariantCreerImigrant :: Spec
makeInvariantCreerImigrant = do
  describe "invariant_creer_imigrant" $ do
    it "returns True if the number of residential zones is greater than 0 and less than or equal to the sum of commercial and industrial zones" $ do
      let ville = Ville (Map.fromList [(ZonId 1, ZR (RectangleP (C 0 0) 10 10) []), 
                                       (ZonId 2, ZC (RectangleP (C 10 0) 10 10) []), 
                                       (ZonId 3, ZI (RectangleP (C 20 0) 10 10) [])]) Map.empty
      invariant_creer_imigrant ville `shouldBe` True
    it "returns False if there are no residential zones" $ do
      let ville = Ville (Map.fromList [(ZonId 1, ZC (RectangleP (C 0 0) 10 10) []), 
                                       (ZonId 2, ZI (RectangleP (C 10 0) 10 10) [])]) Map.empty
      invariant_creer_imigrant ville `shouldBe` False
    it "returns False if the number of residential zones is greater than the sum of commercial and industrial zones" $ do
      let ville = Ville (Map.fromList [(ZonId 1, ZR (RectangleP (C 0 0) 10 10) []), 
                                       (ZonId 2, ZR (RectangleP (C 10 0) 10 10) []), 
                                       (ZonId 3, ZC (RectangleP (C 20 0) 10 10) [])]) Map.empty
      invariant_creer_imigrant ville `shouldBe` False

-- QuickCheck properties
prop_isPowered :: Ville -> Zone -> Bool
prop_isPowered ville zone = isPowered ville zone == any isWire (getAdjacent ville zone)

prop_updateCitizens :: Ville -> Bool
prop_updateCitizens ville = all invariantCitoyen (Map.elems (viCit (updateCitizens ville)))

prop_invariant_creer_imigrant :: Ville -> Bool
prop_invariant_creer_imigrant = invariant_creer_imigrant

villeSpec :: Spec
villeSpec = do 
    describe "Ville Tests" $ do
        makeExtraireRoutes
        makeContientRoute
        makeBesideRoad
        makeGetAdjacent
        makePropRoutesConnexes
        makegetAdjacentRouteIds
        makeGraph
        makeIsWithin
        makeIsOverlapping
        makeIsPowered
        makeCanAddBuilding
        makeUpdateCitizens
        makePollutionZone
        makeCoordOfShop
        makeReachableNodes
        makeAStar
        makeInvariantCreerImigrant

    describe "QuickCheck Properties" $ do
        it "isPowered property" $ property prop_isPowered
        it "updateCitizens property" $ property prop_updateCitizens
        it "invariant_creer_imigrant property" $ property prop_invariant_creer_imigrant
