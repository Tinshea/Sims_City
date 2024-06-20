module ShapesSpec where

import Model.Shapes
import Test.Hspec

makeZoneForme :: Spec
makeZoneForme = do
    describe "zoneForme" $ do
      it "returns the correct Forme for each Zone" $ do
        let zone1 = Eau (RectangleP (C 0 0) 10 20)
        zoneForme zone1 `shouldBe` RectangleP (C 0 0) 10 20
        let zone2 = Route (RectangleP (C 0 0) 10 20)
        zoneForme zone2 `shouldBe` RectangleP (C 0 0) 10 20
        let zone3 = ZR (RectangleP (C 0 0) 10 20) []
        zoneForme zone3 `shouldBe` RectangleP (C 0 0) 10 20
        let zone4 = ZI (RectangleP (C 0 0) 10 20) []
        zoneForme zone4 `shouldBe` RectangleP (C 0 0) 10 20
        let zone5 = ZC (RectangleP (C 0 0) 10 20) []
        zoneForme zone5 `shouldBe` RectangleP (C 0 0) 10 20
        let zone6 = Admin (RectangleP (C 0 0) 10 20) []
        zoneForme zone6 `shouldBe` RectangleP (C 0 0) 10 20

makeLimites :: Spec
makeLimites = do
    describe "limites" $ do
      it "returns the correct limits for a Forme" $ do
        let forme1 = HSegment (C 0 0) 10
        limites forme1 `shouldBe` (0, 0, 0, 9)
        let forme2 = VSegment (C 0 0) 10
        limites forme2 `shouldBe` (0, -11, 0, 0)
        let forme3 = RectangleP (C 0 0) 10 20
        limites forme3 `shouldBe` (0, 19, 0, 9)

makeAppartient :: Spec
makeAppartient = do
    describe "appartient" $ do
      it "returns True if a Coord is in a Forme" $ do
        let coord = C 0 0
            forme = RectangleP (C 0 0) 10 10
        appartient coord forme `shouldBe` True
      it "returns False if a Coord is not in a Forme" $ do
        let coord = C 11 11
            forme = RectangleP (C 0 0) 10 10
        appartient coord forme `shouldBe` False

makeAdjacent :: Spec
makeAdjacent = do
    describe "adjacent" $ do
      it "returns True if a Coord is adjacent to a Forme" $ do
        let coord = C 0 1
            forme = RectangleP (C 0 0) 10 10
        adjacent coord forme `shouldBe` True
      it "returns False if a Coord is not adjacent to a Forme" $ do
        let coord = C 0 2
            forme = RectangleP (C 0 0) 10 10
        adjacent coord forme `shouldBe` False

makeFormesAdjacentes :: Spec
makeFormesAdjacentes = do
    describe "formesAdjacentes" $ do
      it "returns True if two rectangles are adjacent" $ do
        let forme1 = RectangleP (C 0 0) 10 10
            forme2 = RectangleP (C 10 0) 10 10
        formesAdjacentes forme1 forme2 `shouldBe` True

      it "returns False if two rectangles are not adjacent" $ do
        let forme1 = RectangleP (C 0 0) 5 5
            forme2 = RectangleP (C 36 0) 5 5  -- 36 is outside the tolerance of 25
        formesAdjacentes forme1 forme2 `shouldBe` False

      it "returns True if two rectangles are within tolerance" $ do
        let forme1 = RectangleP (C 0 0) 10 10
            forme2 = RectangleP (C 20 0) 10 10  -- Adjacent within tolerance of 25
        formesAdjacentes forme1 forme2 `shouldBe` True

      it "returns False if two rectangles are outside of tolerance" $ do
        let forme1 = RectangleP (C 0 0) 10 10
            forme2 = RectangleP (C 0 36) 10 10  -- Outside tolerance of 25
        formesAdjacentes forme1 forme2 `shouldBe` False


makeZonesAdjacentes :: Spec
makeZonesAdjacentes = do
    describe "zonesAdjacentes" $ do
      it "returns True if two zones with adjacent forms are given" $ do
        let zone1 = Route (RectangleP (C 0 0) 10 10)
            zone2 = Route (RectangleP (C 10 0) 10 10)
        zonesAdjacentes zone1 zone2 `shouldBe` True

      it "returns False if two zones with non-adjacent forms are given" $ do
        let zone1 = Route (RectangleP (C 0 0) 10 10)
            zone2 = Route (RectangleP (C 36 0) 10 10)  -- 36 is outside the tolerance of 25
        zonesAdjacentes zone1 zone2 `shouldBe` False

      it "returns True if two zones with exactly adjacent forms are given" $ do
        let zone1 = Route (RectangleP (C 0 0) 10 1)
            zone2 = Route (RectangleP (C 20 0) 10 1)  -- Within tolerance of 25
        zonesAdjacentes zone1 zone2 `shouldBe` True

      it "returns False if two zones with non-adjacent forms outside tolerance are given" $ do
        let zone1 = Route (RectangleP (C 0 0) 10 1)
            zone2 = Route (RectangleP (C 0 36) 10 1)  -- Outside tolerance of 25
        zonesAdjacentes zone1 zone2 `shouldBe` False

makeCollision :: Spec
makeCollision = do
    describe "collision" $ do
      it "returns True if two rectangles collide" $ do
        let forme1 = RectangleP (C 0 0) 10 10
            forme2 = RectangleP (C 5 5) 10 10
        collision forme1 forme2 `shouldBe` True
      it "returns False if two rectangles do not collide" $ do
        let forme1 = RectangleP (C 0 0) 10 10
            forme2 = RectangleP (C 10 0) 10 10
        collision forme1 forme2 `shouldBe` False

makeUpdateZoneCoord :: Spec
makeUpdateZoneCoord = do
    describe "updateZoneCoord" $ do
      it "updates the coordinates and dimensions of a residential zone (ZR)" $ do
        let originalZone = ZR (RectangleP (C 0 0) 10 10) []
            newCoord = C 20 20
            newWidth = 15
            newHeight = 25
            updatedZone = updateZoneCoord originalZone newCoord newWidth newHeight
        updatedZone `shouldBe` ZR (RectangleP (C 20 20) 15 25) []

      it "updates the coordinates and dimensions of an industrial zone (ZI)" $ do
        let originalZone = ZI (RectangleP (C 0 0) 10 10) []
            newCoord = C 30 30
            newWidth = 20
            newHeight = 20
            updatedZone = updateZoneCoord originalZone newCoord newWidth newHeight
        updatedZone `shouldBe` ZI (RectangleP (C 30 30) 20 20) []

      it "updates the coordinates and dimensions of a commercial zone (ZC)" $ do
        let originalZone = ZC (RectangleP (C 0 0) 10 10) []
            newCoord = C 40 40
            newWidth = 30
            newHeight = 15
            updatedZone = updateZoneCoord originalZone newCoord newWidth newHeight
        updatedZone `shouldBe` ZC (RectangleP (C 40 40) 30 15) []

      it "updates the coordinates and dimensions of a route (Route)" $ do
        let originalZone = Route (RectangleP (C 0 0) 10 10)
            newCoord = C 50 50
            newWidth = 10
            newHeight = 10
            updatedZone = updateZoneCoord originalZone newCoord newWidth newHeight
        updatedZone `shouldBe` Route (RectangleP (C 50 50) 10 10)

      it "updates the coordinates and dimensions of a water zone (Eau)" $ do
        let originalZone = Eau (RectangleP (C 0 0) 10 10)
            newCoord = C 60 60
            newWidth = 15
            newHeight = 15
            updatedZone = updateZoneCoord originalZone newCoord newWidth newHeight
        updatedZone `shouldBe` Eau (RectangleP (C 60 60) 15 15)

      it "updates the coordinates and dimensions of an administrative zone (Admin)" $ do
        let originalZone = Admin (RectangleP (C 0 0) 10 10) []
            newCoord = C 70 70
            newWidth = 20
            newHeight = 25
            updatedZone = updateZoneCoord originalZone newCoord newWidth newHeight
        updatedZone `shouldBe` Admin (RectangleP (C 70 70) 20 25) []

      it "updates the coordinates and dimensions of an energy production zone (ZE)" $ do
        let originalZone = ZE (RectangleP (C 0 0) 10 10)
            newCoord = C 80 80
            newWidth = 20
            newHeight = 20
            updatedZone = updateZoneCoord originalZone newCoord newWidth newHeight
        updatedZone `shouldBe` ZE (RectangleP (C 80 80) 20 20)

      it "updates the coordinates and dimensions of a wire zone (Wire)" $ do
        let originalZone = Wire (RectangleP (C 0 0) 10 10)
            newCoord = C 90 90
            newWidth = 25
            newHeight = 25
            updatedZone = updateZoneCoord originalZone newCoord newWidth newHeight
        updatedZone `shouldBe` Wire (RectangleP (C 90 90) 25 25)

        
shapesSpec :: Spec
shapesSpec = do 
  describe "Shapes Tests" $ do
    makeZoneForme
    makeLimites
    makeAppartient
    makeAdjacent
    makeFormesAdjacentes
    makeZonesAdjacentes
    makeCollision
    makeUpdateZoneCoord
