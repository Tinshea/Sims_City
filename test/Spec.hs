import Test.Hspec
import ShapesSpec as ShapesS
import VilleSpec as VilleS

main :: IO ()
main = hspec $ do
  ShapesS.shapesSpec
  VilleS.villeSpec
