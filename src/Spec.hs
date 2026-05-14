module Spec where
import PdePreludat
import Library
import DatosEstadoTesting
import Test.Hspec


correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "esMayorDeEdad da True cuando la persona es mayor a 18" $ do
      esMayorDeEdad ("Pepita", 19, 100) `shouldBe` True
    it "esMayorDeEdad da False cuando la persona es menor a 18" $ do
      esMayorDeEdad ("Pepita", 17, 100) `shouldBe` False
    it "la reputacion de un jovit es la longitud de su nombre multiplicado por la estatura" $ do
      reputacion bilbo `shouldBe` 625