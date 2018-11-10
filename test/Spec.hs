import Test.Hspec (hspec, it, shouldBe)

import Utils
import Activation
import qualified Matrix as M
import qualified Data.Vector as V

vector = V.fromList [-2, 3, 6, 1, -8, -9, 3, -5]

testChunk = do
    it "Empty vector" $
        chunksOf 5 (V.empty :: V.Vector Int) `shouldBe` V.empty
    it "Even chunks" $
        chunksOf 2 vector `shouldBe` V.fromList (fmap V.fromList [[-2, 3], [6, 1], [-8, -9], [3, -5]])
    it "Uneven chunks" $
        chunksOf 3 vector `shouldBe` V.fromList (fmap V.fromList [[-2, 3, 6], [1, -8, -9], [3, -5]])

matrix = M.Matrix 2 4 vector

testMatrix = do
    it "Sum rows" $
        M.applyRow sum matrix `shouldBe` V.fromList [8, -19]
    it "Multiply vector with unmatching dimensions" $
        M.multiplyVector matrix vector `shouldBe` V.empty
    it "Multiply vector with matching dimensions" $
        M.multiplyVector matrix (V.fromList [0, -6, 3, 1]) `shouldBe` V.fromList [1, 58]

testActivation = do
    it "ReLu activation forward" $
        activationForward ReLu vector `shouldBe` V.fromList [0, 3, 6, 1, 0, 0, 3, 0]
    it "ReLu activation derivate" $
        activationDerivate ReLu vector `shouldBe` V.fromList [0, 1, 1, 1, 0, 0, 1, 0]
    it "Sign activation forward" $
        activationForward Sign vector `shouldBe` V.fromList [-1, 1, 1, 1, -1, -1, 1, -1]

main :: IO ()
main = hspec $ testChunk >> testMatrix >> testActivation