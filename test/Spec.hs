import Test.Hspec (describe, hspec, it, shouldBe)

import qualified Utils
import qualified Activation as A
import qualified Matrix as M
import qualified Data.Vector as V

vector = V.fromList [-2, 3, 6, 1, -8, -9, 3, -5]

testUtils =
    describe "Test of utility functions:" $ do
        it "Chunks an empty vector" $
            chunksOf 5 (V.empty :: V.Vector Int) `shouldBe` V.empty
        it "Even chunks a vector" $
            chunksOf 2 vector `shouldBe` V.fromList (fmap V.fromList [[-2, 3], [6, 1], [-8, -9], [3, -5]])
        it "Uneven chunks a vector" $
            chunksOf 3 vector `shouldBe` V.fromList (fmap V.fromList [[-2, 3, 6], [1, -8, -9], [3, -5]])

matrix = M.Matrix 2 4 vector

testMatrix =
    describe "Test of matrix functions:" $ do
        it "Sums rows" $
            M.applyRow sum matrix `shouldBe` V.fromList [8, -19]
        it "Multiplies vector with unmatching dimensions" $
            M.multiplyVector matrix vector `shouldBe` V.empty
        it "Multiplies vector with matching dimensions" $
            M.multiplyVector matrix (V.fromList [0, -6, 3, 1]) `shouldBe` V.fromList [1, 58]

testActivation =
    describe "Test of activation functions:" $ do
        it "Forwards a ReLu activation" $
            A.activationForward A.ReLu vector `shouldBe` V.fromList [0, 3, 6, 1, 0, 0, 3, 0]
        it "Derivates a ReLu activation" $
            A.activationDerivate A.ReLu vector `shouldBe` V.fromList [0, 1, 1, 1, 0, 0, 1, 0]
        it "Forwards a Sign activation" $
            A.activationForward A.Sign vector `shouldBe` V.fromList [-1, 1, 1, 1, -1, -1, 1, -1]

main :: IO ()
main =
    hspec $ testUtils >> testMatrix >> testActivation
