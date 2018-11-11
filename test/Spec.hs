import Test.Hspec (describe, hspec, it, shouldBe)

import qualified Activation as A
import qualified Data.Vector as V (empty, fromList, Vector(..))
import qualified Matrix as M
import qualified Network as N
import qualified System.Random as S (mkStdGen)
import qualified Utils as U

vector = V.fromList [-2, 3, 6, 1, -8, -9, 3, -5]

testUtils =
    describe "Test of utility functions:" $ do
        it "Chunks an empty vector" $
            U.chunksOf 5 (V.empty :: V.Vector Int) `shouldBe` V.empty
        it "Even chunks a vector" $
            U.chunksOf 2 vector `shouldBe` V.fromList (fmap V.fromList [[-2, 3], [6, 1], [-8, -9], [3, -5]])
        it "Uneven chunks a vector" $
            U.chunksOf 3 vector `shouldBe` V.fromList (fmap V.fromList [[-2, 3, 6], [1, -8, -9], [3, -5]])

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

generator = S.mkStdGen 123456

testNetwork =
    describe "Test of network functions:" $ do
        it "Generates a random network" $
            N.fromList [3, 2, 1] [A.ReLu, A.Sign] generator `shouldBe` N.Network [A.ReLu, A.Sign] [
                M.Matrix 3 2 $ V.fromList [0.2677647642349532, -0.7475585300807475, -0.8720265567974974, -1.6527016918064907e-2, -0.3026656798412395, 0.35555427661286965],
                M.Matrix 2 1 $ V.fromList [-0.7860612064870318, -0.34481051139882446]
            ]

main :: IO ()
main =
    hspec $
        testUtils >>
        testMatrix >>
        testActivation >>
        testNetwork
