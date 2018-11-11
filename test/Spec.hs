import qualified Activation as A
import qualified Data.Vector as V (empty, fromList, Vector(..))
import qualified Test.Hspec as T (describe, hspec, it, shouldBe)
import qualified Test.QuickCheck as TQ (Arbitrary(..), property, arbitrary, vector, getPositive)
import qualified Matrix as M
import qualified Network as N
import qualified System.Random as S (mkStdGen)
import qualified Utils as U

vector = V.fromList [-2, 3, 6, 1, -8, -9, 3, -5]
vectorTranspose = V.fromList [-2, -8, 3, -9, 6, 3, 1, -5]

matrix = M.Matrix 2 4 vector
matrixTranspose = M.Matrix 4 2 vectorTranspose

instance TQ.Arbitrary a => TQ.Arbitrary (M.Matrix a) where
    arbitrary = do
        nrows <- TQ.getPositive <$> TQ.arbitrary
        ncols <- TQ.getPositive <$> TQ.arbitrary
        vec <- V.fromList <$> TQ.vector (nrows * ncols)
        return $ M.Matrix nrows ncols vec

testUtils =
    T.describe "Test of utility functions:" $ do
        T.it "Chunks an empty vector" $
            U.chunksOf 5 (V.empty :: V.Vector Int) `T.shouldBe` V.empty
        T.it "Even chunks a vector" $
            U.chunksOf 2 vector `T.shouldBe` V.fromList (fmap V.fromList [[-2, 3], [6, 1], [-8, -9], [3, -5]])
        T.it "Uneven chunks a vector" $
            U.chunksOf 3 vector `T.shouldBe` V.fromList (fmap V.fromList [[-2, 3, 6], [1, -8, -9], [3, -5]])

testMatrix =
    T.describe "Test of matrix functions:" $ do
        T.it "Sums rows" $
            M.applyRow sum matrix `T.shouldBe` V.fromList [8, -19]
        T.it "Multiplies vector with unmatching dimensions" $
            M.multiplyVector matrix vector `T.shouldBe` V.empty
        T.it "Multiplies vector with matching dimensions" $
            M.multiplyVector matrix (V.fromList [0, -6, 3, 1]) `T.shouldBe` V.fromList [1, 58]
        T.it "Transpose Matrix" $
            M.transpose matrix `T.shouldBe` matrixTranspose
        T.it "double transpose is the identity" $
            TQ.property $ \m -> M.transpose (M.transpose m) == (m :: M.Matrix Int)

testActivation =
    T.describe "Test of activation functions:" $ do
        T.it "Forwards a ReLu activation" $
            A.activationForward A.ReLu vector `T.shouldBe` V.fromList [0, 3, 6, 1, 0, 0, 3, 0]
        T.it "Derivates a ReLu activation" $
            A.activationDerivate A.ReLu vector `T.shouldBe` V.fromList [0, 1, 1, 1, 0, 0, 1, 0]
        T.it "Forwards a Sign activation" $
            A.activationForward A.Sign vector `T.shouldBe` V.fromList [-1, 1, 1, 1, -1, -1, 1, -1]

generator = S.mkStdGen 12345

testNetwork =
    T.describe "Test of network functions:" $ do
        T.it "Generates a random network" $
            N.fromList [3, 2, 1] [A.ReLu, A.Sign] generator `T.shouldBe` N.Network [A.ReLu, A.Sign] [
                M.Matrix 3 2 $ V.fromList [1.9543818196252394e-2, -8.256066438750898e-2, 0.30326905954505934, 0.3728469630471347, -0.40816135066028125, -0.7351927684114008],
                M.Matrix 2 1 $ V.fromList [9.31527772916203e-2, -4.6601584116810146e-2]
            ]

main :: IO ()
main =
    T.hspec $
        testUtils >>
        testMatrix >>
        testActivation >>
        testNetwork
