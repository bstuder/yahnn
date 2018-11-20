import qualified Activation as A
import qualified Data.Either as E (isLeft, Either(..))
import qualified Data.Vector as V (empty, fromList, Vector(..))
import qualified Test.Hspec as T (describe, hspec, it, shouldBe, shouldSatisfy)
import qualified Test.QuickCheck as TQ (Arbitrary(..), property, arbitrary, vector, getPositive)
import qualified Matrix as M
import qualified Network as N
import qualified System.Random as S (mkStdGen)
import qualified Utils as U

vector = V.fromList [-2, 3, 6, 1, -8, -9, 3, -5]
matrix = M.Matrix 2 4 vector
network = N.Network [A.ReLu, A.ReLu] [
        M.Matrix 2 3 $ V.fromList [2, -1, 3, 7, -5, 0],
        M.Matrix 1 2 $ V.fromList [3, -1]
    ]

generator = S.mkStdGen 12345

instance TQ.Arbitrary a => TQ.Arbitrary (M.Matrix a) where
    arbitrary = do
        rows <- TQ.getPositive <$> TQ.arbitrary
        columns <- TQ.getPositive <$> TQ.arbitrary
        vector <- V.fromList <$> TQ.vector (rows * columns)
        return $ M.Matrix rows columns vector

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
        T.it "Multiplies vector with mismatching dimensions" $
            M.multiplyVector matrix vector `T.shouldSatisfy` E.isLeft
        T.it "Multiplies vector with matching dimensions" $
            M.multiplyVector matrix (V.fromList [0, -6, 3, 1]) `T.shouldBe` E.Right (V.fromList [1, 58])
        T.it "Transposes a matrix" $
            M.transpose matrix `T.shouldBe` M.Matrix 4 2 (V.fromList [-2, -8, 3, -9, 6, 3, 1, -5])
        T.it "Transposes several matrices twice" $
            TQ.property $ \m -> M.transpose (M.transpose m) == (m :: M.Matrix Int)

testActivation =
    T.describe "Test of activation functions:" $ do
        T.it "Forwards a ReLu activation" $
            A.forward A.ReLu vector `T.shouldBe` V.fromList [0, 3, 6, 1, 0, 0, 3, 0]
        T.it "Derivates a ReLu activation" $
            A.derivate A.ReLu vector `T.shouldBe` V.fromList [0, 1, 1, 1, 0, 0, 1, 0]
        T.it "Forwards a Sign activation" $
            A.forward A.Sign vector `T.shouldBe` V.fromList [-1, 1, 1, 1, -1, -1, 1, -1]

testNetwork =
    T.describe "Test of network functions:" $ do
        T.it "Generates a random network with mismatching dimensions" $
            N.fromList [3, 2] [A.ReLu, A.Sign] generator `T.shouldSatisfy` E.isLeft
        T.it "Generates a random network with matching dimensions" $
            N.fromList [3, 2, 1] [A.ReLu, A.Sign] generator `T.shouldBe` E.Right (N.Network [A.ReLu, A.Sign] [
                M.Matrix 2 3 $ V.fromList [1.9543818196252394e-2, -8.256066438750898e-2, 0.30326905954505934, 0.3728469630471347, -0.40816135066028125, -0.7351927684114008],
                M.Matrix 1 2 $ V.fromList [9.31527772916203e-2, -4.6601584116810146e-2]
            ])
        T.it "Forwards an input through a network" $
            N.forward (V.fromList [-1, -3, -2]) network `T.shouldBe` E.Right (N.ForwardResult
                (V.fromList <$> [[-5, 8], [-8]])
                (V.fromList <$> [[0, 8], [0]])
           )

main :: IO ()
main = T.hspec $ do
    testUtils
    testMatrix
    testActivation
    testNetwork
