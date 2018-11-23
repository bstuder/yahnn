import qualified Activation as A
import qualified Data.Either as E
import qualified Data.Vector as V (empty, fromList, Vector(..))
import qualified Test.Hspec as T (describe, hspec, it, shouldBe, shouldSatisfy)
import qualified Test.QuickCheck as TQ (Arbitrary(..), property, arbitrary, vector, getPositive)
import qualified Loss as L
import qualified Matrix as M
import qualified Network as N
import qualified System.Random as S (mkStdGen)
import qualified Utils as U

vector = V.fromList [-2, 3, 6, 1, -8, -9]
matrix = M.Matrix 2 3 vector
network = N.Network [A.ReLu, A.ReLu, A.ReLu, A.ReLu] [
        M.Matrix 3 6 $ V.fromList [2, -1, 3, 7, -5, 1, 5, -1, 2, 1, 7, -3, 6, 4, -8, 1, -2, -4],
        M.Matrix 2 3 $ V.fromList [3, -1, 2, 2, -5, 1],
        M.Matrix 3 2 $ V.fromList [1, 1, -4, 8, 3, 2],
        M.Matrix 6 3 $ V.fromList [-7, 3, 2, -1, 4, -6, 6, -2, -1, 5, 2, 4, -1, 8, 3, 2, -4, 2]
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
            U.chunksOf 2 vector `T.shouldBe` V.fromList (fmap V.fromList [[-2, 3], [6, 1], [-8, -9]])
        T.it "Uneven chunks a vector" $
            U.chunksOf 5 vector `T.shouldBe` V.fromList (fmap V.fromList [[-2, 3, 6, 1, -8], [-9]])

testMatrix =
    T.describe "Test of linear algebra functions:" $ do
        T.it "Sums rows of a matrix" $
            M.applyRow sum matrix `T.shouldBe` V.fromList [7, -16]
        T.it "Multiplies two vectors" $
            M.fromVectors vector (V.fromList [-1, 5]) `T.shouldBe` M.Matrix 6 2 (V.fromList [2, -10, -3, 15, -6, 30, -1, 5, 8, -40, 9, -45])
        T.it "Multiplies a vector with a matrix of matching dimensions" $
            M.multiplyVectorL (V.fromList [-1, 5]) matrix `T.shouldBe` E.Right (V.fromList [7, -43, -51])
        T.it "Multiplies a vector with a matrix of mismatching dimensions" $
            M.multiplyVectorL vector matrix `T.shouldSatisfy` E.isLeft
        T.it "Multiplies a matrix with a vector of matching dimensions" $
            M.multiplyVectorR matrix (V.fromList [0, -6, 3]) `T.shouldBe` E.Right (V.fromList [0, 21])
        T.it "Multiplies a matrix with a vector of mismatching dimensions" $
            M.multiplyVectorR matrix vector `T.shouldSatisfy` E.isLeft
        T.it "Transposes a matrix" $
            M.transpose matrix `T.shouldBe` M.Matrix 3 2 (V.fromList [-2, 1, 3, -8, 6, -9])
        T.it "Transposes several matrices twice" $
            TQ.property $ \m -> M.transpose (M.transpose m) == (m :: M.Matrix Int)
        T.it "Multiplies two matrices with mismatching dimensions" $
            matrix `M.multiplyMatrices` M.Matrix 2 2 (V.fromList [1, 1, 1, 1]) `T.shouldSatisfy` E.isLeft
        T.it "Multiplies two matrices with matching dimensions" $
            matrix `M.multiplyMatrices` M.Matrix 3 3 (V.fromList [-4..4]) `T.shouldBe` E.Right (M.Matrix 2 3 (V.fromList [17, 24, 31, -14, -30, -46]))
        T.it "Multiplies several matrices by their transpose" $
            TQ.property $ \m -> E.isRight (m `M.multiplyMatrices` M.transpose (m :: M.Matrix Double))

testActivation =
    T.describe "Test of activation functions:" $ do
        T.it "Forwards a ReLu activation" $
            A.forward A.ReLu vector `T.shouldBe` V.fromList [0, 3, 6, 1, 0, 0]
        T.it "Derivates a ReLu activation" $
            A.derivate A.ReLu vector `T.shouldBe` V.fromList [0, 1, 1, 1, 0, 0]

testNetwork =
    T.describe "Test of network functions:" $ do
        T.it "Generates a random network with mismatching dimensions" $
            N.fromList [3, 2] [A.ReLu, A.Sign] generator `T.shouldSatisfy` E.isLeft
        T.it "Generates a random network with matching dimensions" $
            N.fromList [3, 2, 1] [A.ReLu, A.Sign] generator `T.shouldBe` E.Right (N.Network [A.ReLu, A.Sign] [
                M.Matrix 2 3 $ V.fromList [1.9543818196252394e-2, -8.256066438750898e-2, 0.30326905954505934, 0.3728469630471347, -0.40816135066028125, -0.7351927684114008],
                M.Matrix 1 2 $ V.fromList [9.31527772916203e-2, -4.6601584116810146e-2]
            ])

        let forwardResult = N.forward vector network

        T.it "Forwards an input through a network" $
            forwardResult `T.shouldBe` E.Right (N.ForwardResult
                (V.fromList <$> [[49, -29, 5], [157, 103], [260, 196, 677], [122, -3538, 491, 4400, 3339, 1090]])
                (V.fromList <$> [[-2, 3, 6, 1, -8, -9], [49, 0, 5], [157, 103], [260, 196, 677], [122, 0, 491, 4400, 3339, 1090]])
           )
        T.it "Computes a gradient for a given target" $
            (forwardResult >>= \justForwardResult -> N.backward network justForwardResult vector L.MSE) `T.shouldBe` E.Right ([
                M.Matrix 3 6 $ V.fromList [-414356.03125, 621534.0625, 1243068.125, 207178.015625, -1657424.125, -1864602.125, 0, 0, 0, 0, 0, 0, -203632.015625, 305448.03125, 610896.0625, 101816.0078125, -814528.0625, -916344.0625],
                M.Matrix 2 3 $ V.fromList [-173754.234375, 0, -17730.0234375, 5336493, 0, 544540.0625],
                M.Matrix 3 2 $ V.fromList [1197805.375, 785821.375, 1600353.625, 1049913.5, 1548962.125, 1016198.125],
                M.Matrix 6 3 $ V.fromList [10746.6669921875, 8101.333984375, 27982.66796875, 0, 0, 0, 42033.3359375, 31686.66796875, 109448.3359375, 381246.6875, 287401.34375, 992707.6875, 290073.34375, 218670.6875, 755306.375, 95246.671875, 71801.3359375, 248007.671875]
            ])

main :: IO ()
main = T.hspec $ do
    testUtils
    testMatrix
    testActivation
    testNetwork
