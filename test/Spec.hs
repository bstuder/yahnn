import qualified Activation as A
import qualified Data.Either as DE (fromRight, isLeft, isRight)
import qualified Data.Vector as DV (empty, fromList, Vector(..))
import qualified Loss as L
import qualified Matrix as M
import qualified Network as N
import qualified System.Random as SR (mkStdGen)
import qualified Test.Hspec as TH (describe, hspec, it, shouldBe, shouldSatisfy)
import qualified Test.QuickCheck as TQ (Arbitrary(..), property, arbitrary, vector, getPositive)
import qualified Utils as U

list = [-2, 3, 6, 1, -8, -9]
vector = DV.fromList list
matrix = M.unsafeFromList 2 3 list
network = N.unsafeFromLists [A.ReLu, A.ReLu, A.ReLu, A.ReLu] [
        M.unsafeFromList 3 6 [2, -1, 3, 7, -5, 1, 5, -1, 2, 1, 7, -3, 6, 4, -8, 1, -2, -4],
        M.unsafeFromList 2 3 [3, -1, 2, 2, -5, 1],
        M.unsafeFromList 3 2 [1, 1, -4, 8, 3, 2],
        M.unsafeFromList 6 3 [-7, 3, 2, -1, 4, -6, 6, -2, -1, 5, 2, 4, -1, 8, 3, 2, -4, 2]
    ]

generator = SR.mkStdGen 12345

instance (RealFloat a, TQ.Arbitrary a) => TQ.Arbitrary (M.Matrix a) where
    arbitrary = do
        rows <- TQ.getPositive <$> TQ.arbitrary
        columns <- TQ.getPositive <$> TQ.arbitrary
        list <- TQ.vector (rows * columns)
        return $ M.unsafeFromList rows columns list

testUtils =
    TH.describe "Test of utility functions:" $
        TH.it "Chunk of a vector" $ do
            U.chunksOf 5 (DV.empty :: DV.Vector Int) `TH.shouldBe` DV.empty
            U.chunksOf 2 vector `TH.shouldBe` DV.fromList (fmap DV.fromList [[-2, 3], [6, 1], [-8, -9]])
            U.chunksOf 5 vector `TH.shouldBe` DV.fromList (fmap DV.fromList [[-2, 3, 6, 1, -8], [-9]])

testMatrix =
    TH.describe "Test of linear algebra functions:" $ do
        TH.it "Sum of matrix rows" $
            M.applyRow sum matrix `TH.shouldBe` DV.fromList [7, -16]
        TH.it "Multiplication of two vectors" $
            M.fromVectors vector (DV.fromList [-1, 5]) `TH.shouldBe` M.unsafeFromList 6 2 [2, -10, -3, 15, -6, 30, -1, 5, 8, -40, 9, -45]
        TH.it "Multiplication of a vector and a matrix" $ do
            M.multiplyVectorL (DV.fromList [-1, 5]) matrix `TH.shouldBe` Right (DV.fromList [7, -43, -51])
            M.multiplyVectorL vector matrix `TH.shouldSatisfy` DE.isLeft
        TH.it "Multiplication of a matrix and a vector" $ do
            M.multiplyVectorR matrix (DV.fromList [0, -6, 3]) `TH.shouldBe` Right (DV.fromList [0, 21])
            M.multiplyVectorR matrix vector `TH.shouldSatisfy` DE.isLeft
        TH.it "Multiplication of two matrices" $ do
            matrix `M.multiplyMatrices` M.unsafeFromList 3 3 [-4..4] `TH.shouldBe` M.fromList 2 3 [17, 24, 31, -14, -30, -46]
            matrix `M.multiplyMatrices` M.unsafeFromList 2 2 [1, 1, 1, 1] `TH.shouldSatisfy` DE.isLeft
        TH.it "Transpose of matrices" $
            M.transpose matrix `TH.shouldBe` M.unsafeFromList 3 2 [-2, 1, 3, -8, 6, -9]
        TH.it "Transpose of a matrix twice" $
            TQ.property $ \m -> M.transpose (M.transpose m) == (m :: M.Matrix Double)
        TH.it "Multiplication of a matrix and its transposed" $
            TQ.property $ \m -> DE.isRight (m `M.multiplyMatrices` M.transpose (m :: M.Matrix Double))
        TH.it "Equality between matrices with tolerance" $ do
            matrix `TH.shouldSatisfy` M.equal 0 matrix
            matrix `TH.shouldSatisfy` not . M.equal 0 (M.transpose matrix)
            matrix `TH.shouldSatisfy` not . M.equal 0.1 (M.unsafeFromList 2 3 [-2, 3, 6+0.11, 1, -8, -9])
            matrix `TH.shouldSatisfy` M.equal 0.2 (M.unsafeFromList 2 3 [-2.15, 3.19, 5.9, 1.2, -8.2, -9])
        {-TH.it "Equality between matrices and diagonal matrices" $ do
            let diagonal = DV.fromList [-1, 0, 2, 0]
            M.Matrix 2 2 diagonal `TH.shouldBe` M.MatrixDiagonal 2 2 diagonal-}
        TH.it "Extraction of matrix's diagonal" $ do
            M.diagonal matrix `TH.shouldBe` DV.fromList [-2, -8]
            M.diagonal (M.transpose matrix) `TH.shouldBe` DV.fromList [-2, -8]

testActivation =
    TH.describe "Test of activation functions:" $
        TH.it "ReLu activation function" $ do
            A.forward A.ReLu vector `TH.shouldBe` DV.fromList [0, 3, 6, 1, 0, 0]
            A.derivate A.ReLu vector `TH.shouldBe` DV.fromList [0, 1, 1, 1, 0, 0]

testNetwork =
    TH.describe "Test of network functions:" $ do
        TH.it "Generation of a random network" $ do
            N.random [3, 2, 1] [A.ReLu, A.TanH] generator `TH.shouldBe` N.fromLists [A.ReLu, A.TanH] [
                M.unsafeFromList 2 3 [1.9543818196252394e-2, -8.256066438750898e-2, 0.30326905954505934, 0.3728469630471347, -0.40816135066028125, -0.7351927684114008],
                M.unsafeFromList 1 2 [9.31527772916203e-2, -4.6601584116810146e-2]
                ]
            N.random [3, 2] [A.ReLu, A.TanH] generator `TH.shouldSatisfy` DE.isLeft

        let forwardResult = N.forward vector network

        TH.it "Forward of an input" $
            forwardResult `TH.shouldBe` Right (N.ForwardResult
                (DV.fromList <$> [[49, -29, 5], [157, 103], [260, 196, 677], [122, -3538, 491, 4400, 3339, 1090]])
                (DV.fromList <$> [[-2, 3, 6, 1, -8, -9], [49, 0, 5], [157, 103], [260, 196, 677], [122, 0, 491, 4400, 3339, 1090]])
           )
        TH.it "Propagation of a gradient" $ do
            let backwardExpected = [
                    M.unsafeFromList 3 6 [-414356.03125, 621534.0625, 1243068.125, 207178.015625, -1657424.125, -1864602.125, 0, 0, 0, 0, 0, 0, -203632.015625, 305448.03125, 610896.0625, 101816.0078125, -814528.0625, -916344.0625],
                    M.unsafeFromList 2 3 [-173754.234375, 0, -17730.0234375, 5336493, 0, 544540.0625],
                    M.unsafeFromList 3 2 [1197805.375, 785821.375, 1600353.625, 1049913.5, 1548962.125, 1016198.125],
                    M.unsafeFromList 6 3 [10746.6669921875, 8101.333984375, 27982.66796875, 0, 0, 0, 42033.3359375, 31686.66796875, 109448.3359375, 381246.6875, 287401.34375, 992707.6875, 290073.34375, 218670.6875, 755306.375, 95246.671875, 71801.3359375, 248007.671875]
                    ]
            (forwardResult >>= \justForwardResult -> N.backward network justForwardResult vector L.MSE) `TH.shouldSatisfy`
                \backwardResult -> any (and . zipWith (M.equal 1) backwardExpected) backwardResult

main :: IO ()
main = TH.hspec $ do
    testUtils
    testMatrix
    testActivation
    testNetwork
