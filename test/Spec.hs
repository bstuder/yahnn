import qualified Activation as A
import qualified Data.Either as DE (fromRight, isLeft, isRight)
import qualified Data.Vector as DV (empty, fromList, Vector(..))
import qualified Loss as L
import qualified Matrix as M
import qualified Network as N
import qualified System.Random as SR (mkStdGen)
import qualified Test.Hspec as TH (describe, hspec, it, shouldBe, shouldSatisfy, Spec)
import qualified Test.QuickCheck as TQ (Arbitrary(..), property, arbitrary, vector, getPositive)
import qualified Utils as U

vector = DV.fromList [-2, 3, 6, 1, -8, -9]

firstRowVector = M.unsafeFromList 1 2 [4, -6]
secondRowVector = M.unsafeFromList 1 2 [7, 0]
firstColumnVector = M.unsafeFromList 2 1 [6, -4]
secondColumnVector = M.unsafeFromList 2 1 [-1, -2]
firstFullSquareMatrix = M.unsafeFromList 2 2 [-2, 3, 6, 1]
secondFullSquareMatrix = M.unsafeFromList 2 2 [-1, -1, 5, 3]
firstFullRectangularMatrix = M.unsafeFromList 3 2 [1, 8, -5, 4, -4, 0]
secondFullRectangularMatrix = M.unsafeFromList 2 3 [0, -2, 3, 6, 1, -8]
firstDiagonalMatrix = M.unsafeFromList 2 2 [4, -1]
secondDiagonalMatrix = M.unsafeFromList 2 2 [5, 2]

network = N.unsafeFromLists [A.ReLu, A.ReLu, A.ReLu, A.ReLu] [
        M.unsafeFromList 3 1 [0, 0, 0],
        M.unsafeFromList 2 1 [0, 0],
        M.unsafeFromList 3 1 [0, 0, 0],
        M.unsafeFromList 6 1 [0, 0, 0, 0, 0, 0]
    ] [
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

testUtils :: TH.Spec
testUtils =
    TH.describe "Test of utility functions:" $
        TH.it "Chunk of a vector" $ do
            U.chunksOf 5 (DV.empty :: DV.Vector Int) `TH.shouldBe` DV.empty
            U.chunksOf 2 vector `TH.shouldBe` DV.fromList (fmap DV.fromList [[-2, 3], [6, 1], [-8, -9]])
            U.chunksOf 5 vector `TH.shouldBe` DV.fromList (fmap DV.fromList [[-2, 3, 6, 1, -8], [-9]])

testMatrix :: TH.Spec
testMatrix =
    TH.describe "Test of linear algebra functions:" $ do
        TH.it "Equality between matrices with tolerance" $ do
            firstFullRectangularMatrix `TH.shouldSatisfy` M.equal 0 firstFullRectangularMatrix
            firstFullRectangularMatrix `TH.shouldSatisfy` not . M.equal 0 (M.transpose firstFullRectangularMatrix)
            firstFullRectangularMatrix `TH.shouldSatisfy` not . M.equal 0.1 (M.unsafeFromList 3 2 [1, 8, -5, 4 + 0.11, -4, 0, 2])
            firstFullRectangularMatrix `TH.shouldSatisfy` M.equal 0.2 (M.unsafeFromList 3 2 [0.81, 8.15, -5.10, 3.92, -4.04, -0.13])

        TH.it "Extraction of matrices" $ do
            M.take M.Columns 1 firstFullRectangularMatrix `TH.shouldBe` M.fromList 3 1 [1, -5, -4]
            M.take M.Rows 2 firstFullRectangularMatrix `TH.shouldBe` M.fromList 2 2 [1, 8, -5, 4]
            M.init M.Columns firstFullRectangularMatrix `TH.shouldBe` M.fromList 3 1 [1, -5, -4]
            M.init M.Rows firstFullRectangularMatrix `TH.shouldBe` M.fromList 2 2 [1, 8, -5, 4]
            M.take M.Columns 10 firstFullRectangularMatrix `TH.shouldBe` Right M.empty
            M.take M.Rows 10 firstFullRectangularMatrix `TH.shouldBe` Right M.empty

        TH.it "Sum of matrix rows" $
            M.applyRow sum firstFullSquareMatrix `TH.shouldBe` DV.fromList [1, 7]

        TH.it "Transpose of matrices" $ do
            M.transpose firstFullRectangularMatrix `TH.shouldBe` M.unsafeFromList 2 3 [1, -5, -4, 8, 4, 0]
            M.transpose firstDiagonalMatrix `TH.shouldBe` firstDiagonalMatrix
            M.transpose firstColumnVector `TH.shouldBe` M.unsafeFromList 1 2 [6, -4]

        TH.it "Transpose of a matrix twice" $
            TQ.property $ \m -> M.transpose (M.transpose m) == (m :: M.Matrix Double)

        TH.it "Multiplication of a matrix and its transposed" $
            TQ.property $ \m -> DE.isRight (m `M.multiplyMatrices` M.transpose (m :: M.Matrix Double))

        TH.it "Concatenation of two matricee" $ do
            M.concatenate M.Columns (M.transpose firstFullRectangularMatrix) secondFullRectangularMatrix `TH.shouldBe` M.fromList 2 6 [1, -5, -4, 0, -2, 3, 8, 4, 0, 6, 1, -8]
            M.concatenate M.Rows firstFullRectangularMatrix (M.transpose secondFullRectangularMatrix) `TH.shouldBe` M.fromList 6 2 [1, 8, -5, 4, -4, 0, 0, 6, -2, 1, 3, -8]
            M.concatenate M.Columns firstFullRectangularMatrix secondFullRectangularMatrix `TH.shouldSatisfy` DE.isLeft
            M.concatenate M.Rows firstFullRectangularMatrix secondFullRectangularMatrix `TH.shouldSatisfy` DE.isLeft

        TH.it "Addition of two matrices" $ do
            M.addMatrices firstFullSquareMatrix secondFullSquareMatrix `TH.shouldBe` M.fromList 2 2 [-3, 2, 11, 4]
            M.addMatrices firstDiagonalMatrix secondFullSquareMatrix `TH.shouldBe` M.fromList 2 2 [3, -1, 5, 2]
            M.addMatrices firstFullSquareMatrix secondDiagonalMatrix `TH.shouldBe` M.fromList 2 2 [3, 3, 6, 3]
            M.addMatrices firstDiagonalMatrix secondDiagonalMatrix `TH.shouldBe` M.fromList 2 2 [9, 1]
            M.addMatrices firstRowVector secondRowVector `TH.shouldBe` M.fromList 1 2 [11, -6]
            M.addMatrices firstColumnVector secondColumnVector `TH.shouldBe` M.fromList 2 1 [5, -6]
            M.addMatrices firstFullSquareMatrix secondColumnVector `TH.shouldSatisfy` DE.isLeft
            M.addMatrices firstRowVector secondDiagonalMatrix `TH.shouldSatisfy` DE.isLeft
            M.addMatrices firstRowVector secondColumnVector `TH.shouldSatisfy` DE.isLeft

        TH.it "Multiplication of two matrices" $ do
            M.multiplyMatrices firstFullRectangularMatrix secondFullRectangularMatrix `TH.shouldBe` M.fromList 3 3 [48, 6, -61, 24, 14, -47, 0, 8, -12]
            M.multiplyMatrices firstFullRectangularMatrix secondFullSquareMatrix `TH.shouldBe` M.fromList 3 2 [39, 23, 25, 17, 4, 4]
            M.multiplyMatrices firstFullSquareMatrix secondFullSquareMatrix `TH.shouldBe` M.fromList 2 2 [17, 11, -1, -3]
            M.multiplyMatrices firstDiagonalMatrix secondFullSquareMatrix `TH.shouldBe` M.fromList 2 2 [-4, -4, -5, -3]
            M.multiplyMatrices firstFullRectangularMatrix secondDiagonalMatrix `TH.shouldBe` M.fromList 3 2 [5, 16, -25, 8, -20, 0]
            M.multiplyMatrices firstDiagonalMatrix secondDiagonalMatrix `TH.shouldBe` M.fromList 2 2 [20, -2]
            M.multiplyMatrices firstRowVector secondColumnVector `TH.shouldBe` M.fromList 1 1 [8]
            M.multiplyMatrices firstColumnVector secondRowVector `TH.shouldBe` M.fromList 2 2 [42, 0, -28, 0]
            M.multiplyMatrices firstRowVector secondFullSquareMatrix `TH.shouldBe` M.fromList 1 2 [-34, -22]
            M.multiplyMatrices firstFullSquareMatrix secondColumnVector `TH.shouldBe` M.fromList 2 1 [-4, -8]
            M.multiplyMatrices firstFullSquareMatrix secondRowVector `TH.shouldSatisfy` DE.isLeft
            M.multiplyMatrices firstColumnVector secondDiagonalMatrix `TH.shouldSatisfy` DE.isLeft

testActivation :: TH.Spec
testActivation =
    TH.describe "Test of activation functions:" $
        TH.it "ReLu activation function" $ do
            A.forward A.ReLu firstColumnVector `TH.shouldBe` M.fromList 2 1 [6, 0]
            A.backward A.ReLu firstColumnVector `TH.shouldBe` M.fromList 2 2 [1, 0]

testNetwork :: TH.Spec
testNetwork =
    TH.describe "Test of network functions:" $ do
        TH.it "Generation of a random network" $ do
            N.random [3, 2, 1] [A.ReLu, A.TanH] generator `TH.shouldBe` N.fromLists [A.ReLu, A.TanH] [
                M.unsafeFromList 1 2 [0, 0],
                M.unsafeFromList 1 1 [0]
                ] [
                M.unsafeFromList 2 3 [1.9543818196252394e-2, -8.256066438750898e-2, 0.30326905954505934, 0.3728469630471347, -0.40816135066028125, -0.7351927684114008],
                M.unsafeFromList 1 2 [9.31527772916203e-2, -4.6601584116810146e-2]
                ]
            N.random [3, 2] [A.ReLu, A.TanH] generator `TH.shouldSatisfy` DE.isLeft

        let datapoint = M.unsafeFromList 6 1 [-2, 3, 6, 1, -8, -9]
        let forwardResult = N.forward datapoint network

        TH.it "Forward of an input" $ do
            let toColumnVector = \vector -> M.unsafeFromList (length vector) 1 vector
            forwardResult `TH.shouldBe` Right (N.ForwardResult
                (toColumnVector <$> [[49, -29, 5], [157, 103], [260, 196, 677], [122, -3538, 491, 4400, 3339, 1090]])
                (toColumnVector <$> [[-2, 3, 6, 1, -8, -9], [49, 0, 5], [157, 103], [260, 196, 677], [122, 0, 491, 4400, 3339, 1090]])
                )
        {-TH.it "Propagation of a gradient" $ do
            let backwardExpected = ([
                    M.unsafeFromList 3 1 [0, 0, 0],
                    M.unsafeFromList 2 1 [0, 0],
                    M.unsafeFromList 3 1 [0, 0, 0],
                    M.unsafeFromList 6 1 [0, 0, 0, 0, 0, 0]
                    ], [
                    M.unsafeFromList 3 6 [-414356.03125, 621534.0625, 1243068.125, 207178.015625, -1657424.125, -1864602.125, 0, 0, 0, 0, 0, 0, -203632.015625, 305448.03125, 610896.0625, 101816.0078125, -814528.0625, -916344.0625],
                    M.unsafeFromList 2 3 [-173754.234375, 0, -17730.0234375, 5336493, 0, 544540.0625],
                    M.unsafeFromList 3 2 [1197805.375, 785821.375, 1600353.625, 1049913.5, 1548962.125, 1016198.125],
                    M.unsafeFromList 6 3 [10746.6669921875, 8101.333984375, 27982.66796875, 0, 0, 0, 42033.3359375, 31686.66796875, 109448.3359375, 381246.6875, 287401.34375, 992707.6875, 290073.34375, 218670.6875, 755306.375, 95246.671875, 71801.3359375, 248007.671875]
                    ])
            (forwardResult >>= \justForwardResult -> N.backward network justForwardResult datapoint L.MSE) `TH.shouldSatisfy`
                \backwardResult -> any (and . zipWith (M.equal 1) backwardExpected) backwardResult-}

main :: IO ()
main = TH.hspec $ do
    testUtils
    testMatrix
    testActivation
    testNetwork
