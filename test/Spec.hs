import qualified Activation as A
import qualified Data.Either as DE (fromRight, isLeft, isRight)
import qualified Data.Vector as DV (empty, fromList)
import qualified Data.Vector.Unboxed as DVU (empty, fromList)
import qualified Loss as L
import qualified Matrix as M
import qualified Network as N
import qualified System.Random as SR (mkStdGen)
import qualified Test.Hspec as TH (describe, hspec, it, shouldBe, shouldSatisfy, Spec)
import qualified Test.QuickCheck as TQ (Arbitrary(..), property, arbitrary, vector, getPositive)
import qualified Utils as U


{----- INSTANCES -----}

instance TQ.Arbitrary M.Matrix where
    arbitrary = do
        rows    <- TQ.getPositive <$> TQ.arbitrary
        columns <- TQ.getPositive <$> TQ.arbitrary
        list    <- TQ.vector (rows * columns)
        return $ M.unsafeFromList rows columns list


{----- UTILITY METHODS -----}

equalBackwardResults :: ([M.Matrix], [M.Matrix]) -> ([M.Matrix], [M.Matrix]) -> Bool
equalBackwardResults (firstBiases, firstWeights) (secondBiases, secondWeights) =
    equalLists firstBiases secondBiases && equalLists firstWeights secondWeights
  where
    equalLists firstList secondList = and $ zipWith (M.equal 1e-5) firstList secondList

equalDouble :: Double -> Either String Double -> Bool
equalDouble firstValue secondValue = DE.fromRight False $ U.equalDouble 1e-5 firstValue <$> secondValue

equalMatrix :: M.Matrix -> Either String M.Matrix -> Bool
equalMatrix firstMatrix secondMatrix = DE.fromRight False $ M.equal 1e-5 firstMatrix <$> secondMatrix

toColumnVector :: [Double] -> M.Matrix
toColumnVector list = M.unsafeFromList (length list) 1 list


{----- TEST METHODS -----}

testActivation :: TH.Spec
testActivation = do
    let vector = M.unsafeFromList 5 1 [-0.1, 0.3, 0.2, -0.5, -0.7]

    TH.describe "Test of activation functions:" $ do
        TH.it "ReLu function" $ do
            A.forward A.ReLu vector `TH.shouldBe` M.fromList 5 1 [0, 0.3, 0.2, 0, 0]
            A.backward A.ReLu vector `TH.shouldBe` M.fromList 5 5 [0, 1, 1, 0, 0]
        TH.it "SoftMax function" $ do
            A.forward A.SoftMax vector `TH.shouldSatisfy` (equalMatrix $ M.unsafeFromList 5 1 [0.1975966248481474, 0.2947795251190231, 0.2667275443985631, 0.1324529786646971, 0.1084433269695693])
            A.backward A.SoftMax vector `TH.shouldSatisfy` (equalMatrix $ M.unsafeFromList 5 5 [
                0.1585521986967679, -0.0582474392378586, -0.0527044625271905, -0.0261722615352278, -0.021428035396491,
                -0.0582474392378586, 0.2078845566896263, -0.0786258188739716, -0.0390444261513795, -0.0319668724264166,
                -0.0527044625271905, -0.0786258188739716, 0.1955839614576756, -0.0353288577475099, -0.0289248223090037,
                -0.0261722615352278, -0.0390444261513795, -0.0353288577475099, 0.1149091871075464, -0.0143636416734291,
                -0.021428035396491, -0.0319668724264166, -0.0289248223090037, -0.0143636416734291, 0.0966833718053404
                ])

testLosses :: TH.Spec
testLosses = do
    let input = M.unsafeFromList 5 1 [-0.1, 0.3, 0.2, -0.5, -0.7]
    let target = M.unsafeFromList 5 1 [0, 0, 1, 0, 0]

    TH.describe "Test of losses functions:" $ do
        TH.it "Cross-Entropy function" $ do
            let transformedInput = (M.map abs input)
            L.forward L.CrossEntropy transformedInput target `TH.shouldSatisfy` (equalDouble 1.6094379124341003)
            L.backward L.CrossEntropy transformedInput target `TH.shouldSatisfy` (equalMatrix $ M.unsafeFromList 1 5 [0, 0, -5, 0, 0])
        TH.it "Mean Squared Error function" $ do
            L.forward L.MSE input target `TH.shouldSatisfy` (equalDouble 0.296)
            L.backward L.MSE input target `TH.shouldSatisfy` (equalMatrix $ M.unsafeFromList 1 5 [-0.04, 0.12, -0.32, -0.2, -0.28])
        TH.it "Negative Log Likelihood with SoftMax function" $ do
            L.forward L.NLLSoftMax input target `TH.shouldSatisfy` (equalDouble 1.3215275745422457)
            L.backward L.NLLSoftMax input target `TH.shouldSatisfy` (equalMatrix $ M.unsafeFromList 1 5 [0.1975966248481474, 0.2947795251190231, -0.733272455601437, 0.1324529786646971, 0.1084433269695693])

testMatrix :: TH.Spec
testMatrix = do
    let firstRowVector = M.unsafeFromList 1 2 [4, -6]
    let secondRowVector = M.unsafeFromList 1 2 [7, 0]
    let firstColumnVector = M.unsafeFromList 2 1 [6, -4]
    let secondColumnVector = M.unsafeFromList 2 1 [-1, -2]
    let firstFullSquareMatrix = M.unsafeFromList 2 2 [-2, 3, 6, 1]
    let secondFullSquareMatrix = M.unsafeFromList 2 2 [-1, -1, 5, 3]
    let firstFullRectangularMatrix = M.unsafeFromList 3 2 [1, 8, -5, 4, -4, 0]
    let secondFullRectangularMatrix = M.unsafeFromList 2 3 [0, -2, 3, 6, 1, -8]
    let firstDiagonalMatrix = M.unsafeFromList 2 2 [4, -1]
    let secondDiagonalMatrix = M.unsafeFromList 2 2 [5, 2]

    TH.describe "Test of linear algebra functions:" $ do
        TH.it "Equality between matrices with tolerance" $ do
            firstFullRectangularMatrix `TH.shouldSatisfy` M.equal 0 firstFullRectangularMatrix
            firstFullRectangularMatrix `TH.shouldSatisfy` not . M.equal 0 (M.transpose firstFullRectangularMatrix)
            firstFullRectangularMatrix `TH.shouldSatisfy` not . M.equal 0.001 (M.unsafeFromList 3 2 [1, 8, -5, 4.01, -4, 0])
            firstFullRectangularMatrix `TH.shouldSatisfy` M.equal 0.01 (M.unsafeFromList 3 2 [0.997, 8.0132, -5.05, 3.97, -4, 0])

        TH.it "Extraction of matrices" $ do
            M.take M.Columns 1 firstFullRectangularMatrix `TH.shouldBe` M.fromList 3 1 [1, -5, -4]
            M.take M.Rows 2 firstFullRectangularMatrix `TH.shouldBe` M.fromList 2 2 [1, 8, -5, 4]
            M.init M.Columns firstFullRectangularMatrix `TH.shouldBe` M.fromList 3 1 [1, -5, -4]
            M.init M.Rows firstFullRectangularMatrix `TH.shouldBe` M.fromList 2 2 [1, 8, -5, 4]
            M.take M.Columns 10 firstFullRectangularMatrix `TH.shouldBe` Right M.empty
            M.take M.Rows 10 firstFullRectangularMatrix `TH.shouldBe` Right M.empty

        TH.it "Transpose of matrices" $ do
            M.transpose firstFullRectangularMatrix `TH.shouldBe` M.unsafeFromList 2 3 [1, -5, -4, 8, 4, 0]
            M.transpose firstDiagonalMatrix `TH.shouldBe` firstDiagonalMatrix
            M.transpose firstColumnVector `TH.shouldBe` M.unsafeFromList 1 2 [6, -4]

        TH.it "Transpose of a matrix twice" $
            TQ.property $ \m -> M.transpose (M.transpose m) == (m :: M.Matrix)

        TH.it "Multiplication of a matrix and its transposed" $
            TQ.property $ \m -> DE.isRight (m `M.multiplyMatrices` M.transpose (m :: M.Matrix))

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

testNetwork :: TH.Spec
testNetwork = do
    let generator = SR.mkStdGen 12345
    let datapoint = M.unsafeFromList 6 1 [-2, 3, 6, 1, -8, -9]
    let network = N.unsafeFromLists [A.ReLu, A.ReLu, A.ReLu, A.ReLu] [
            M.unsafeFromList 3 1 [3, -2, 1],
            M.unsafeFromList 2 1 [6, -1],
            M.unsafeFromList 3 1 [-1, 2, 7],
            M.unsafeFromList 6 1 [4, 3, -7, 0, 1, -1]
            ] [
            M.unsafeFromList 3 6 [2, -1, 3, 7, -5, 1, 5, -1, 2, 1, 7, -3, 6, 4, -8, 1, -2, -4],
            M.unsafeFromList 2 3 [3, -1, 2, 2, -5, 1],
            M.unsafeFromList 3 2 [1, 1, -4, 8, 3, 2],
            M.unsafeFromList 6 3 [-7, 3, 2, -1, 4, -6, 6, -2, -1, 5, 2, 4, -1, 8, 3, 2, -4, 2]
            ]

    TH.describe "Test of network functions:" $ do
        TH.it "Generation of a random network" $ do
            N.random [3, 2, 1] [A.ReLu, A.TanH] generator `TH.shouldBe` N.fromLists [A.ReLu, A.TanH] [
                M.unsafeFromList 2 1 [1.9543818196252394e-2, -8.256066438750898e-2],
                M.unsafeFromList 1 1 [-0.24973890320215109]
                ] [
                M.unsafeFromList 2 3 [9.31527772916203e-2, -4.6601584116810146e-2, 0.7510221398598724, 0.6559757229088519, 0.8968734492868771, 0.9412527518865514],
                M.unsafeFromList 1 2 [-0.7812955190553257, 0.6031585052661881]
                ]
            N.random [3, 2] [A.ReLu, A.TanH] generator `TH.shouldSatisfy` DE.isLeft

        let forwardResult = N.forward datapoint network

        TH.it "Forward of an input" $
            forwardResult `TH.shouldBe` Right (N.ForwardResult
                (toColumnVector <$> [[52, -31, 6], [174, 109], [282, 178, 747], [58, -4049, 582, 4754, 3384, 1345]])
                (toColumnVector <$> [[-2, 3, 6, 1, -8, -9], [52, 0, 6], [174, 109], [282, 178, 747], [58, 0, 582, 4754, 3384, 1345]])
                )

        TH.it "Propagation of a gradient" $ do
            let backwardExpected = ([
                    M.unsafeFromList 3 1 [220107.015625, 0, 109957.0078125],
                    M.unsafeFromList 2 1 [-193.0009765625, 110343.0078125],
                    M.unsafeFromList 3 1 [8705.6669921875, 10084.66796875, 10480.0009765625],
                    M.unsafeFromList 6 1 [20, 0, 192, 1584.3333740234375, 1130.666748046875, 451.3333435058594]
                    ], [
                    M.unsafeFromList 3 6 [-440214.03125, 660321.0625, 1320642.125, 220107.015625, -1760856.125, -1980963.125, 0, 0, 0, 0, 0, 0, -219914.015625, 329871.03125, 659742.0625, 109957.0078125, -879656.0625, -989613.0625],
                    M.unsafeFromList 2 3 [-10036.05078125, 0, -1158.005859375, 5737836.5, 0, 662058.0625],
                    M.unsafeFromList 3 2 [1514786.0, 948917.6875, 1754732.25, 1099228.75, 1823520.125, 1142320.125],
                    M.unsafeFromList 6 3 [5640, 3560, 14940, 0, 0, 0, 54144, 34176, 143424, 446782, 282011.34375, 1183497, 318848.03125, 201258.6875, 844608.0625, 127276, 80337.3359375, 337146]
                    ])

            (forwardResult >>= N.backward L.MSE network datapoint) `TH.shouldSatisfy` (\backwardResult -> DE.fromRight False $ equalBackwardResults backwardExpected <$> backwardResult)

testUtils :: TH.Spec
testUtils = do
    let vector = DVU.fromList [-2, 3, 6, 1, -8, -9]

    TH.describe "Test of utility functions:" $
        TH.it "Chunk of a vector" $ do
            U.chunksOf 5 DVU.empty `TH.shouldBe` DV.empty
            U.chunksOf 2 vector `TH.shouldBe` DV.fromList (fmap DVU.fromList [[-2, 3], [6, 1], [-8, -9]])
            U.chunksOf 5 vector `TH.shouldBe` DV.fromList (fmap DVU.fromList [[-2, 3, 6, 1, -8]])


{----- MAIN -----}

main :: IO ()
main = TH.hspec $ do
    testUtils
    testMatrix
    testActivation
    testLosses
    testNetwork
