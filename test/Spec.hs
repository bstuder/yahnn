import qualified Activation as A
import qualified Data.Either as E
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
    T.describe "Test of linear algebra functions:" $ do
        T.it "Sums rows of a matrix" $
            M.applyRow sum matrix `T.shouldBe` V.fromList [8, -19]
        T.it "Multiplies two vectors" $
            M.fromVectors vector (V.fromList [-1, 5]) `T.shouldBe` M.Matrix 8 2 (V.fromList [2, -10, -3, 15, -6, 30, -1, 5, 8, -40, 9, -45, -3, 15, 5, -25])
        T.it "Multiplies a vector with a matrix of matching dimensions" $
            M.multiplyVectorL (V.fromList [-1, 5]) matrix `T.shouldBe` E.Right (V.fromList [-38, -48, 9, -26])
        T.it "Multiplies a vector with a matrix of mismatching dimensions" $
            M.multiplyVectorL vector matrix `T.shouldSatisfy` E.isLeft
        T.it "Multiplies a matrix with a vector of matching dimensions" $
            M.multiplyVectorR matrix (V.fromList [0, -6, 3, 1]) `T.shouldBe` E.Right (V.fromList [1, 58])
        T.it "Multiplies a matrix with a vector of mismatching dimensions" $
            M.multiplyVectorR matrix vector `T.shouldSatisfy` E.isLeft
        T.it "Transposes a matrix" $
            M.transpose matrix `T.shouldBe` M.Matrix 4 2 (V.fromList [-2, -8, 3, -9, 6, 3, 1, -5])
        T.it "Transposes several matrices twice" $
            TQ.property $ \m -> M.transpose (M.transpose m) == (m :: M.Matrix Int)
        T.it "Multiplies two matrices with mismatching dimensions" $
            matrix `M.multiplyMatrices` M.Matrix 2 2 (V.fromList [1, 1, 1, 1]) `T.shouldSatisfy` E.isLeft
        T.it "Multiplies two matrices with matching dimensions" $
            matrix `M.multiplyMatrices` M.Matrix 4 3 (V.fromList [-6..5]) `T.shouldBe` E.Right (M.Matrix 2 3 (V.fromList [6, 14, 22, 60, 41, 22]))
        T.it "Multiplies several matrices by their transpose" $
            TQ.property $ \m -> E.isRight (m `M.multiplyMatrices` M.transpose (m :: M.Matrix Double))

testActivation =
    T.describe "Test of activation functions:" $ do
        T.it "Forwards a ReLu activation" $
            A.forward A.ReLu vector `T.shouldBe` V.fromList [0, 3, 6, 1, 0, 0, 3, 0]
        T.it "Derivates a ReLu activation" $
            A.derivate A.ReLu vector `T.shouldBe` V.fromList [0, 1, 1, 1, 0, 0, 1, 0]

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
                (V.fromList <$> [[-1, -3, -2],[0, 8], [0]])
           )

{-testBackWardStep =-}
    {-T.describe "Test of the backward propagation step" $ do-}
        {-T.it "step 1" $ -}
            {-N.backwardStep (V.fromList [1, 2]) A.ReLu matrix (V.fromList [2, 5]) (V.fromList [-1, 10]) (V.fromList [15, 7]) `T.shouldBe` E.Left "test"-}

main :: IO ()
main = T.hspec $ do
    testUtils
    testMatrix
    testActivation
    testNetwork
   {-testBackWardStep-}
