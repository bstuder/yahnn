import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Utils (chunksOf)
import Activation (reLu, sign)
import qualified Matrix as M
import qualified Data.Vector as V (empty, fromList, Vector(..))

vec = V.fromList [-2, 3, 6, 1, -8, -9, 3, -5]

testChunk = do
    it "Empty vector" $
        chunksOf 5 (V.empty :: V.Vector Int) `shouldBe` V.empty
    it "Even chunks" $
        chunksOf 2 vec `shouldBe` V.fromList (fmap V.fromList [[-2, 3], [6, 1], [-8, -9], [3, -5]])
    it "Uneven chunks" $
        chunksOf 3 vec `shouldBe` V.fromList (fmap V.fromList [[-2, 3, 6], [1, -8, -9], [3, -5]])

mat = M.Matrix 2 4 vec

testMatrix = do
    it "Sum rows" $
        M.applyRow sum mat `shouldBe` V.fromList [8, -19]

testActivation = do
    it "Activation sign" $
        sign mat `shouldBe` V.fromList [1, -1]
    it "Activation ReLu" $
        reLu mat `shouldBe` V.fromList [8, 0]

main :: IO ()
main = hspec $ testChunk >> testMatrix >> testActivation
