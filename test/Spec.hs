import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Utils (chunksOf)
import qualified Matrix as M
import qualified Data.Vector as V (empty, fromList, Vector(..))

vec = V.fromList [1..8]

testChunk = do
    it "empty vector" $
        chunksOf 5 (V.empty :: V.Vector Int) `shouldBe` V.empty
    it "even chunks" $
        chunksOf 2 vec `shouldBe` V.fromList (fmap V.fromList [[1, 2], [3, 4], [5, 6], [7, 8]])
    it "uneven chunks" $
        chunksOf 3 vec `shouldBe` V.fromList (fmap V.fromList [[1, 2, 3], [4, 5, 6], [7, 8]])

mat = M.Matrix 2 4 vec

testMatrix = do
    it "sum rows" $
        M.applyRow sum mat `shouldBe` V.fromList [10, 26]

main :: IO ()
main = hspec $ testChunk >> testMatrix
