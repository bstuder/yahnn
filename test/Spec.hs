import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Utils (chunksOf)
import qualified Matrix as M
import qualified Data.Vector as V

testChunk = do
    it "no chunk" $
        chunksOf (-6) [1..6] `shouldBe` []
    it "even chunks" $
        chunksOf 2 [1..6] `shouldBe` [[1,2],[3,4],[5,6]]
    it "uneven chunks" $
        chunksOf 3 [1..8] `shouldBe` [[1,2,3],[4,5,6],[7,8]]

mat = M.Matrix 2 3 (V.fromList [1..6])

testMatrix = do
    it "sum rows" $
        M.applyRow sum mat `shouldBe` V.fromList [6, 15]

main :: IO ()
main = hspec $ testChunk >> testMatrix
