import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Utils (chunksOf)

testChunk = do
    it "no chunk" $
        chunksOf (-6) [1..6] `shouldBe` []
    it "even chunks" $
        chunksOf 2 [1..6] `shouldBe` [[1,2],[3,4],[5,6]]
    it "uneven chunks" $
        chunksOf 3 [1..8] `shouldBe` [[1,2,3],[4,5,6],[7,8]]

main :: IO ()
main = hspec $ testChunk
