-- test/Spec.hs
import Test.HUnit
import CoreLogic
import Types
import qualified Data.Text as T

main :: IO ()
main = runTestTTAndExit tests

wordCount :: T.Text -> Int
wordCount = length . T.words

tests :: Test
tests = TestList
  [ "empty text yields empty features" ~:
      extractFeaturesFromText "empty.txt" (T.pack "") ~?= BookFeatures "empty.txt" 0 0 0 0
  , "word count test" ~:
      let txt = T.pack "Hello world. Hello again."
      in wordCount txt ~?= 4
  ]
