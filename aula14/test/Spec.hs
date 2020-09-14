import Data.JSONLib

import Test.Tasty
import Test.Tasty.QuickCheck



roundTrip :: Property
roundTrip
  = forAll (arbitrary :: Gen JSON)
       (\ json ->
          let
            str = prettyJSON json
          in case parseJSON str of
               Left _ -> False
               Right json' -> json == json')


tests :: TestTree
tests = testGroup "Library tests"
                  [
                    testProperty "Round trip" roundTrip
                  ]

main :: IO ()
main = defaultMain tests
