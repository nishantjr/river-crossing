import Test.Tasty
import Test.Tasty.HUnit

unitTests = testGroup "Unit tests"
    [ testCase "Falsity" $
        assertEqual "Falsity" (2 + 2) 5
    ]

main :: IO ()
main = defaultMain $ testGroup "Tests" [unitTests]


