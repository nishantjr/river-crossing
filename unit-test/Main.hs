import Test.Tasty
import Test.Tasty.HUnit

basic = testGroup "Basic"
    [ testCase "two plus two" $
        assertEqual "" (2 + 2) 4
    ]

main :: IO ()
main = defaultMain $ testGroup "Tests" [basic]

