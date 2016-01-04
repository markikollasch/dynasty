import Test.Framework (defaultMain)
import Domain.Zone.Tests

main :: IO ()
main = defaultMain
    [ Domain.Zone.Tests.tests
    ]
