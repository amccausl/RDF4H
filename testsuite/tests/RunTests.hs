
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import qualified Text.RDF.RDF4H.XmlParser_Test as XmlParser

main = defaultMain ( XmlParser.tests )

