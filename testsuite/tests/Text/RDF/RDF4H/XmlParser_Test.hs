{-# LANGUAGE ScopedTypeVariables #-}

module Text.RDF.RDF4H.XmlParser_Test where

-- Testing imports
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

-- Import common libraries to facilitate tests
import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Time
import Data.Ranged
import Locale
import Network.HTTP (Request(..))

import Data.RDF
import Data.RDF.Namespace (mkPrefixedNS')
import Data.RDF.TriplesGraph (TriplesGraph(..))
import Data.RDF.TriplesGraph_Test

import Text.RDF.RDF4H.XmlParser

tests = [ testGroup "XmlParser:parseXmlRDF" [ testCase "simpleStriping" test_simpleStriping
                                            , testCase "example07" test_parseXmlRDF_example07
--                                            , testCase "example08" test_parseXmlRDF_example08
--                                            , testCase "example09" test_parseXmlRDF_example09
--                                            , testCase "example10" test_parseXmlRDF_example10
--                                            , testCase "example11" test_parseXmlRDF_example11
--                                            , testCase "example12" test_parseXmlRDF_example12
--                                            , testCase "example13" test_parseXmlRDF_example13
--                                            , testCase "example14" test_parseXmlRDF_example14
--                                            , testCase "example15" test_parseXmlRDF_example15
--                                            , testCase "example16" test_parseXmlRDF_example16
--                                            , testCase "example17" test_parseXmlRDF_example17
--                                            , testCase "example18" test_parseXmlRDF_example18
--                                            , testCase "example19" test_parseXmlRDF_example19
--                                            , testCase "example20" test_parseXmlRDF_example20
--                                            , testCase "example21" test_parseXmlRDF_example21
                                            ]
        ]

mkTextNode = lnode . plainL . s2b
testParse exRDF ex = assertBool ("expected: " ++ show ex ++ "but got: " ++ show parsed) (isIsomorphic (parsed :: TriplesGraph) (ex :: TriplesGraph))
  where parsed = case parseXmlRDF Nothing Nothing (s2b exRDF) of Right result -> result

test_simpleStriping = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\
      \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\">\
        \<dc:title>RDF/XML Syntax Specification (Revised)</dc:title>\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple ((unode . s2b) "http://www.w3.org/TR/rdf-syntax-grammar") ((unode . s2b) "dc:title") (mkTextNode "RDF/XML Syntax Specification (Revised)") ]
            Nothing
            ( PrefixMappings (Map.fromList [ (s2b "dc", s2b "http://purl.org/dc/elements/1.1/")
                                           , (s2b "rdf", s2b "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

example07 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
                    \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\
                    \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
              \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\"\
                              \ dc:title=\"RDF/XML Syntax Specification (Revised)\">\
                \<ex:editor>\
                  \<rdf:Description ex:fullName=\"Dave Beckett\">\
                    \<ex:homePage rdf:resource=\"http://purl.org/net/dajobe/\" />\
                  \</rdf:Description>\
                \</ex:editor>\
              \</rdf:Description>\
            \</rdf:RDF>"

test_parseXmlRDF_example07 = testParse example07
  ( mkRdf [ Triple ((unode . s2b) "http://www.w3.org/TR/rdf-syntax-grammar") ((unode . s2b) "dc:title") (mkTextNode "RDF/XML Syntax Specification (Revised)")
          , Triple (BNodeGen 1) ((unode . s2b) "ex:fullName") (mkTextNode "Dave Beckett")
          , Triple (BNodeGen 1) ((unode . s2b) "ex:homePage") ((unode . s2b) "http://purl.org/net/dajobe/")
          , Triple ((unode . s2b) "http://www.w3.org/TR/rdf-syntax-grammar") ((unode . s2b) "ex:editor") (BNodeGen 1)
          ]
          Nothing
          ( PrefixMappings (Map.fromList [ (s2b "dc", s2b "http://purl.org/dc/elements/1.1/")
                                         , (s2b "ex", s2b "http://example.org/stuff/1.0/")
                                         , (s2b "rdf", s2b "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
  )

example08 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
                    \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\
              \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\">\
                \<dc:title>RDF/XML Syntax Specification (Revised)</dc:title>\
                \<dc:title xml:lang=\"en\">RDF/XML Syntax Specification (Revised)</dc:title>\
                \<dc:title xml:lang=\"en-US\">RDF/XML Syntax Specification (Revised)</dc:title>\
              \</rdf:Description>\
              \<rdf:Description rdf:about=\"http://example.org/buecher/baum\" xml:lang=\"de\">\
                \<dc:title>Der Baum</dc:title>\
                \<dc:description>Das Buch ist außergewöhnlich</dc:description>\
                \<dc:title xml:lang=\"en\">The Tree</dc:title>\
              \</rdf:Description>\
            \</rdf:RDF>"

--test_parseXmlRDF_example08 = testParse example08
--  ( mkRdf [ Triple ((unode . s2b) "http://www.w3.org/TR/rdf-syntax-grammar") ((unode . s2b) "dc:title") ((lnode . plainL . s2b) "RDF/XML Syntax Specification (Revised)")

example09 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
                    \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
              \<rdf:Description rdf:about=\"http://example.org/item01\">\
                \<ex:prop rdf:parseType=\"Literal\"\
                        \ xmlns:a=\"http://example.org/a#\"><a:Box required=\"true\">\
                  \<a:widget size=\"10\" />\
                  \<a:grommit id=\"23\" /></a:Box>\
                \</ex:prop>\
              \</rdf:Description>\
            \</rdf:RDF>"

example10 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
                    \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
              \<rdf:Description rdf:about=\"http://example.org/item01\">\
                \<ex:size rdf:datatype=\"http://www.w3.org/2001/XMLSchema#int\">123</ex:size>\
              \</rdf:Description>\
            \</rdf:RDF>"

example11 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
                    \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\
                    \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
              \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\"\
		                      \ dc:title=\"RDF/XML Syntax Specification (Revised)\">\
                \<ex:editor rdf:nodeID=\"abc\"/>\
              \</rdf:Description>\
              \<rdf:Description rdf:nodeID=\"abc\"\
                              \ ex:fullName=\"Dave Beckett\">\
                \<ex:homePage rdf:resource=\"http://purl.org/net/dajobe/\"/>\
              \</rdf:Description>\
            \</rdf:RDF>"

example12 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
                    \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\
                    \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
              \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\"\
		                      \ dc:title=\"RDF/XML Syntax Specification (Revised)\">\
                \<ex:editor rdf:parseType=\"Resource\">\
                  \<ex:fullName>Dave Beckett</ex:fullName>\
                  \<ex:homePage rdf:resource=\"http://purl.org/net/dajobe/\"/>\
                \</ex:editor>\
              \</rdf:Description>\
            \</rdf:RDF>"

example13 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
                    \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\
                    \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
              \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\"\
		                      \ dc:title=\"RDF/XML Syntax Specification (Revised)\">\
                \<ex:editor ex:fullName=\"Dave Beckett\" />\
              \</rdf:Description>\
            \</rdf:RDF>"

example14 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
                    \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\
                    \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
              \<rdf:Description rdf:about=\"http://example.org/thing\">\
                \<rdf:type rdf:resource=\"http://example.org/stuff/1.0/Document\"/>\
                \<dc:title>A marvelous thing</dc:title>\
              \</rdf:Description>\
            \</rdf:RDF>"

example15 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
                    \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\
                    \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
              \<ex:Document rdf:about=\"http://example.org/thing\">\
                \<dc:title>A marvelous thing</dc:title>\
              \</ex:Document>\
            \</rdf:RDF>"

example16 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
                    \ xmlns:ex=\"http://example.org/stuff/1.0/\"\
                    \ xml:base=\"http://example.org/here/\">\
              \<rdf:Description rdf:ID=\"snack\">\
                \<ex:prop rdf:resource=\"fruit/apple\"/>\
              \</rdf:Description>\
            \</rdf:RDF>"

example17 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\
              \<rdf:Seq rdf:about=\"http://example.org/favourite-fruit\">\
                \<rdf:_1 rdf:resource=\"http://example.org/banana\"/>\
                \<rdf:_2 rdf:resource=\"http://example.org/apple\"/>\
                \<rdf:_3 rdf:resource=\"http://example.org/pear\"/>\
              \</rdf:Seq>\
            \</rdf:RDF>"

example18 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\
              \<rdf:Seq rdf:about=\"http://example.org/favourite-fruit\">\
                \<rdf:li rdf:resource=\"http://example.org/banana\"/>\
                \<rdf:li rdf:resource=\"http://example.org/apple\"/>\
                \<rdf:li rdf:resource=\"http://example.org/pear\"/>\
              \</rdf:Seq>\
            \</rdf:RDF>"

example19 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
                    \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
              \<rdf:Description rdf:about=\"http://example.org/basket\">\
                \<ex:hasFruit rdf:parseType=\"Collection\">\
                  \<rdf:Description rdf:about=\"http://example.org/banana\"/>\
                  \<rdf:Description rdf:about=\"http://example.org/apple\"/>\
                  \<rdf:Description rdf:about=\"http://example.org/pear\"/>\
                \</ex:hasFruit>\
              \</rdf:Description>\
            \</rdf:RDF>"

example20 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
                    \ xmlns:ex=\"http://example.org/stuff/1.0/\"\
                    \ xml:base=\"http://example.org/triples/\">\
              \<rdf:Description rdf:about=\"http://example.org/\">\
                \<ex:prop rdf:ID=\"triple1\">blah</ex:prop>\
              \</rdf:Description>\
            \</rdf:RDF>"

example21 = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
                    \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\
              \<rdf:Description rdf:about=\"example21.html\">\
                \<dc:title>My document</dc:title>\
                \<dc:description>A document about various things.</dc:description>\
                \<dc:creator>The author</dc:creator>\
              \</rdf:Description>\
            \</rdf:RDF>"


