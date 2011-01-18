{-# LANGUAGE Arrows, RankNTypes #-}
-- |An parser for the RDF/XML format 
-- <http://www.w3.org/TR/REC-rdf-syntax/>.

module Text.RDF.RDF4H.XmlParser(
  parseXmlRDF, getRDF
) where

import qualified Data.Map as Map
import Data.RDF

import Control.Arrow

import Text.XML.HXT.Core

import Data.ByteString.Lazy.Char8(ByteString)
import Data.String.Utils

-- |Parse a xml ByteString to an RDF representation
parseXmlRDF :: forall rdf. (RDF rdf)
            => Maybe BaseUrl           -- ^ The base URL for the RDF if required
            -> Maybe ByteString        -- ^ DocUrl: The request URL for the RDF if available
            -> ByteString              -- ^ The contents to parse
            -> Either ParseFailure rdf -- ^ The RDF representation of the triples or ParseFailure
parseXmlRDF bUrl dUrl xmlStr = Left (ParseFailure "XML parsing not implemented")
  where xml = runLA xread (b2s xmlStr)

-- |Arrow that translates HXT XmlTree to an RDF representation
getRDF :: forall rdf a. (RDF rdf, ArrowXml a) => a XmlTree rdf
getRDF = proc xml -> do
              rdf <- hasName "rdf:RDF" <<< isElem <<< getChildren                -< xml
              bUrl <- arr (Just . BaseUrl . s2b) <<< getAttrValue "transfer-URI" -< xml
              prefixMap <- arr toPrefixMap <<< toAttrMap                         -< rdf
              triples <- (parseDescription <<< hasName "rdf:Description" <<< isElem <<< getChildren) >. id -< rdf
              returnA -< mkRdf triples bUrl prefixMap
  where toAttrMap = (getAttrl >>> (getName &&& (getChildren >>> getText))) >. id
        toPrefixMap = PrefixMappings . Map.fromList . map (\(n, m) -> (s2b (drop 6 n), s2b m)) . filter (startswith "xmlns:" . fst)
        toUNode = unode . s2b
        parseDescription = proc desc -> do
                                s <- getAttrValue "rdf:about" -< desc
                                (p, o) <- ((getName >>> isA (/= "rdf:about")) &&& (getChildren >>> getText)) <<< getAttrl -< desc
                                returnA -< Triple (toUNode s) (toUNode p) (toUNode o)

fakeDoc :: ArrowXml a => a XmlTree XmlTree
fakeDoc = mkelem "/"
            [ sattr "source" "../../data/xml/example07.rdf"
            , sattr "transfer-URI" "http://www.w3.org/TR/REC-rdf-syntax/example07.rdf"
            , sattr "transfer-Message" "OK"
            , sattr "transfer-Status" "200"
            , sattr "transfer-MimeType" "text/rdf"
            , sattr "transfer-Encoding" "ISO-8859-1"
            ]
            [ arr id ]

