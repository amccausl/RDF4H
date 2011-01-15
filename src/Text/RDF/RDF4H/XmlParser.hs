{-# LANGUAGE RankNTypes #-}
-- |An parser for the RDF/XML format 
-- <http://www.w3.org/TR/REC-rdf-syntax/>.

module Text.RDF.RDF4H.XmlParser(
  parseXmlRDF
) where

import Data.RDF
import Data.RDF.Namespace

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs (NTree(..))

import Data.ByteString.Lazy.Char8(ByteString)

parseXmlRDF :: forall rdf. (RDF rdf)
            => Maybe BaseUrl           -- ^ The base URL for the RDF if required
            -> Maybe ByteString        -- ^ DocUrl: The request URL for the RDF if available
            -> ByteString              -- ^ The contents to parse
            -> Either ParseFailure rdf -- ^ The RDF representation of the triples or ParseFailure
parseXmlRDF bUrl dUrl xmlStr = Left (ParseFailure "XML parsing not implemented")

