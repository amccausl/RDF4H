module Text.RDF.RDF4H.Namespace(
  Namespace, makePlainNS, makePrefixedNS, makePrefixedNS',
  PrefixMapping(PrefixMapping), PrefixMappings(PrefixMappings), toPMList,
  mergePrefixMappings,
  makeUri,
  prefixOf, uriOf,
  rdf, rdfs, dc, dct, owl, xsd, skos, foaf, ex, ex2, standard_ns_mappings
)
where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.ByteString.Lazy.Char8(ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

standard_namespaces :: [Namespace]
standard_namespaces = [rdf, rdfs, dc, dct, owl, xsd, skos, foaf, ex, ex2]

standard_ns_mappings  :: PrefixMappings
standard_ns_mappings  =  PrefixMappings $ Map.fromList $ 
                         map (\(PrefixedNS pre uri) -> (pre, uri)) standard_namespaces

p :: String -> ByteString
p = B.pack

-- Standard namespaces defined here for convenience:
-- |The RDF namespace.
rdf  :: Namespace
rdf   =   makePrefixedNS' "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

-- |The RDF Schema namespace.
rdfs :: Namespace
rdfs  =   makePrefixedNS'  "rdfs"  "http://www.w3.org/2000/01/rdf-schema#"

-- |The Dublic Core namespace.
dc   :: Namespace
dc    =   makePrefixedNS'  "dc"    "http://purl.org/dc/elements/1.1/"

-- |The Dublin Core terms namespace.
dct  :: Namespace
dct   =   makePrefixedNS'  "dct"    "http://purl.org/dc/terms/"

-- |The OWL namespace.
owl  :: Namespace
owl   =   makePrefixedNS'  "owl"   "http://www.w3.org/2002/07/owl#"

-- |The XML Schema namespace.
xsd  :: Namespace
xsd   =   makePrefixedNS'  "xsd"   "http://www.w3.org/2001/XMLSchema#"

-- |The SKOS namespace.
skos :: Namespace
skos  =   makePrefixedNS'  "skos"  "http://www.w3.org/2004/02/skos/core#"

-- |The friend of a friend namespace.
foaf :: Namespace
foaf  =   makePrefixedNS'  "foaf"  "http://xmlns.com/foaf/0.1/"

-- |Example namespace #1.
ex   :: Namespace
ex    =   makePrefixedNS'  "ex"    "http://www.example.org/"

-- |Example namespace #2.
ex2  :: Namespace
ex2   =   makePrefixedNS'  "ex2"   "http://www2.example.org/"

-- |An alias for a set of prefix mappings.
newtype PrefixMappings   = PrefixMappings (Map ByteString ByteString)
  deriving (Eq, Ord, Show)

-- |Perform a left-biased merge of the two sets of prefix mappings.
mergePrefixMappings :: PrefixMappings -> PrefixMappings -> PrefixMappings
mergePrefixMappings (PrefixMappings p1s) (PrefixMappings p2s) = 
  PrefixMappings $ Map.union p1s p2s

-- |View the prefix mappings as a list of key-value pairs. The PM in
-- in the name is to reduce name clashes if used without qualifying.
toPMList :: PrefixMappings -> [(ByteString, ByteString)]
toPMList (PrefixMappings m) = Map.toList m

-- |A mapping of a prefix to the URI for that prefix.
newtype PrefixMapping = PrefixMapping (ByteString, ByteString)
  deriving (Eq, Ord, Show)

-- |Make a URI consisting of the given namespace and the given localname.
makeUri :: Namespace -> ByteString -> ByteString
makeUri ns local = uriOf ns `B.append` local

-- |Represents a namespace as either a prefix and uri, respectively,
--  or just a uri.
data Namespace = PrefixedNS  ByteString ByteString -- prefix and ns uri
               | PlainNS     ByteString        -- ns uri alone

-- |Make a namespace for the given URI reference.
makePlainNS     ::  ByteString -> Namespace
makePlainNS       =  PlainNS

-- |Make a namespace having the given prefix for the given URI reference,
-- respectively.
makePrefixedNS  :: ByteString -> ByteString -> Namespace
makePrefixedNS    =  PrefixedNS

-- |Make a namespace having the given prefix for the given URI reference,
-- respectively, using strings which will be converted to bytestrings
-- automatically.
makePrefixedNS' :: String -> String -> Namespace
makePrefixedNS' s1 s2 = makePrefixedNS (p s1) (p s2)

instance Eq Namespace where
  (PrefixedNS _ u1) == (PrefixedNS _ u2)  = u1 == u2
  (PlainNS      u1) == (PlainNS      u2)  = u1 == u2
  (PrefixedNS _ u1) == (PlainNS      u2)  = u1 == u2
  (PlainNS      u1) == (PrefixedNS _ u2)  = u1 == u2

instance Show Namespace where
  show (PlainNS           uri)  =  show uri
  show (PrefixedNS prefix uri)  =  show (prefix, uri)

-- |Determine the URI of the given namespace.
uriOf     ::  Namespace -> ByteString
uriOf    (PlainNS      uri)  = uri
uriOf    (PrefixedNS _ uri)  = uri

-- |Determine the prefix of the given namespace, if it has one.
prefixOf  ::  Namespace -> Maybe ByteString
prefixOf (PlainNS      _)    = Nothing
prefixOf (PrefixedNS p _)    = Just p