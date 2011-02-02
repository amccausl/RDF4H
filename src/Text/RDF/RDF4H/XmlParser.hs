{-# LANGUAGE Arrows, RankNTypes, FlexibleContexts #-}
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

-- |Global state for the parser
data GParseState = GParseState { stateGenId :: Int
                               }

-- |Local state for the parser (dependant on the parent xml elements)
data LParseState = LParseState { stateBaseUrl :: BaseUrl
                               , stateLang :: Maybe String
                               , stateSubject :: Subject
                               }

-- |Parse a xml ByteString to an RDF representation
parseXmlRDF :: forall rdf. (RDF rdf)
            => Maybe BaseUrl           -- ^ The base URL for the RDF if required
            -> Maybe ByteString        -- ^ DocUrl: The request URL for the RDF if available
            -> ByteString              -- ^ The contents to parse
            -> Either ParseFailure rdf -- ^ The RDF representation of the triples or ParseFailure
parseXmlRDF bUrl dUrl xmlStr = case runSLA (xread >>> addMetaData bUrl dUrl >>> getRDF) (GParseState { stateGenId = 0 }) (b2s xmlStr) of
                                (_,r:_) -> Right r
                                _ -> Left (ParseFailure "XML parsing failed")

-- |Add a root tag to a given XmlTree to appear as if it was read from a readDocument function
addMetaData :: (ArrowXml a) => Maybe BaseUrl -> Maybe ByteString -> a XmlTree XmlTree
addMetaData bUrlM dUrlM = mkelem "/"
                        ( [ sattr "transfer-Message" "OK"
                          , sattr "transfer-MimeType" "text/rdf"
                          ] ++ mkSource dUrlM ++ mkBase bUrlM
                        )
                        [ arr id ]
  where mkSource (Just dUrl) = [ sattr "source" (b2s dUrl) ]
        mkSource Nothing = []
        mkBase (Just (BaseUrl bUrl)) = [ sattr "transfer-URI" (b2s bUrl) ]
        mkBase Nothing = []

-- |Arrow that translates HXT XmlTree to an RDF representation
getRDF :: forall rdf a. (RDF rdf, ArrowXml a, ArrowState GParseState a) => a XmlTree rdf
getRDF = proc xml -> do
              rdf <- hasName "rdf:RDF" <<< isElem <<< getChildren         -< xml
              bUrl <- arr (BaseUrl . s2b) <<< ((getAttrValue0 "xml:base" <<< isElem <<< getChildren) `orElse` getAttrValue "transfer-URI") -< xml
              prefixMap <- arr toPrefixMap <<< toAttrMap                  -< rdf
              triples <- parseDescription' >. id -< (bUrl, rdf)
              returnA -< mkRdf triples (Just bUrl) prefixMap
  where toAttrMap = (getAttrl >>> (getName &&& (getChildren >>> getText))) >. id
        toPrefixMap = PrefixMappings . Map.fromList . map (\(n, m) -> (s2b (drop 6 n), s2b m)) . filter (startswith "xmlns:" . fst)

-- |Read the initial state from an rdf element
parseDescription' :: forall a. (ArrowXml a, ArrowState GParseState a) => a (BaseUrl, XmlTree) Triple
parseDescription' = proc (bUrl, rdf) -> do
                         desc <- isElem <<< getChildren -< rdf
                         state <- arr (\(s, o) -> s { stateSubject = o }) <<< arr fst &&& arr2A mkNode -< (LParseState bUrl Nothing undefined, desc)
                         triple <- parseDescription -< (state, desc)
                         returnA -< triple

-- |Read an rdf:Description tag to its corresponding Triples
parseDescription :: forall a. (ArrowXml a, ArrowState GParseState a) => a (LParseState, XmlTree) Triple
parseDescription = (updateState
               >>> (arr2A parsePredicatesFromAttr
                   <+> (second (getChildren >>> isElem) >>> parsePredicatesFromChildren)
                   <+> (second (neg (hasName "rdf:Description")) >>> arr2A readTypeTriple))) -- If the rdf:Description element has another name, that is it's type
               >>. (replaceLiElems [] (1 :: Int))
  where readTypeTriple :: forall a. (ArrowXml a, ArrowState GParseState a) => LParseState -> a XmlTree Triple
        readTypeTriple state = getName >>> arr ((Triple (stateSubject state) ((unode . s2b) "rdf:type")) . unode . s2b)
        replaceLiElems acc n (Triple s p o : rest) | p == (unode . s2b) "rdf:li" = replaceLiElems (Triple s ((unode . s2b) ("rdf:_" ++ show n)) o : acc) (n + 1) rest
        replaceLiElems acc n (Triple s p o : rest) = replaceLiElems (Triple s p o : acc) n rest
        replaceLiElems acc _ [] = acc

-- |Parse the current predicate element as a rdf:Description element (used when rdf:parseType = "Resource")
parseAsResource :: forall a. (ArrowXml a, ArrowState GParseState a) => Node -> a (LParseState, XmlTree) Triple
parseAsResource n = (updateState
               >>> (arr2A parsePredicatesFromAttr
                   <+> (second getName >>> arr (\(s, p) -> Triple (stateSubject s) ((unode . s2b) p) n))
                   <+> (arr (\s -> s { stateSubject = n }) *** (getChildren >>> isElem) >>> parsePredicatesFromChildren))) -- If the rdf:Description element has another name, that is it's type

-- |Read the attributes of an rdf:Description element.  These correspond to the Predicate Object pairs of the Triple
parsePredicatesFromAttr :: forall a. (ArrowXml a, ArrowState GParseState a) => LParseState -> a XmlTree Triple
parsePredicatesFromAttr s = getAttrl >>> ((getName >>> neg (isMetaAttr)
                                            >>> (arr (unode . s2b)))
                                        &&& (getChildren >>> getText >>> arr (lnode . plainL . s2b))) >>> arr (attachSubject (stateSubject s))

isMetaAttr :: forall a. (ArrowXml a, ArrowState GParseState a) => a String String
isMetaAttr = isA (== "rdf:about")
         <+> isA (== "rdf:nodeID")
         <+> isA (== "rdf:ID")
         <+> isA (== "xml:lang")
         <+> isA (== "rdf:parseType")

-- |Read a children of an rdf:Description element.  These correspond to the Predicate portion of the Triple
parsePredicatesFromChildren :: forall a. (ArrowXml a, ArrowState GParseState a) => a (LParseState, XmlTree) Triple
parsePredicatesFromChildren = updateState
                          >>> choiceA [ second (hasAttrValue "rdf:parseType" (== "Literal")) :-> arr2A parseAsLiteralTriple
                                      , second (hasAttrValue "rdf:parseType" (== "Resource")) :-> (defaultA
                                                                                               <+> (mkBlankNode &&& arr id >>> arr2A parseAsResource))
                                      , second (hasAttrValue "rdf:parseType" (== "Collection")) :-> (listA (defaultA >>> arr id &&& mkBlankNode) >>> mkCollectionTriples >>> unlistA)
                                      , second (hasAttr "rdf:datatype") :-> arr2A getTypedTriple
                                      , second (hasAttr "rdf:resource") :-> arr2A getResourceTriple
                                      , second (hasAttr "rdf:nodeID") :-> arr2A getNodeIdTriple
                                      , second (hasAttr "rdf:ID") :-> (arr2A mkRelativeNode &&& defaultA >>> arr2A reifyTriple >>> unlistA)
                                      , this :-> (defaultA
                                              <+> ((second getAttrl &&& (neg . second) (getAttrl >>> getName >>> isMetaAttr)) `guards` (mkBlankNode &&& arr id >>> arr2A parsePredicateAttr)))
                                      ]
  where defaultA = proc (state, predXml) -> do
                         p <- arr(unode . s2b) <<< getName -< predXml
                         t <- arr2A (\s -> arr2A (parseObjectsFromChildren s)) <<< second (second getChildren) -< (state, (p, predXml))
                         returnA -< t
        parsePredicateAttr n = (second getName >>> arr (\(s, p) -> Triple (stateSubject s) ((unode . s2b) p) n))
                           <+> (first (arr (\s -> s { stateSubject = n })) >>> arr2A parsePredicatesFromAttr)

parseObjectsFromChildren :: forall a. (ArrowXml a, ArrowState GParseState a)
                         => LParseState -> Predicate -> a XmlTree Triple
parseObjectsFromChildren s p = (isText >>> getText >>> arr ((Triple (stateSubject s) p) . mkLiteralNode s))
                           <+> (isElem >>> hasName "rdf:Description" >>> parseObjectDescription) -- TODO: include ability to alias for rdf:type
  where parseObjectDescription = proc desc -> do
                                      o <- mkNode s -< desc
                                      t0 <- arr (\(sub, (p, o)) -> Triple sub p o) -< (stateSubject s, (p, o))
                                      t <- arr fst <+> (parseDescription <<< arr snd) -< (t0, (s { stateSubject = o }, desc))
                                      returnA -< t

attachSubject :: Subject -> (Predicate, Object) -> Triple
attachSubject s (p, o) = Triple s p o

reifyTriple :: forall a. (ArrowXml a, ArrowState GParseState a) => Subject -> a Triple Triples
reifyTriple node = arr (\(Triple s p o) -> [ Triple s p o
                                           , Triple node rdfType rdfStatement
                                           , Triple node rdfSubject s
                                           , Triple node rdfPredicate p
                                           , Triple node rdfObject o
                                           ])
  where rdfType = (unode . s2b) "rdf:type"
        rdfStatement = (unode . s2b) "rdf:Statement"
        rdfSubject = (unode . s2b) "rdf:subject"
        rdfPredicate = (unode . s2b) "rdf:predicate"
        rdfObject = (unode . s2b) "rdf:object"

-- |Updates the local state at a given node
updateState :: forall a. (ArrowXml a, ArrowState GParseState a)
            => a (LParseState, XmlTree) (LParseState, XmlTree)
updateState = (ifA (second (hasAttr "xml:lang")) (arr2A readLang) (arr id))
          >>> (ifA (second (hasAttr "xml:base")) (arr2A readBase) (arr id))
  where readLang state = (getAttrValue0 "xml:lang" >>> arr (\lang -> state { stateLang = Just lang } ) ) &&& arr id
        readBase state = (getAttrValue0 "xml:base" >>> arr (\base -> state { stateBaseUrl = (BaseUrl . s2b) base } ) ) &&& arr id

-- |Read a Triple with an rdf:parseType of Literal
parseAsLiteralTriple :: forall a. (ArrowXml a, ArrowState GParseState a) => LParseState -> a XmlTree Triple
parseAsLiteralTriple state = ((getName >>> arr (unode . s2b)) &&& (xshow ( getChildren ) >>> arr (mkTypedLiteralNode state nodeType))) >>> arr (attachSubject (stateSubject state))
  where nodeType = mkFastString (s2b "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral")

mkCollectionTriples :: forall a. (ArrowXml a, ArrowState GParseState a) => a [(Triple, Node)] Triples
mkCollectionTriples = arr (mkCollectionTriples' [])
  where mkCollectionTriples' [] ((Triple s1 p1 o1, n1):rest) = mkCollectionTriples' [Triple s1 p1 n1] ((Triple s1 p1 o1, n1):rest)
        mkCollectionTriples' acc ((Triple _ _ o1, n1):(t2, n2):rest) = mkCollectionTriples' (Triple n1 headNode o1 : Triple n1 tailNode n2 : acc) ((t2, n2):rest)
        mkCollectionTriples' acc [(Triple _ _ o1, n1)] = Triple n1 headNode o1 : Triple n1 tailNode nilNode : acc
        mkCollectionTriples' _ [] = []
        headNode = (unode . s2b) "rdf:first"
        tailNode = (unode . s2b) "rdf:rest"
        nilNode = (unode . s2b) "rdf:nil"

-- |Read a Triple and it's type when rdf:datatype is available
getTypedTriple :: forall a. (ArrowXml a, ArrowState GParseState a) => LParseState -> a XmlTree Triple
getTypedTriple state = ((getName >>> arr (unode . s2b)) &&& ((getAttrValue "rdf:datatype" &&& baseUrl >>> expandURI) &&& xshow (getChildren) >>> arr (\(t, v) -> lnode (typedL (s2b v) (mkFastString (s2b t)))))) >>> arr (attachSubject (stateSubject state))
  where baseUrl = constA (case stateBaseUrl state of BaseUrl b -> b2s b)

getResourceTriple :: forall a. (ArrowXml a, ArrowState GParseState a) => LParseState -> a XmlTree Triple
getResourceTriple state = ((getName >>> arr (unode . s2b)) &&& ((getAttrValue "rdf:resource" &&& baseUrl) >>> expandURI >>> arr (unode . s2b))) >>> arr (attachSubject (stateSubject state))
  where baseUrl = constA (case stateBaseUrl state of BaseUrl b -> b2s b)

getNodeIdTriple :: forall a. (ArrowXml a, ArrowState GParseState a) => LParseState -> a XmlTree Triple
getNodeIdTriple state = ((getName >>> arr (unode . s2b)) &&& (getAttrValue "rdf:nodeID" >>> arr (bnode . s2b))) >>> arr (attachSubject (stateSubject state))

-- |Read a Node from the "rdf:about" property or generate a blank node
mkNode :: forall a. (ArrowXml a, ArrowState GParseState a) => LParseState -> a XmlTree Node
mkNode s = choiceA [ hasAttr "rdf:about" :-> (getAttrValue "rdf:about" &&& baseUrl >>> expandURI >>> arr (unode . s2b))
                   , hasAttr "rdf:resource" :-> (getAttrValue "rdf:resource" &&& baseUrl >>> expandURI >>> arr (unode . s2b))
                   , hasAttr "rdf:nodeID" :-> (getAttrValue "rdf:nodeID" >>> arr (bnode . s2b))
                   , hasAttr "rdf:ID" :-> mkRelativeNode s
                   , this :-> mkBlankNode
                   ]
  where baseUrl = constA (case stateBaseUrl s of BaseUrl b -> b2s b)

--mkRelativeNode :: forall a. (ArrowXml a, ArrowState GParseState a) => LParseState -> a XmlTree Node
mkRelativeNode s = (getAttrValue "rdf:ID" >>> arr (\x -> '#':x)) &&& baseUrl >>> expandURI >>> arr (unode . s2b)
  where baseUrl = constA (case stateBaseUrl s of BaseUrl b -> b2s b)

mkTypedLiteralNode :: LParseState -> FastString -> String -> Node
mkTypedLiteralNode (LParseState _ _ _) t content = lnode (typedL (s2b content) t)

-- |Use the given state to create a literal node
mkLiteralNode :: LParseState -> String -> Node
mkLiteralNode (LParseState _ (Just lang) _) content = (lnode (plainLL (s2b content) (s2b lang)))
mkLiteralNode (LParseState _ Nothing _) content = (lnode . plainL . s2b) content

-- |Generate an RDF blank node with incrementing IDs from the arrow state
mkBlankNode :: forall a b. (ArrowState GParseState a) => a b Node
mkBlankNode = nextState (\gState -> gState { stateGenId = stateGenId gState + 1 } ) >>> arr (BNodeGen . stateGenId)

