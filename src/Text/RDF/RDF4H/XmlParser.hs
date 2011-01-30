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
data LParseState = LParseState { stateBaseUrl :: Maybe BaseUrl
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
                                (_,r:rest) -> Right r
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
              rdf <- hasName "rdf:RDF" <<< isElem <<< getChildren                -< xml
              bUrl <- arr (Just . BaseUrl . s2b) <<< getAttrValue "transfer-URI" -< xml
              prefixMap <- arr toPrefixMap <<< toAttrMap                         -< rdf
              desc <- isElem <<< getChildren                                     -< rdf
              state <- arr (\(bUrl, o) -> LParseState bUrl Nothing o) <<< second(mkNode (LParseState Nothing Nothing undefined)) -< (bUrl, desc)
              triples <- parseDescription >. id -< (state, desc)
              returnA -< mkRdf triples bUrl prefixMap
  where toAttrMap = (getAttrl >>> (getName &&& (getChildren >>> getText))) >. id
        toPrefixMap = PrefixMappings . Map.fromList . map (\(n, m) -> (s2b (drop 6 n), s2b m)) . filter (startswith "xmlns:" . fst)

-- |Read an rdf:Description tag to its corresponding Triples
parseDescription :: forall a. (ArrowXml a, ArrowState GParseState a) => a (LParseState, XmlTree) Triple
parseDescription = updateState
               >>> (arr2A parsePredicatesFromAttr <+> (second (getChildren >>> isElem) >>> parsePredicatesFromChildren))

-- |Read the attributes of an rdf:Description element.  These correspond to the Predicate Object pairs of the Triple
parsePredicatesFromAttr :: forall a. (ArrowXml a, ArrowState GParseState a) => LParseState -> a XmlTree Triple
parsePredicatesFromAttr s = getAttrl >>> ((getName >>> isA (/= "rdf:about") >>> (arr (unode . s2b))) &&& (getChildren >>> getText >>> arr (lnode . plainL . s2b))) >>> arr (attachSubject (stateSubject s))

-- |Read a children of an rdf:Description element.  These correspond to the Predicate portion of the Triple
parsePredicatesFromChildren :: forall a. (ArrowXml a, ArrowState GParseState a) => a (LParseState, XmlTree) Triple
parsePredicatesFromChildren = updateState
                          >>> choiceA [ second (hasAttrValue "rdf:parseType" (== "Literal")) :-> arr2A getLiteralTriple
                                      , second (hasAttrValue "rdf:parseType" (== "Collection")) :-> arr2A getCollectionTriples
                                      , second (hasAttr "rdf:resource") :-> arr2A getResourceTriple
                                      , this :-> proc (state, predXml) -> do
                                                      p <- arr(unode . s2b) <<< getName -< predXml
                                                      t <- arr2A (\s -> arr2A (parseObjectsFromChildren s)) <<< second (second getChildren) -< (state, (p, predXml))
                                                      returnA -< t
                                      ]

parseObjectsFromChildren :: forall a. (ArrowXml a, ArrowState GParseState a)
                         => LParseState -> Predicate -> a XmlTree Triple
parseObjectsFromChildren s p = (isText >>> getText >>> arr ((Triple (stateSubject s) p) . mkLiteralNode s))
                           <+> (isElem >>> hasName "rdf:Description" >>> parseObjectDescription)
  where parseObjectDescription = proc desc -> do
                                      o <- mkNode s -< desc
                                      t0 <- arr (\(sub, (p, o)) -> Triple sub p o) -< (stateSubject s, (p, o))
                                      t <- arr fst <+> (parseDescription <<< arr snd) -< (t0, (s { stateSubject = o }, desc))
                                      returnA -< t

attachSubject :: Subject -> (Predicate, Object) -> Triple
attachSubject s (p, o) = Triple s p o

-- |Updates the local state at a given node
updateState :: forall a. (ArrowXml a, ArrowState GParseState a)
            => a (LParseState, XmlTree) (LParseState, XmlTree)
updateState = (ifA (second (hasAttr "rdf:lang")) (arr2A readLang) (arr id))
          >>> (ifA (second (hasAttr "xml:base")) (arr2A readBase) (arr id))
  where readLang state = (getAttrValue0 "rdf:lang" >>> arr (\lang -> state { stateLang = Just lang } ) ) &&& arr id
        readBase state = (getAttrValue0 "xml:base" >>> arr (\base -> state { stateBaseUrl = (Just . BaseUrl . s2b) base } ) ) &&& arr id

-- |Read a Triple with an rdf:parseType of Literal
getLiteralTriple :: forall a. (ArrowXml a, ArrowState GParseState a) => LParseState -> a XmlTree Triple
getLiteralTriple state = ((getName >>> arr (unode . s2b)) &&& (xshow ( getChildren ) >>> arr (mkTypedLiteralNode state nodeType))) >>> arr (attachSubject (stateSubject state))
  where nodeType = mkFastString (s2b "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral")

-- TODO
getCollectionTriples :: forall a. (ArrowXml a, ArrowState GParseState a) => LParseState -> a XmlTree Triple
getCollectionTriples state = none

-- TODO: include use of BaseURL
getResourceTriple :: forall a. (ArrowXml a, ArrowState GParseState a) => LParseState -> a XmlTree Triple
getResourceTriple state = ((getName >>> arr (unode . s2b)) &&& (getAttrValue "rdf:resource" >>> arr (unode . s2b))) >>> arr (attachSubject (stateSubject state))

-- |Read a Node from the "rdf:about" property or generate a blank node
mkNode :: forall a. (ArrowXml a, ArrowState GParseState a) => LParseState -> a XmlTree Node
mkNode s = choiceA [ hasAttr "rdf:about" :-> (getAttrValue "rdf:about" >>> arr (unode . s2b))
                   , hasAttr "rdf:resource" :-> (getAttrValue "rdf:resource" >>> arr (unode . s2b))
                   , this :-> mkBlankNode
                   ]

mkTypedLiteralNode :: LParseState -> FastString -> String -> Node
mkTypedLiteralNode (LParseState _ (Just lang) _) t content = (lnode (typedL (s2b content) t))
mkTypedLiteralNode (LParseState _ Nothing _) t content = lnode (typedL (s2b content) t)

-- |Use the given state to create a literal node
mkLiteralNode :: LParseState -> String -> Node
mkLiteralNode (LParseState _ (Just lang) _) content = (lnode (plainLL (s2b content) (s2b lang)))
mkLiteralNode (LParseState _ Nothing _) content = (lnode . plainL . s2b) content

-- |Generate an RDF blank node with incrementing IDs from the arrow state
mkBlankNode :: forall a b. (ArrowState GParseState a) => a b Node
mkBlankNode = nextState (\gState -> gState { stateGenId = stateGenId gState + 1 } ) >>> arr (BNodeGen . stateGenId)

