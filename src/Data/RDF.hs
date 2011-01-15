{-# LANGUAGE RankNTypes, MultiParamTypeClasses #-}

-- |The RDF module provides shared semantic types, constructors and tests shared by other libraries.
--

module Data.RDF (
  -- * Serializing RDF
  RdfSerializer(hWriteG, writeG, hWriteH, writeH, hWriteTs, writeTs, hWriteT, writeT, hWriteN, writeN),
  -- * RDF graph 
  RDF(empty, mkRdf, triplesOf, select, query, baseUrl, prefixMappings, addPrefixMappings),
  Semantic(..),
  -- * RDF triples, nodes, and literals
  Triple(Triple), triple, Triples, sortTriples,
  Node(UNode, BNode, BNodeGen, LNode),
  LValue(PlainL, PlainLL, TypedL),

  -- * Supporting types and functions
  BaseUrl(BaseUrl),
  PrefixMappings(PrefixMappings), toPMList, PrefixMapping(PrefixMapping),
  NodeSelector, isUNode, isBNode, isLNode,
  equalSubjects, equalPredicates, equalObjects,
  subjectOf, predicateOf, objectOf,
  Subject, Predicate, Object,
  ParseFailure(ParseFailure),
  FastString(uniq,value),mkFastString,
  s2b,b2s,unode,bnode,lnode,plainL,plainLL,typedL,
  View, view,
  fromEither, maybeHead, removeDupes
) where

import Data.RDF.Namespace
import Data.RDF.Utils

import Data.ByteString.Lazy.Char8(ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List
import Data.Ord (comparing)

import qualified Data.Map as Map

import System.IO

import Text.Printf

-- |A type class for ADTs that expose views to clients.
class View a b where
  view :: a -> b

-- |An alias for 'Node', defined for convenience and readability purposes.
type Subject = Node

-- |An alias for 'Node', defined for convenience and readability purposes.
type Predicate = Node

-- |An alias for 'Node', defined for convenience and readability purposes.
type Object = Node

-- |An RDF graph is a set of (unique) RDF triples, together with the
-- operations defined upon the graph.  A minimal implementation includes
-- at least one of each of:
--   prefixMappings
--   addPrefixMappings
--   mkRdf
--   triplesOf, select or query
--
-- For information about the efficiency of the functions, see the
-- documentation for the particular graph instance.
--
-- For more information about the concept of an RDF graph, see
-- the following: <http://www.w3.org/TR/rdf-concepts/#section-rdf-graph>.
class RDF rdf where

  -- |Return the base URL of this graph, if any.
  baseUrl :: rdf -> Maybe BaseUrl
  baseUrl _ = Nothing

  -- |Return the prefix mappings defined for this graph, if any.
  prefixMappings :: rdf -> PrefixMappings

  -- |Return a graph with the specified prefix mappings merged with
  -- the existing mappings. If the Bool arg is True, then a new mapping
  -- for an existing prefix will replace the old mapping; otherwise,
  -- the new mapping is ignored.
  addPrefixMappings :: rdf -> PrefixMappings -> Bool -> rdf

  -- |Return an empty RDF graph.
  empty :: rdf
  empty = mkRdf [] Nothing (PrefixMappings Map.empty)

  -- |Return a graph containing all the given triples. Handling of duplicates
  -- in the input depend on the particular graph implementation.
  mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> rdf

  -- |Return all triples in the graph, as a list.
  triplesOf :: rdf -> Triples
  triplesOf rdf = query rdf Nothing Nothing Nothing

  -- |Select the triples in the graph that match the given selectors.
  --
  -- The three NodeSelector parameters are optional functions that match
  -- the respective subject, predicate, and object of a triple. The triples
  -- returned are those in the given graph for which the first selector
  -- returns true when called on the subject, the second selector returns
  -- true when called on the predicate, and the third selector returns true
  -- when called on the ojbect. A 'Nothing' parameter is equivalent to a
  -- function that always returns true for the appropriate node; but
  -- implementations may be able to much more efficiently answer a select
  -- that involves a 'Nothing' parameter rather than an @(id True)@ parameter.
  --
  -- The following call illustrates the use of select, and would result in
  -- the selection of all and only the triples that have a blank node
  -- as subject and a literal node as object:
  --
  -- > select gr (Just isBNode) Nothing (Just isLNode)
  --
  -- Note: this function may be very slow; see the documentation for the
  -- particular graph implementation for more information.
  select    :: rdf -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
  select rdf funcS funcP funcO = filter (select' funcS funcP funcO) (triplesOf rdf)
    where select' (Just fS) (Just fP) (Just fO) = \(Triple s p o) -> fS s && fP p && fO o
          select' (Just fS) (Just fP)  Nothing  = \(Triple s p _) -> fS s && fP p
          select' (Just fS)  Nothing  (Just fO) = \(Triple s _ o) -> fS s && fO o
          select' (Just fS)  Nothing   Nothing  = \(Triple s _ _) -> fS s
          select'  Nothing  (Just fP) (Just fO) = \(Triple _ p o) -> fP p && fO o
          select'  Nothing  (Just fP)  Nothing  = \(Triple _ p _) -> fP p
          select'  Nothing   Nothing  (Just fO) = \(Triple _ _ o) -> fO o
          select'  Nothing   Nothing   Nothing  = \(Triple _ _ _) -> True

  -- |Return the triples in the graph that match the given pattern, where
  -- the pattern (3 Maybe Node parameters) is interpreted as a triple pattern.
  --
  -- The @Maybe Node@ params are interpreted as the subject, predicate, and
  -- object of a triple, respectively. @Just n@ is true iff the triple has
  -- a node equal to @n@ in the appropriate location; @Nothing@ is always
  -- true, regardless of the node in the appropriate location.
  --
  -- For example, @ query gr (Just n1) Nothing (Just n2) @ would return all
  -- and only the triples that have @n1@ as subject and @n2@ as object,
  -- regardless of the predicate of the triple.
  query         :: rdf -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
  query rdf s p o = select rdf (toNodeSelector s) (toNodeSelector p) (toNodeSelector o)
    where toNodeSelector Nothing = Nothing
          toNodeSelector (Just node) = Just (== node)

-- |An RdfSerializer is a serializer of RDF to some particular output format, such as
-- NTriples or Turtle.
class RdfSerializer s where
  -- |Write the graph to a file handle using whatever configuration is specified by
  -- the first argument.
  hWriteG     :: forall rdf. (RDF rdf) => s -> Handle -> rdf -> IO ()

  -- |Write the graph to stdout; equivalent to @'hWriteG' stdout@.
  writeG      :: forall rdf. (RDF rdf) => s -> rdf -> IO ()

  -- |Write to the file handle whatever header information is required based on
  -- the output format. For example, if serializing to Turtle, this method would
  -- write the necessary \@prefix declarations and possibly a \@baseUrl declaration,
  -- whereas for NTriples, there is no header section at all, so this would be a no-op.
  hWriteH     :: forall rdf. (RDF rdf) => s -> Handle -> rdf -> IO ()

  -- |Write header information to stdout; equivalent to @'hWriteH' stdout@.
  writeH      :: forall rdf. (RDF rdf) => s -> rdf -> IO ()

  -- |Write some triples to a file handle using whatever configuration is specified
  -- by the first argument. 
  -- 
  -- WARNING: if the serialization format has header-level information 
  -- that should be output (e.g., \@prefix declarations for Turtle), then you should
  -- use 'hWriteG' instead of this method unless you're sure this is safe to use, since
  -- otherwise the resultant document will be missing the header information and 
  -- will not be valid.
  hWriteTs    :: s -> Handle  -> Triples -> IO ()

  -- |Write some triples to stdout; equivalent to @'hWriteTs' stdout@.
  writeTs     :: s -> Triples -> IO ()

  -- |Write a single triple to the file handle using whatever configuration is 
  -- specified by the first argument. The same WARNING applies as to 'hWriteTs'.
  hWriteT     :: s -> Handle  -> Triple  -> IO ()

  -- |Write a single triple to stdout; equivalent to @'hWriteT' stdout@.
  writeT      :: s -> Triple  -> IO ()

  -- |Write a single node to the file handle using whatever configuration is 
  -- specified by the first argument. The same WARNING applies as to 'hWriteTs'.
  hWriteN     :: s -> Handle  -> Node    -> IO ()

  -- |Write a single node to sdout; equivalent to @'hWriteN' stdout@.
  writeN      :: s -> Node    -> IO ()

class Semantic t where
  fromSemantic :: (RDF rdf) => t -> rdf
  toSemantic :: (RDF rdf) => rdf -> t

-- |An RDF node, which may be either a URIRef node ('UNode'), a blank
-- node ('BNode'), or a literal node ('LNode').
data Node =

  -- |An RDF URI reference. See
  -- <http://www.w3.org/TR/rdf-concepts/#section-Graph-URIref> for more
  -- information.
  UNode {-# UNPACK #-} !FastString

  -- |An RDF blank node. See
  -- <http://www.w3.org/TR/rdf-concepts/#section-blank-nodes> for more
  -- information.
  | BNode {-# UNPACK #-} !FastString

  -- |An RDF blank node with an auto-generated identifier, as used in
  -- Turtle.
  | BNodeGen  {-# UNPACK #-} !Int

  -- |An RDF literal. See
  -- <http://www.w3.org/TR/rdf-concepts/#section-Graph-Literal> for more
  -- information.
  | LNode {-# UNPACK #-} !LValue

-- ==============================
-- Constructor functions for Node

-- |Return a URIRef node for the given bytetring URI.
{-# INLINE unode #-}
unode :: ByteString -> Node
unode = UNode . mkFastString

-- |Return a blank node using the given string identifier.
{-# INLINE bnode #-}
bnode :: ByteString ->  Node
bnode = BNode . mkFastString

-- |Return a literal node using the given LValue.
{-# INLINE lnode #-}
lnode :: LValue ->  Node
lnode = LNode

-- Constructor functions for Node
-- ==============================


-- |A list of triples. This is defined for convenience and readability.
type Triples = [Triple]

-- |An RDF triple is a statement consisting of a subject, predicate,
-- and object, respectively.
--
-- See <http://www.w3.org/TR/rdf-concepts/#section-triples> for
-- more information.
data Triple = Triple {-# UNPACK #-} !Node {-# UNPACK #-} !Node {-# UNPACK #-} !Node

-- |A smart constructor function for 'Triple' that verifies the node arguments
-- are of the correct type and creates the new 'Triple' if so or calls 'error'.
-- /subj/ must be a 'UNode' or 'BNode', and /pred/ must be a 'UNode'.
triple :: Subject -> Predicate -> Object -> Triple
triple subj pred obj
  | isLNode subj     =  error $ "subject must be UNode or BNode: "     ++ show subj
  | isLNode pred     =  error $ "predicate must be UNode, not LNode: " ++ show pred
  | isBNode pred     =  error $ "predicate must be UNode, not BNode: " ++ show pred
  | otherwise        =  Triple subj pred obj

-- |The actual value of an RDF literal, represented as the 'LValue'
-- parameter of an 'LNode'.
data LValue =
  -- Constructors are not exported, because we need to have more
  -- control over the format of the literal bytestring that we store.

  -- |A plain (untyped) literal value in an unspecified language.
  PlainL {-# UNPACK #-} !ByteString

  -- |A plain (untyped) literal value with a language specifier.
  | PlainLL {-# UNPACK #-} !ByteString {-# UNPACK #-} !ByteString

  -- |A typed literal value consisting of the literal value and
  -- the URI of the datatype of the value, respectively.
  | TypedL {-# UNPACK #-} !ByteString {-# UNPACK #-} !FastString

-- ================================
-- Constructor functions for LValue

-- |Return a PlainL LValue for the given string value.
{-# INLINE plainL #-}
plainL :: ByteString -> LValue
plainL =  PlainL

-- |Return a PlainLL LValue for the given string value and language,
-- respectively.
{-# INLINE plainLL #-}
plainLL :: ByteString -> ByteString -> LValue
plainLL = PlainLL

-- |Return a TypedL LValue for the given string value and datatype URI,
-- respectively.
{-# INLINE typedL #-}
typedL :: ByteString -> FastString -> LValue
typedL val dtype = TypedL (canonicalize dtype val) dtype

-- Constructor functions for LValue
-- ================================


-- |The base URL of a graph.
newtype BaseUrl = BaseUrl ByteString
  deriving (Eq, Ord, Show)

-- |A 'NodeSelector' is either a function that returns 'True'
--  or 'False' for a node, or Nothing, which indicates that all
-- nodes would return 'True'.
--
-- The selector is said to select, or match, the nodes for
-- which it returns 'True'.
--
-- When used in conjunction with the 'select' method of 'Graph', three
-- node selectors are used to match a triple.
type NodeSelector = Maybe (Node -> Bool)

-- |Represents a failure in parsing an N-Triples document, including
-- an error message with information about the cause for the failure.
newtype ParseFailure = ParseFailure String
  deriving (Eq, Show)

-- |A node is equal to another node if they are both the same type
-- of node and if the field values are equal.
instance Eq Node where
  (UNode fs1)    ==  (UNode fs2)     =  uniq fs1 == uniq fs2
  (BNode fs1)    ==  (BNode fs2)     =  uniq fs1 == uniq fs2
  (BNodeGen i1)  ==  (BNodeGen i2)   =  i1 == i2
  (LNode l1)     ==  (LNode l2)      =  l1 == l2
  _              ==  _               =  False

-- |Node ordering is defined first by type, with Unode < BNode < BNodeGen
-- < LNode PlainL < LNode PlainLL < LNode TypedL, and secondly by
-- the natural ordering of the node value.
--
-- E.g., a '(UNode _)' is LT any other type of node, and a
-- '(LNode (TypedL _ _))' is GT any other type of node, and the ordering
-- of '(BNodeGen 44)' and '(BNodeGen 3)' is that of the values, or
-- 'compare 44 3', GT.
instance Ord Node where
  compare (UNode fs1)                      (UNode fs2)                      = compareFS fs1 fs2
  compare (UNode _)                        _                                = LT
  compare (BNode fs1)                      (BNode fs2)                      = compareFS fs1 fs2
  compare (BNode _)                        (UNode _)                        = GT
  compare (BNode _)                        _                                = LT
  compare (BNodeGen i1)                    (BNodeGen i2)                    = compare i1 i2
  compare (BNodeGen _)                     (LNode _)                        = LT
  compare (BNodeGen _)                     _                                = GT
  compare (LNode (PlainL bs1))             (LNode (PlainL bs2))             = compare bs1 bs2
  compare (LNode (PlainL _))               (LNode _)                        = LT
  compare (LNode (PlainLL bs1 bs1'))       (LNode (PlainLL bs2 bs2'))       =
    case compare bs1' bs2' of
      EQ -> compare bs1 bs2
      LT -> LT
      GT -> GT
  compare (LNode (PlainLL _ _))            (LNode (PlainL _))               = GT
  compare (LNode (PlainLL _ _))            (LNode _)                        = LT
  compare (LNode (TypedL bs1 fs1))         (LNode (TypedL bs2 fs2))         =
    case compare fs1 fs2 of
      EQ -> compare bs1 bs2
      LT -> LT
      GT -> GT
  compare (LNode (TypedL _ _))             (LNode _)                        = GT
  compare (LNode _)                        _                                = GT

-- |Two triples are equal iff their respective subjects, predicates, and objects
-- are equal.
instance Eq Triple where
  (Triple s1 p1 o1) == (Triple s2 p2 o2) = s1 == s2 && p1 == p2 && o1 == o2

-- |The ordering of triples is based on that of the subject, predicate, and object
-- of the triple, in that order.
instance Ord Triple where
  compare = comparing (\(Triple s p o) -> (s, p, o))

-- |Two 'LValue' values are equal iff they are of the same type and all fields are
-- equal.
instance Eq LValue where
  (PlainL bs1)        ==  (PlainL bs2)        =  bs1 == bs2
  (PlainLL bs1 bs1')  ==  (PlainLL bs2 bs2')  =  bs1' == bs2'    &&  bs1 == bs2
  (TypedL bs1 fs1)    ==  (TypedL bs2 fs2)    =  equalFS fs1 fs2 &&  bs1 == bs2
  _                   ==  _                   =  False

-- |Ordering of 'LValue' values is as follows: (PlainL _) < (PlainLL _ _)
-- < (TypedL _ _), and values of the same type are ordered by field values,
-- with '(PlainLL literalValue language)' being ordered by language first and
-- literal value second, and '(TypedL literalValue datatypeUri)' being ordered
-- by datatype first and literal value second.
instance Ord LValue where
  compare (PlainL bs1)       (PlainL bs2)       = compare bs1 bs2
  compare (PlainL _)         _                  = LT
  compare _                  (PlainL _)         = GT
  compare (PlainLL bs1 bs1') (PlainLL bs2 bs2') =
    case compare bs1' bs2' of
      EQ -> compare bs1 bs2
      GT -> GT
      LT -> LT
  compare (PlainLL _ _)       _                 = LT
  compare _                   (PlainLL _ _)     = GT
  compare (TypedL l1 t1) (TypedL l2 t2) =
    case compareFS t1 t2 of
      EQ -> compare l1 l2
      GT -> GT
      LT -> LT

-- String representations of the various data types; generally NTriples-like.

instance Show Triple where
  show (Triple s p o) =
    printf "Triple(%s,%s,%s)" (show s) (show p) (show o)

instance Show Node where
  show (UNode uri)                   = "UNode(" ++ show uri ++ ")"
  show (BNode  i)                    = "BNode(" ++ show i ++ ")"
  show (BNodeGen genId)              = "BNodeGen(" ++ show genId ++ ")"
  show (LNode lvalue)                = "LNode(" ++ show lvalue ++ ")"

instance Show LValue where
  show (PlainL lit)               = "PlainL(" ++ B.unpack lit ++ ")"
  show (PlainLL lit lang)         = "PlainLL(" ++ B.unpack lit ++ ", " ++ B.unpack lang ++ ")"
  show (TypedL lit dtype)         = "TypedL(" ++ B.unpack lit ++ "," ++ show dtype ++ ")"

-- |Answer the given list of triples in sorted order.
sortTriples :: Triples -> Triples
sortTriples = sort

-- |Answer the subject node of the triple.
{-# INLINE subjectOf #-}
subjectOf :: Triple -> Node
subjectOf (Triple s _ _) = s

-- |Answer the predicate node of the triple.
{-# INLINE predicateOf #-}
predicateOf :: Triple -> Node
predicateOf (Triple _ p _) = p

-- |Answer the object node of the triple.
{-# INLINE objectOf #-}
objectOf :: Triple -> Node
objectOf (Triple _ _ o)   = o

-- |Answer if given node is a URI Ref node.
{-# INLINE isUNode #-}
isUNode :: Node -> Bool
isUNode (UNode _) = True
isUNode _         = False

-- |Answer if given node is a blank node.
{-# INLINE isBNode #-}
isBNode :: Node -> Bool
isBNode (BNode _)    = True
isBNode (BNodeGen _) = True
isBNode _            = False

-- |Answer if given node is a literal node.
{-# INLINE isLNode #-}
isLNode :: Node -> Bool
isLNode (LNode _) = True
isLNode _         = False

-- |Determine whether two triples have equal subjects.
equalSubjects :: Triple -> Triple -> Bool
equalSubjects (Triple s1 _ _) (Triple s2 _ _) = s1 == s2

-- |Determine whether two triples have equal predicates.
equalPredicates :: Triple -> Triple -> Bool
equalPredicates (Triple _ p1 _) (Triple _ p2 _) = p1 == p2

-- |Determine whether two triples have equal objects.
equalObjects :: Triple -> Triple -> Bool
equalObjects (Triple _ _ o1) (Triple _ _ o2) = o1 == o2

-- |Convert a parse result into a graph if it was successful
-- and error and terminate if not.
fromEither :: RDF rdf => Either ParseFailure rdf -> rdf
fromEither res =
  case res of
    (Left err) -> error (show err)
    (Right rdf) -> rdf

-- |Remove duplicate triples, returning unique triples. This 
-- function may return the triples in a different order than 
-- given.
removeDupes :: Triples -> Triples
removeDupes =  map head . group . sort

