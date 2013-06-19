{-# LANGUAGE OverloadedStrings #-}
module SoOSiM.Components.SoOSApplicationGraph where

import Data.Aeson          ((.:),(.:?),(.!=),FromJSON(..),Value (..))
import Control.Applicative ((<$>),(<*>),(<|>))

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor

type VertexId = Int

-- | Every vertex is a code part - this may be compared to a thread, a
-- function, or a process. In the most general case, a Vertex can also
-- include concurrent code, in a hierarchical way. However, to
-- simplify the simulation, here we assume that a vertex is a
-- sequential piece of code, that can be implemented by a thread or by
-- a process
data Vertex
  = Vertex
  { -- | The vertex identifier. This must be unique in the graph, and
    -- must be a number >= 0.
    --
    -- And id == -1 means no vertex (see the Edge below)
    v_id                  :: VertexId

    -- | The resource characteristics of this piece of code. This
    -- include the memory requirements (all memory, data, and code)
    -- and the ISA.
  , resourceRequirements  :: ResourceDescriptor

    -- | This is not needed right now
  , pointerToCodeInMemory :: Int

    -- | This is an indication of the amount of simulation time
    -- (cycles) that are needed to execute this piece of code.
    -- Notice that depends on the ISA described in the
    -- resourceRequirements part. When changing ISA, this has to
    -- change (see CodeAdapter)
  , executionTime         :: Int
    -- | (Offset,Length)
  , memRange              :: (Int,Int)
  , appCommands           :: [AppCommand]
  , alloc                 :: Maybe Int
  } deriving Show

data AppCommand
  = ReadCmd  (Int,Int)
  | DelayCmd Int
  | WriteCmd (Int,Int)
  deriving Show

instance FromJSON AppCommand where
  parseJSON (Object v) =
    (ReadCmd <$> (v .: "read")) <|>
    (DelayCmd <$> (v .: "delay")) <|>
    (WriteCmd <$> (v .: "write"))

instance FromJSON Vertex where
  parseJSON (Object v) =
    Vertex <$>
      (v .:  "id") <*>
      (v .:  "resourceRequirements") <*>
      (v .:? "pointerToCodeInMemory" .!= 0) <*>
      (v .:  "executionTime") <*>
      (v .:? "mem" .!= (0,0)) <*>
      (v .:? "commands" .!= []) <*>
      (v .:? "alloc")

-- | This structure represents a directed edge between two vertexes,
-- source and destination. This edge will pass tokens from one Vertex
-- to the other. Tokens are just nameless signals, and do not contain
-- any other information except the fact that they have been sent and
-- hence will activate the destination vertex.
--
-- Also, an edge contains the initial number of tokens.
data Edge
  = Edge
  { start    :: VertexId
  , end      :: VertexId
  , n_tokens :: Int
  , periodic :: Maybe Int
  , deadline :: Maybe Int
  } deriving Show

instance FromJSON Edge where
  parseJSON (Object v) =
    Edge <$>
      (v .: "nodeOut") <*>
      (v .: "nodeIn") <*>
      (v .:? "ntokens" .!= 0) <*>
      (v .:? "periodic") <*>
      (v .:? "deadline")

-- | The graph that represents an application.
--
-- TODO:
-- we need ways to visit the graph:
--
-- - depth-first
--
-- - breadth-first
--
-- Then algorithms to:
--
-- - compute the longest path
--
-- - compute the maximum number of parallel vertexes (number of different paths?)
--
-- Are these libraries in Haskell that we can easily re-use?
data ApplicationGraph
  = ApplicationGraph
  { appName       :: String
  , schedulerSort :: Maybe String
  , allocSort     :: Maybe String
  , recSpec       :: Maybe [ (ResourceDescriptor,Int) ]
  , vertices      :: [Vertex]
  , edges         :: [Edge]
  } deriving Show

instance FromJSON ApplicationGraph where
  parseJSON (Object v) =
    ApplicationGraph <$>
      (v .: "name") <*>
      (v .:? "scheduler") <*>
      (v .:? "allocation") <*>
      (v .:? "resourceSpecification" ) <*>
      (v .: "vertices") <*>
      (v .: "edges")

-- | returns the total number of vertexes
numberOfVertices :: ApplicationGraph -> Int
numberOfVertices (ApplicationGraph _ _ _ _ v _) = length v
