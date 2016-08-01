module EcsEdsl.MathNode where

import EcsEdsl.NodeId

data MathNode
  = Not NodeId
  | And NodeId NodeId
  | Or NodeId NodeId
  | AddNodeId NodeId NodeId
  | SubNodeId NodeId NodeId
  | MultNodeId NodeId NodeId
  | DivNodeId NodeId NodeId
  | ModNodeId NodeId NodeId
  | AddDouble NodeId NodeId
  | SubDouble NodeId NodeId
  | MultDouble NodeId NodeId
  | DivDouble NodeId NodeId
  deriving (Eq, Ord)
