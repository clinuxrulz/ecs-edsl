module EcsEdsl.NodeM where

import EcsEdsl.NodeId
import EcsEdsl.ExprNode

import Control.Monad.State
import Data.Bimap

newtype DAG = DAG (Bimap NodeId ExprNode)

newtype NodeM a = NodeM { unNodeM :: State DAG NodeId }
