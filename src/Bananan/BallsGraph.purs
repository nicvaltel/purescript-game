module Bananan.BallsGraph where

import Bananan.Reexport

import Bananan.Actors (ActorData(..), BallColor)
import Data.List (List)
import Data.List as List
import Engine.Model (Actor(..), NameId)


newtype NodeBall = NodeBall NodeBallRec

type NodeBallRec =
  { nameId :: NameId
  , color :: BallColor
  , attachedToCeiling :: Boolean
  , neighbours :: List NodeBall
  }

derive instance newtypeNodeBall :: Newtype NodeBall _

instance showNodeBall :: Show NodeBall where
  show (NodeBall r) = show r

type GraphBall = List NodeBall

getNamesId :: GraphBall -> List NameId
getNamesId = map (\(NodeBall nb) -> nb.nameId)

addNodeBall :: Actor ActorData -> List (Actor ActorData) -> GraphBall -> GraphBall
addNodeBall (Actor a) connectedBalls graph = case a.data of
    ActorBall ball | a.nameId `List.notElem` (getNamesId graph) -> do
        let connectedNamesIds = map (\(Actor b) -> b.nameId) connectedBalls
        let neighbours = List.filter (\(NodeBall nb) -> nb.nameId `List.elem` connectedNamesIds) graph
        let newNode = NodeBall 
                { nameId : a.nameId
                , color : ball.color
                , attachedToCeiling : a.y < 1.0  
                , neighbours : neighbours
                }
        List.(:) newNode graph
    _ -> graph

deleteNodeBall :: NameId -> GraphBall -> GraphBall
deleteNodeBall nameId graph =
    if nameId `List.elem` (getNamesId graph) 
        then
            map (\(NodeBall node) -> NodeBall node{neighbours = List.filter (\(NodeBall neigh) -> neigh.nameId /= nameId ) node.neighbours}) $
                List.filter (\(NodeBall nb) -> nb.nameId /= nameId) graph
        else graph

findAttachedToCeilingBalls :: List NodeBall -> List NameId -> List NameId
findAttachedToCeilingBalls nodesToCheck attachedNodes = do
    let part = flip List.partition nodesToCheck $ \(NodeBall n) -> 
                    n.attachedToCeiling || 
                    n.nameId `List.elem` attachedNodes ||
                    List.any (\(NodeBall m) -> m.nameId `List.elem` attachedNodes) n.neighbours
    if null part.yes -- partition.ynodesToCheckes for elems, where predicate is true; partition.no for elems where predicate is false
        then attachedNodes
        else findAttachedToCeilingBalls part.no (attachedNodes <> getNamesId part.yes)

findNotAttachedToCeilingBalls :: GraphBall -> List NameId
findNotAttachedToCeilingBalls graph = List.(\\) (getNamesId graph) (findAttachedToCeilingBalls graph List.Nil)

