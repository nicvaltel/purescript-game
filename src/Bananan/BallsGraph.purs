module Bananan.BallsGraph where

import Bananan.Reexport

import Bananan.Actors (ActorData(..), BallColor)
import Data.List (List(..))
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

getAllNamesId :: GraphBall -> List NameId
getAllNamesId graph = getBalls graph Nil
  where
    getBalls :: List NodeBall -> List NameId -> List NameId
    getBalls neigs prevs = case neigs of
      Nil -> prevs
      _ -> foldr 
              (\(NodeBall nb) prevsAcc -> 
                  if (nb.nameId `List.elem` prevsAcc) 
                    then prevsAcc 
                    else getBalls nb.neighbours (List.(:) nb.nameId prevsAcc))
              prevs 
              neigs 

getListNamesId :: GraphBall -> List NameId
getListNamesId = map (\(NodeBall nb) -> nb.nameId)

addNodeBall :: Actor ActorData -> List (Actor ActorData) -> GraphBall -> GraphBall
addNodeBall (Actor a) connectedBalls graph = case a.data of
    ActorBall ball | a.nameId `List.notElem` (getAllNamesId graph) -> do
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
deleteNodeBall nameId graph = deleteBall graph Nil
  where
    deleteBall :: GraphBall -> List NameId -> GraphBall
    deleteBall graph prevNames =
      let filtered = List.filter (\(NodeBall nb) -> nb.nameId /= nameId) graph
      in flip map filtered $ \node@(NodeBall nb) ->
          if nb.nameId `List.elem` prevNames
            then node
            else NodeBall nb{neighbours = deleteBall nb.neighbours (List.(:) nb.nameId prevNames)}
  

findAttachedToCeilingBalls :: List NodeBall -> List NameId -> List NameId
findAttachedToCeilingBalls nodesToCheck attachedNodes = do
    let part = flip List.partition nodesToCheck $ \(NodeBall n) -> 
                    n.attachedToCeiling || 
                    n.nameId `List.elem` attachedNodes ||
                    List.any (\(NodeBall m) -> m.nameId `List.elem` attachedNodes) n.neighbours
    if null part.yes -- partition.yes for elems, where predicate is true; partition.no for elems where predicate is false
        then attachedNodes
        else findAttachedToCeilingBalls part.no (attachedNodes <> getListNamesId part.yes)

findNotAttachedToCeilingBalls :: GraphBall -> List NameId
findNotAttachedToCeilingBalls graph = List.(\\) (getListNamesId graph) (findAttachedToCeilingBalls graph List.Nil)
