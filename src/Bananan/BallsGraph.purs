module Bananan.BallsGraph
  ( BallData
  , Graph
  , GraphBall
  , GraphNode(..)
  , addNodeBall
  , deleteNodeBall
  , findNotAttachedToCeilingBalls
  , updateAttachedToCeilingInGraphBall
  )
  where

import Bananan.Reexport hiding ((:))

import Bananan.Actors (ActorData(..), BallColor)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map.Internal as M
import Engine.Model (Actor(..), NameId)



data GraphNode a id = Node {nodeId :: id, nodeData :: a, neighbours :: List (GraphNode a id)}

instance showGraphNode :: (Show a, Show id) => Show (GraphNode a id) where
  show (Node{nodeId, nodeData, neighbours})  = 
    "Node nodeId = " <> show nodeId <>
    " nodeData = " <> show nodeData <>
    " neighbours = " <> show neighbours

type Graph a id = List (GraphNode a id)


mapGraphRecursive :: forall a b id. Eq id => (id -> a -> b) -> List (GraphNode a id) -> List (GraphNode b id)
mapGraphRecursive f graph = mgraph graph Nil
  where
    mgraph :: List (GraphNode a id) -> List id -> List (GraphNode b id)
    mgraph Nil _ = Nil
    mgraph neighs prevs = fst $
      foldr (\(Node{nodeId, nodeData, neighbours}) tpl@(Tuple newGraph accPrevs) -> 
        if nodeId `List.elem` accPrevs 
          then tpl
          else
            let newPrevs = nodeId : accPrevs
                newNeighbours = mgraph neighbours newPrevs
                newNode = Node{nodeId, nodeData : (f nodeId nodeData), neighbours : newNeighbours} 
            in Tuple (newNode : newGraph) newPrevs)
        (Tuple Nil prevs)
        neighs  

filterGraphRecursive :: forall a id. Eq id => (id -> a -> Boolean) -> List (GraphNode a id) -> List (GraphNode a id)
filterGraphRecursive predicate graph = fgraph graph Nil
  where 
    filterNeighs :: List (GraphNode a id) -> List (GraphNode a id)
    filterNeighs = List.filter (\(Node{nodeId, nodeData}) -> predicate nodeId nodeData)

    fgraph :: List (GraphNode a id) -> List id -> List (GraphNode a id)
    fgraph Nil _ = Nil
    fgraph neighs prevs = fst $
      foldr (\(Node{nodeId, nodeData, neighbours}) tpl@(Tuple newGraph accPrevs) -> 
        if nodeId `List.elem` accPrevs 
          then tpl
          else
            let newPrevs = nodeId : accPrevs
                newNeighbours = fgraph (filterNeighs neighbours) newPrevs
                newNode = Node{nodeId, nodeData, neighbours : newNeighbours} 
            in Tuple (newNode : newGraph) newPrevs)
        (Tuple Nil prevs)
        neighs  


getTopLayerNamesId :: forall a id. Graph a id -> List id
getTopLayerNamesId = map (\(Node{nodeId}) -> nodeId)


type BallData =
  { nameId :: NameId
  , color :: BallColor
  , attachedToCeiling :: Boolean
  }

type GraphBall = Graph BallData NameId


updateAttachedToCeilingInGraphBall :: M.Map NameId (Actor ActorData) -> GraphBall -> GraphBall
updateAttachedToCeilingInGraphBall allBalls graph = do
  let updateFunc :: NameId -> BallData -> BallData 
      updateFunc nodeId nodeData = case M.lookup nodeId allBalls of
        Just (Actor a) -> nodeData{attachedToCeiling = a.y < 1.0}
        _ -> nodeData
  mapGraphRecursive updateFunc graph

addNodeBall :: Actor ActorData -> List (Actor ActorData) -> GraphBall -> GraphBall
addNodeBall (Actor a) connectedBalls graph = case a.data of
    ActorBall ball | a.nameId `List.notElem` (getTopLayerNamesId graph) -> do -- getTopLayerNamesId is OK because all balls are present at top level of list (no need to recurcsive search)
        let connectedNamesIds = map (\(Actor b) -> b.nameId) connectedBalls
        let neighbours = List.filter (\(Node{nodeData}) -> nodeData.nameId `List.elem` connectedNamesIds) graph
        let newNode = Node 
                { nodeId : a.nameId
                , nodeData : 
                    { nameId : a.nameId
                    , color : ball.color
                    , attachedToCeiling : a.y < 1.0 -- TODO here is problem. When ball move down it stays attachedToCeiling
                    }
                , neighbours : neighbours
                }
        -- attache new ball as a neighbor to old ones
        let updatedGraph = flip map graph $ \node@(Node n) ->
              if n.nodeId `List.elem` connectedNamesIds
                then Node n{neighbours = newNode : n.neighbours}
                else node
        (newNode : updatedGraph)
    _ -> graph

deleteNodeBall :: NameId -> GraphBall -> GraphBall
deleteNodeBall nameId graph = -- we need to delete ball from top level and from all list of neighbours at top level
  let filteredTopLayer = List.filter (\(Node{nodeId}) -> nodeId /= nameId) graph
  in flip map filteredTopLayer $ \(Node node) -> 
      Node node{ neighbours = List.filter (\(Node{nodeId}) -> nodeId /= nameId ) node.neighbours}

findAttachedToCeilingBalls :: GraphBall -> List NameId -> List NameId
findAttachedToCeilingBalls nodesToCheck attachedNodes = do
    let part = flip List.partition nodesToCheck $ \(Node n) -> 
                    n.nodeData.attachedToCeiling || 
                    n.nodeId `List.elem` attachedNodes ||
                    List.any (\(Node m) -> m.nodeId `List.elem` attachedNodes) n.neighbours
    if null part.yes -- partition.yes for elems, where predicate is true; partition.no for elems where predicate is false
        then attachedNodes
        else findAttachedToCeilingBalls part.no (attachedNodes <> getTopLayerNamesId part.yes)

findNotAttachedToCeilingBalls :: GraphBall -> List NameId
findNotAttachedToCeilingBalls graph = List.(\\) (getTopLayerNamesId graph) (findAttachedToCeilingBalls graph List.Nil)






gr1 :: Graph String Int
gr1 = 
    Node {nodeId : 1, nodeData : "one", neighbours : 
      ( Node {nodeId : 10, nodeData : "ten", neighbours : 
        Node {nodeId : 15, nodeData : "fifteen", neighbours : Nil} : Nil } : 
        Node {nodeId : 11, nodeData : "eleven", neighbours : Nil} : Nil
      ) } :
    Node {nodeId : 2, nodeData : "two", neighbours : 
      (Node {nodeId : 10, nodeData : "ten", neighbours: Nil} :
        Node {nodeId : 20, nodeData : "twenty", neighbours : 
      (Node {nodeId : 10, nodeData : "ten", neighbours: Nil}: Nil) } : Nil)} :
    Nil
  

runTestGraph :: Effect Unit
runTestGraph = do
  log (show gr1)
  log ""
  log (show $ mapGraphRecursive (\_ s -> "_" <> s  ) gr1)
  log ""
  log(show $ filterGraphRecursive (\id _ -> id < 15) gr1)

  pure unit
