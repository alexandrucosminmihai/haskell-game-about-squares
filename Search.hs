{-# OPTIONS_GHC -Wall #-}

module Search where

import ProblemState

import Data.Maybe

import Data.List

import qualified Data.Set as S

{- 
    The type of a Node used in the searching process.
    Different data structures for the root of the tree and the other nodes
    where they are represented by their state, the action that led to that state,
    the parent node, the depth
-}
data Node s a = NewNode s a (Node s a) Int | Root s Int
    deriving (Eq, Show)

-- Returns the state stored in a Node
nodeState :: Node s a -> s
nodeState (NewNode state action parentNode depth) = state
nodeState (Root state depth) = state

{-
    Returns a list of the nodes that were visited in the limited depth search 
    of the state space, starting from the given state
-}
limitedDfs :: (ProblemState s a, Ord s)
           => s           -- Initial state
           -> Bool        -- Flag for using or not using the heuristic
           -> Int         -- Maximum exploration depth
           -> [Node s a]  -- The resulting list of visited nodes
limitedDfs initState useHeuristic maxDepth = root:(reverse (dfs maxDepth (map (\ (childAction, childState) -> (NewNode childState childAction root 1) ) (successors initState)) S.empty [] useHeuristic))
    where root = Root initState 0 

compareNodes :: (ProblemState s a, Ord s) => (Node s a) -> (Node s a) -> Ordering
compareNodes node1 node2 = compare (heuristic (nodeState node2)) (heuristic (nodeState node1))

dfs :: (ProblemState s a, Ord s) => Int -> [Node s a] -> S.Set s -> [Node s a] -> Bool -> [Node s a]
{-
    If I reached the last level, I stop and return the list of the nodes that have been visited until now.
    Otherwise, add the current node to the visited nodes set and continue the propagation, returning the
        union of all the sets returned by the successors of the current node
-}
--dfs maxDepth toVisit visitedStates visitedNodes useHeuristic
dfs maxDepth [] visitedStates visitedNodes useHeuristic = visitedNodes -- if there is nothing left to visit
-- if there is at least on more node to visit
dfs maxDepth ((NewNode state action parent depth):xs) visitedStates visitedNodes useHeuristic 
    {-
        if I reached the last level (of depth), I stop using the current node and I continue with the 
        other traversals (the other nodes from toVisit)
    -}
    |depth > maxDepth = dfs maxDepth xs visitedStates visitedNodes useHeuristic 
    -- If I already visited this state, there is no other information I can obtain by traversing its subtree
    |S.member state visitedStates = dfs maxDepth xs visitedStates visitedNodes useHeuristic
    |useHeuristic == False = dfs maxDepth ((map (\ (childAction, childState) -> (NewNode childState childAction (NewNode state action parent depth) (depth + 1))) (successors state)) ++ xs) (S.insert state visitedStates) ((NewNode state action parent depth):visitedNodes) useHeuristic
    |otherwise = dfs maxDepth ( sortBy compareNodes ((map (\ (childAction, childState) -> (NewNode childState childAction (NewNode state action parent depth) (depth + 1))) (successors state)) ++ xs) ) (S.insert state visitedStates) ((NewNode state action parent depth):visitedNodes) useHeuristic


isNodeSolution :: (ProblemState s a, Ord s) => Node s a -> Bool
isNodeSolution (NewNode state action parent depth) = isGoal state
isNodeSolution (Root state depth) = isGoal state

-- Take a state and verifies if by going until maxDepth it can find a solution in the state space tree
hasSolution :: (ProblemState s a, Ord s) => s -> Int -> Bool
hasSolution initialState maxDepth = foldl (||) False (map isNodeSolution $ limitedDfs initialState False maxDepth)

 -- We must know for sure that in the given list there is a node which represents the final state
getMySolution :: (ProblemState s a, Ord s) => [Node s a] -> (Node s a, Int)
getMySolution nodes = (head theRest, length badOnes)
    where (badOnes, theRest) = break isNodeSolution nodes

{-
    Explores depth first the state space using iterative deepening in order
    to find the first final (goal) state.

    Returns a pair consisting of the node which holds the goal state and the
    number of states (that are not final) visited until that moment
-}

iterativeDeepening :: (ProblemState s a, Ord s)
    => s                -- Initial state
    -> Bool             -- True if we use the heuristic
    -> (Node s a, Int)  -- (First final state Node, number of not final states visited)
{-
    I try one by one all the maxDepths starting with 1.
    How can I stop when I find a solution and not continue going with maxDepth to infinity?
        Solution 1: Explicit recursion = keep a Bool parameter named 'found'. When 'found' becomes True, I stop and return something
        Solution 2: Use dropWhile or Break to go through all the depths from 1 to infinity that do not have a final State.
                    The first number after these will represent the first depth where I can find a final state.
-}
iterativeDeepening initialState useHeuristic = (solNode, notSolNodesInSolNodesTraversal + notSolNodesInUnsuccessfulTraversals + 1)
    where (dontHaveSolution, theRest) = break (hasSolution initialState) [1..]
          (solNode, notSolNodesInSolNodesTraversal) = getMySolution ( limitedDfs initialState useHeuristic ( head theRest ) )
          notSolNodesInUnsuccessfulTraversals = foldl (+) 0 (map (length . (limitedDfs initialState useHeuristic)) dontHaveSolution) 

getParent :: Node s a -> Maybe (Node s a)
getParent (NewNode _ _ (Root _ _) _) = Nothing
getParent (NewNode _ _ parent _ ) = Just parent
{-
    Starting from a node, rebuilds the path to the first state, following the links to parents.
    Returns a list of pairs (action, state) which ends with the final state, but doesn't
    contain the initial state.
-}
extractPath (NewNode state action (Root _ _) depth) = [(action, state)]
extractPath (NewNode state action parent depth) = (extractPath parent) ++ [(action, state)]

{-
    Used to print each element in a list on a separate line.
-}
printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))