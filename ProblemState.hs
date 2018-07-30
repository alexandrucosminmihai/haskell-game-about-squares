{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module ProblemState where

{-
    Exposes the functions that are needed for creating the state space for
    any given problem.

    's' and 'a' represent the types of the states and of the actions which
    transform a state into another.
-}
class ProblemState s a | s -> a where
    {-
        For the current state, output the list of all possible
        (action, next state) pairs
    -}
    successors :: s -> [(a, s)]

    -- Returns True if the current state is a final, goal state
    isGoal :: s -> Bool

    {-
        Returns an estimated distance from the current state to a final state.
        The shorter the distance, the better the current state.
    -}
    heuristic :: s -> Int
    heuristic = const 0