# Haskell Game About Squares
A Haskell implementation of the hit puzzle game (http://gameaboutsquares.com/) including solution finder.

# Project Structure
### ProblemState.hs
ProblemState.hs contains the type class representing states of any problem.
### Search.hs
Search.hs contains the implementation of the **Iterative Deepening** search algorithm used to solve the levels of the game by exploring the state space tree.

### GAS.hs
GAS.hs contains all the game logic that lets the user move the squares and the code that takes care of the textual visual representation of the game. It also containts the implementation of the ProblemState class for our game.

### Levels.hs
Levels.hs contains some examples of game levels that can be used to play the game (or have it solved for you).