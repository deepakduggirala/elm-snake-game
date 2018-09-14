# elm-snake-game

Try it at: https://deepakduggirala.github.io/elm-snake-game/

Model of the snake is "continuous" which allows for variable speeds without messing with fps.

Src/Snake.elm and Src/Math.elm provides the interface for snake model.
The app (Model, View and Update) is agnostic of the snake implementation.

# TODO
1. Simplify snake model
    - Restrict snake to move only along X and Y axes
    - Use list to track head positions and truncate at base when information is no longer needed
2. Lossless Grid
    - Use 2^32 x 2^32 grid.
      - Pros: I don't have to deal with information loss and tolerances involved with floating point arithmatic.
      - Cons: I should transform the grid to view and viceversa
3. Multiplayer
    - ?
4. Upgrade to 0.19
    - Does dependent modules have 0.19 versions?