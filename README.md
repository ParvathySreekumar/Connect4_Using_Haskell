This is a simple implementation of the game connect4 with minimax AI to play against the computer.

To run the project and start playing use the below commands
-- stack build
-- stack run connect4-exe


Expected Outcomes
-----------------

* Yellow is assigned for human player and red for the computer.
* When the game starts, it asks the human player to take the initial turn.
* As we progress through the game , the computer uses minimax algorithm to play its part and prevent the human player from securing wins.
* The code is designed to identify horizontal, vertical, and diagonal wins during the game.


Limitations
-----------

* There is some limitation in minimax AI due to which all the expected wins are not blocked by the computer, but it does capture some of them.
* Preventing wins is given a higher priority than taking the chance of winning. (Penality of -20 for preventing win and 10 for taking chance of win)
* In minimax AI, while calculating the possible moves it always starts with column 6. Hence computer mostly starts its first moove with column 6.
