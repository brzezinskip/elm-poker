Participating in the game:

1. You need to pay amount at least equal to the last raise to make a call and participate in the game. -> Call.

2. You can raise further meaning you have to pay amount equal to the last raise plus amount which you want to raise by. -> Raise.

3. If you have less money than the last raise then you can only fold or all in.

4. You can always fold.


TODO:


1. Add state tracking of last raise action
  ```elm
    type alias LastRaiseAction = {raiseAmount: Int, raisingPlayer: Player}
  ```

2. Implement availableActions function 
  ```elm
    availableActions : Player -> LastRaiseAction -> List Player.Msg
  ```
