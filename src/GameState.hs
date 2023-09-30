{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where 

-- These are all the import. Feel free to use more if needed.
import RenderState (BoardInfo (..), Point, DeltaBoard)
import qualified RenderState as Board
import Data.Sequence ( Seq(..))
import qualified Data.Sequence as S
import System.Random (uniformR, RandomGen(split), StdGen)
import Data.Maybe (isJust)

-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

-- | The snakeSeq is a non-empty sequence. It is important to use precise types in Haskell
--   In first sight we'd define the snake as a sequence, but If you think carefully, an empty 
--   sequence can't represent a valid Snake, therefore we must use a non empty one.
--   You should investigate about Seq type in haskell and we it is a good option for our porpouse.
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

-- | The GameState represents all important bits in the game. The Snake, The apple, the current direction of movement and 
--   a random seed to calculate the next random apple.
data GameState = GameState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  , randomGen :: StdGen
  }
  deriving (Show, Eq)

-- | This function should calculate the opposite movement.
oppositeMovement :: Movement -> Movement
oppositeMovement North = South
oppositeMovement South = North
oppositeMovement East = West
oppositeMovement West = East

-- >>> oppositeMovement North == South
-- >>> oppositeMovement South == North
-- >>> oppositeMovement East == West
-- >>> oppositeMovement West == East


-- | Purely creates a random point within the board limits
--   You should take a look to System.Random documentation. 
--   Also, in the import list you have all relevant functions.
makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
makeRandomPoint (BoardInfo h w) gen = ((y, x), gen1')
  where
    (gen1, gen2) = split gen
    (y, gen1') = uniformR (1, h) gen1
    (x, _) = uniformR (1, w) gen2

{-
We can't test makeRandomPoint, because different implementation may lead to different valid result.
-}


-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq -> Bool
inSnake p (SnakeSeq sHead sBody) = p == sHead || p `elem` sBody

{-
This is a test for inSnake. It should return 
True
True
False
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> inSnake (1,1) snake_seq
-- >>> inSnake (1,2) snake_seq
-- >>> inSnake (1,4) snake_seq
-- True
-- True
-- False


-- | Calculates the new head of the snake. Considering it is moving in the current direction
--   Take into acount the edges of the board
nextHead :: BoardInfo -> GameState -> Point
nextHead (BoardInfo h w) (GameState (SnakeSeq (y, x) _) _ mov _) =
  case mov of
    East -> if x == w then (y, 1) else (y, x + 1)
    West -> if x == 1 then (y, w) else (y, x - 1)
    South -> if y == h then (1, x) else (y + 1, x)
    North -> if y == 1 then (h, x) else (y - 1, x)

{-
This is a test for nextHead. It should return
True
True
True
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,2) 
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> nextHead board_info game_state1 == (1,4)
-- >>> nextHead board_info game_state2 == (2,1)
-- >>> nextHead board_info game_state3 == (4,1)
-- True
-- True
-- True


-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: BoardInfo -> GameState -> (Point, StdGen)
newApple binfo state@(GameState ss apos _ gen) =
  if inSnake p ss || p == apos
  then newApple binfo state{randomGen = gen'}
  else (p, gen')
  where (p, gen') = makeRandomPoint binfo gen

{- We can't test this function because it depends on makeRandomPoint -}


-- | Moves the snake based on the current direction. It sends the adequate RenderMessage
-- Notice that a delta board must include all modified cells in the movement.
-- For example, if we move between this two steps
--        - - - -          - - - -
--        - 0 $ -    =>    - - 0 $
--        - - - -    =>    - - - -
--        - - - X          - - - X
-- We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]
--
-- Another example, if we move between this two steps
--        - - - -          - - - -
--        - - - -    =>    - X - -
--        - - - -    =>    - - - -
--        - 0 $ X          - 0 0 $
-- We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
-- 
move :: BoardInfo -> GameState -> ([Board.RenderMessage], GameState)
move info state@(GameState (SnakeSeq sHead sBody) apple _ _)
  | gameOver = ([Board.GameOver], state)
  | nHead == apple =
    let (nApple, gen') = newApple info state in
      ( [ Board.RenderBoard [(apple, Board.SnakeHead), (sHead, Board.Snake), (nApple, Board.Apple)]
        , Board.ScoreUp]
      , state {snakeSeq = SnakeSeq nHead (sHead :<| sBody), applePosition = nApple, randomGen = gen'}
      )
  | otherwise = case sBody of
    S.Empty -> 
      ( [Board.RenderBoard [(nHead, Board.SnakeHead), (sHead, Board.Empty)]]
      , state {snakeSeq = SnakeSeq nHead sBody}
      )
    b0 :<| S.Empty -> 
      ( [Board.RenderBoard [(nHead, Board.SnakeHead), (sHead, Board.Snake), (b0, Board.Empty)]]
      , state {snakeSeq = SnakeSeq nHead (S.singleton sHead)}
      )
    bs :|> b1 -> 
      ( [Board.RenderBoard [(nHead, Board.SnakeHead), (sHead, Board.Snake), (b1, Board.Empty)]]
      , state {snakeSeq = SnakeSeq nHead (sHead :<| bs)}
      )
  where 
    nHead = nextHead info state
    gameOver = nHead `elem` sBody

{- This is a test for move. It should return

RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)] ** your Apple might be different from mine
RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

-}

-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,1) 
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> fst $ move board_info game_state1
-- >>> fst $ move board_info game_state2
-- >>> fst $ move board_info game_state3
-- RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
-- RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)]
-- RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]
