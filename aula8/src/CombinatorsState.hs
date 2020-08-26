module CombinatorsState where

-- state definition

type State s a = s -> (a,s)

-- thenDo function

thenDo :: State s a -> (a -> State s b) -> State s b
thenDo f g s
  = let (result,newState) = f s
      in g result newState

-- interface for manipulating state

remain :: a -> State s a
remain v = \ s -> (v , s)

access :: (s -> a) -> State s a
access f = \ s -> (f s, s)

modify :: (s -> s) -> State s ()
modify f = \ s -> ((), f s)
