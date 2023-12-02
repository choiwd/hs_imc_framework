{-# LANGUAGE Arrows       #-}
{-# LANGUAGE InstanceSigs #-}

module IMC.Control.BehaviorTree (
    BStatus (..)
    
  -- Functorial Stateful Behavior tree
  , FStateBT (..)
  , runFSBT
) where

import FRP.Yampa ( returnA, Arrow((&&&)), SF, (>>>) )

data BStatus = Success | Failure | Running deriving (Eq, Show)
data BasicBT input output  = Condition (SF input (BStatus, output))
                          | Action    (SF input (BStatus, output))
                          | Fallback  [BasicBT input output]
                          | Sequence  [BasicBT input output]

mkCondition :: Monoid b => (a -> Bool) -> BasicBT a b
mkCondition f = Condition c
  where c = proc a -> do
          if f a
            then
              returnA -< (Success, mempty)
            else
              returnA -< (Failure, mempty)

allCollector :: Monoid b => SF ((BStatus, b), (BStatus, b)) (BStatus, b)
allCollector = proc ((status1, l1), (status2, l2)) -> do
  let joinedSignal  | status1 == Failure                                = (Failure, mempty)
                    | status1 == Running                                = (Running, l1)
                    | status1 == Success && status2 == Running          = (Running, l2)
                    | status1 == Success && status2 == Success          = (Success, mempty)
                    |                       status2 == Failure          = (Failure, mempty)
  returnA -< joinedSignal

andSF :: Monoid b => SF a (BStatus, b) -> SF a (BStatus, b) -> SF a (BStatus, b)
andSF s1 s2 = s1 &&& s2 >>> allCollector

anyCollector :: Monoid b => SF ((BStatus, b), (BStatus, b)) (BStatus, b)
anyCollector = proc ((status1, l1), (status2, l2)) -> do
  let joinedSignal  | status1 == Success                                = (Success, mempty)
                    | status1 == Running                                = (Running, l1)
                    | status1 == Failure && status2 == Running          = (Running, l2)
                    | status1 == Failure && status2 == Failure          = (Failure, mempty)
                    |                       status2 == Success          = (Success, mempty)
  returnA -< joinedSignal

orSF :: Monoid b => SF a (BStatus, b) -> SF a (BStatus, b) -> SF a (BStatus, b)
orSF s1 s2 = s1 &&& s2 >>> anyCollector

-- |Flattens the whole tree to a single arrow
-- Note: nodes with empty lists always crashes.
runBBT :: (Show a, Show b, Monoid b) => BasicBT a b -> SF a (BStatus, b)
runBBT (Condition s) = s
runBBT (Action    s) = s
runBBT (Fallback  s) = wireFallback s
  where wireFallback l = foldl1 orSF (map runBBT l)
runBBT (Sequence  s) = wireSeq s
  where wireSeq l = foldl1 andSF (map runBBT l)

-- ######################################
-- Statefull behavior tree
-- ######################################

-- A Statefull behavior tree is a behavior tree that has an additional parameter
-- that can be used to store a value that is passed down (and possibly "modified") from 
-- root to leaf. That is, this parameter is given to a node and fed to the next called node,
-- which essentially follows the call stack trace (as if one existed in the arrow framework)

data StateBT exState tState output  = ConditionS (SF (exState, tState) ((BStatus, output), tState))
                                    | ActionS    (SF (exState, tState) ((BStatus, output), tState))
                                    | FallbackS  [StateBT exState tState output]
                                    | SequenceS  [StateBT exState tState output]

allCollectorS :: Monoid b => SF (((BStatus, b), c), ((BStatus, b), c)) ((BStatus, b), c)
allCollectorS = proc (((status1, l1), ts1), ((status2, l2), ts2)) -> do
  let joinedSignal  | status1 == Failure                                = ((Failure, mempty), ts1)
                    | status1 == Running                                = ((Running, l1), ts1)
                    | status1 == Success && status2 == Running          = ((Running, l2), ts2)     -- Return ts2 only when I'm sure the second node has executed
                    | status1 == Success && status2 == Success          = ((Success, mempty), ts2)
                    |                       status2 == Failure          = ((Failure, mempty), ts2)
  returnA -< joinedSignal

andSFS :: Monoid output => SF (exState, tState) ((BStatus, output), tState)
                          -> SF (exState, tState) ((BStatus, output), tState)
                          -> SF (exState, tState) ((BStatus, output), tState)
andSFS s1 s2 = proc (exs, ts) -> do
  ((nodeState1, _output1), ts1) <- s1 -< (exs, ts)
  s2_output <- s2 -< (exs, ts1)

  allCollectorS -< (((nodeState1, _output1), ts1), s2_output)

anyCollectorS :: Monoid b => SF (((BStatus, b), c), ((BStatus, b), c)) ((BStatus, b), c)
anyCollectorS = proc (((status1, l1), ts1), ((status2, l2), ts2)) -> do
  let joinedSignal  | status1 == Success                                = ((Success, mempty), ts1)
                    | status1 == Running                                = ((Running, l1), ts1)
                    | status1 == Failure && status2 == Running          = ((Running, l2), ts2)    -- Return ts2 only when I'm sure the second node has executed
                    | status1 == Failure && status2 == Failure          = ((Failure, mempty), ts2)
                    | status1 == Failure && status2 == Success          = ((Success, mempty), ts2)
  returnA -< joinedSignal

orSFS :: Monoid output => SF (exState, tState) ((BStatus, output), tState)
                          -> SF (exState, tState) ((BStatus, output), tState)
                          -> SF (exState, tState) ((BStatus, output), tState)
orSFS s1 s2 = proc (exs, ts) -> do
  ((nodeState1, _output1), ts1) <- s1 -< (exs, ts)
  s2_output <- s2 -< (exs, ts1)

  anyCollectorS -< (((nodeState1, _output1), ts1), s2_output)

-- | Most simple stateful condition
mkConditionS :: Monoid output => (exState -> Bool) -> StateBT exState tState output
mkConditionS f = ConditionS c
  where c = proc (exs, ts) -> do
          if f exs
            then
              returnA -< ((Success, mempty), ts)
            else
              returnA -< ((Failure, mempty), ts)

-- | Flattens the whole tree to a single arrow
-- Note: nodes with empty lists always crashes.
runSBT :: (Show exState, Show output, Monoid output) => StateBT exState tState output -> SF (exState, tState) ((BStatus, output), tState)
runSBT (ConditionS s) = s
runSBT (ActionS    s) = s
runSBT (FallbackS  s) = wireFallback s
  where wireFallback l = foldl1 orSFS (map runSBT l)
runSBT (SequenceS  s) = wireSeq s
  where wireSeq l = foldl1 andSFS (map runSBT l)

-- ######################################
-- Functorial Statefull behavior tree
-- ######################################

-- Refactor behavior tree type to allow functor instance

data FStateBT input output  = ConditionSF (SF input output)
                            | ActionSF    (SF input output)
                            | FallbackSF  [FStateBT input output]
                            | SequenceSF  [FStateBT input output]

instance Functor (FStateBT c) where
  fmap :: (a -> b) -> FStateBT c a -> FStateBT c b
  fmap f (ConditionSF x)  = ConditionSF $ fmap f x
  fmap f (ActionSF x)     = ActionSF $ fmap f x
  fmap f (FallbackSF l)   = FallbackSF $ fmap (fmap f) l
  fmap f (SequenceSF l)   = SequenceSF $ fmap (fmap f) l

-- | Flattens a functorial stateful behavior tree to a single arrow
-- It is specialized to a particular type of inputs and outputs. 
-- Note: nodes with empty lists always crashes.
runFSBT :: (Show exState, Show output, Monoid output) => FStateBT (exState, tState) ((BStatus, output), tState) -> SF (exState, tState) ((BStatus, output), tState)
runFSBT (ActionSF    s) = s
runFSBT (ConditionSF s) = s
runFSBT (FallbackSF  s) = wireFallback s
  where wireFallback l = foldl1 orSFS (map runFSBT l)
runFSBT (SequenceSF  s) = wireSeq s
  where wireSeq l = foldl1 andSFS (map runFSBT l)