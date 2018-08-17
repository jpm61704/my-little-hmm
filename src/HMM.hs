module HMM where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import Control.Monad.Identity
import qualified Data.Foldable        as F
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Number.CReal
import           Data.Ratio
import qualified Data.Traversable     as T
import qualified Data.Vector          as V
import qualified Data.Sequence as S



-------------------------------------------------------------------------------------
-- * Hidden Markov Models
-------------------------------------------------------------------------------------

-- | The definition of a hidden markov model
--   s := data type of states
--   o := data type of observations
--   p := data type of probabilities
--   t := a traversable functor to contain the states and observations
data HMM s o p t = HMM { states                 :: t s
                       , observations           :: t o
                       , initialProbability     :: s -> p
                       , stateTransitions       :: s -> s -> p
                       , observationProbability :: s -> o -> p
                       }


data StateInfo s o p = StateInfo { pi_s :: p
                                 , p_inbound :: s -> p
                                 , p_outbound :: s -> p
                                 , p_obs :: o -> p
                                 }

stateInfo :: HMM s o p t -> s -> StateInfo s o p
stateInfo hmm st = StateInfo pi p_i p_o p_os
  where pi = initialProbability hmm st
        p_i = (flip (stateTransitions hmm)) st
        p_o = stateTransitions hmm st
        p_os = observationProbability hmm st


overStateSpace :: (Foldable t, Ord s)
                => HMM s o p t -> (s -> a) -> M.Map s a
overStateSpace hmm f = foldr (\s mm -> M.insert s (f s) mm) (M.empty) $ states hmm

overObsSpace :: (Foldable t, Ord o)
                => HMM s o p t -> (o -> a) -> M.Map o a
overObsSpace hmm f = foldr (\s mm -> M.insert s (f s) mm) (M.empty) $ observations hmm


foreachState :: (Foldable t)
             => HMM s o p t -> (s -> a -> a) -> a -> a
foreachState hmm f x = foldr f x $ states hmm

data BlankState = BState Integer deriving (Eq, Ord)

instance Show BlankState where
  show (BState n) = "S" ++ subscript n

subscript :: Integer -> String
subscript n = map subs $ show n
  where subs c = case c of
                   '0' -> '₀'
                   '1' -> '₁'
                   '2' -> '₂'
                   '3' -> '₃'
                   '4' -> '₄'
                   '5' -> '₅'
                   '6' -> '₆'
                   '7' -> '₇'
                   '8' -> '₈'
                   '9' -> '₉'
                   x -> x

newHMM :: (Fractional p) => Integer -> [o] -> HMM BlankState o p []
newHMM num_states obs = HMM sts obs pi a b
  where sts = map BState [1..num_states]
        n_sts = fromInteger num_states
        n_obs = fromInteger $ toInteger $ length obs
        pi = (\_ -> 1 / n_sts)
        a = (\_ _ -> 1 / n_sts)
        b = (\_ _ -> 1 / n_obs)

randHMM :: (Fractional p) => Integer -> [o] -> [p] ->  HMM BlankState o p []
randHMM num_states obs rands = HMM sts obs pi a b
  where sts = map BState [1..num_states]
        n_sts = fromInteger num_states
        n_obs = fromInteger $ toInteger $ length obs
        pi = (\_ -> 1 / n_sts)
        a = (\_ _ -> 1 / n_sts)
        b = (\_ _ -> 1 / n_obs)

newtype ObservationSeq o = ObsSeq (S.Seq o)

first :: ObservationSeq o -> o
first (ObsSeq oseq) = case S.viewl oseq of
                        S.EmptyL -> error "empty seq"
                        x S.:< rest -> x

rest :: ObservationSeq o -> S.Seq o
rest (ObsSeq oseq) = case S.viewl oseq of
                        S.EmptyL -> error "empty seq"
                        x S.:< rest -> rest

fromList :: [o] -> ObservationSeq o
fromList = ObsSeq . S.fromList

obsAt :: ObservationSeq o -> Int -> o
obsAt (ObsSeq s) i = S.index s i

data StateSeq o seq = StateSeq (seq o)


-- | a function applied to the probabilities of transition(a_ij * alpha_i) from s_i to s_j
pIntoG :: (Num p) => (p -> p -> p) -> StateInfo s o p -> M.Map s p -> p
pIntoG f st_info a_map = M.foldrWithKey sumIncomingProbability 0 a_map
  where sumIncomingProbability s_i alpha_i sum = let a = p_inbound st_info s_i
                                                 in f sum (a * alpha_i)


accumMultiply :: (Num p) => (p -> p -> p) -> StateInfo s o p -> s -> p -> p -> p
accumMultiply f st_info s_i alpha_i acc = f acc (a * alpha_i)
  where a = p_inbound st_info s_i

-------------------------------------------------------------------------------------
-- ** Helper functions for Monads
-------------------------------------------------------------------------------------

-- *** HMMComp - Stateful actions within an HMM environment(reader)

-- type HMMComp s o p t env m = StateT env (ReaderT (HMM s o p t) m)

-- overStateSpace :: (Monad m, Ord s, Traversable t)
--                => (s -> HMMComp s o p t st m a)
--                -> HMMComp s o p t st m (M.Map s a)
-- overStateSpace f = do
--   sts <- lift $ getStates
--   foldl (\mm x -> do
--             m <- mm
--             y <- f x
--             return $ M.insert x y m
--          ) (return M.empty) sts

-- -- *** ReaderT functions

-- getStates :: (Monad m) => ReaderT (HMM s o p t) m (t s)
-- getStates = reader states

-- foreachState :: (Traversable t, Monad m)
--              => (a -> ReaderT (HMM a o p t) m b)
--              -> ReaderT (HMM a o p t) m (t b)
-- foreachState f = do
--   sts <- getStates
--   forM sts f

-- foreachStateMap :: (Ord a, Traversable t, Monad m)
--                 => (a -> ReaderT (HMM a o p t) m b)
--                 -> ReaderT (HMM a o p t) m (M.Map a b)
-- foreachStateMap f = do
--   sts <- getStates
--   foldl (\mm x -> do
--             m <- mm
--             y <- f x
--             return $ M.insert x y m
--         ) (return M.empty) sts

-- getInitialProbability :: (Monad m) => s -> ReaderT (HMM s o p t) m p
-- getInitialProbability s = reader initialProbability >>= \f -> return $ f s

-- getObsProbability :: (Monad m) => s -> o -> ReaderT (HMM s o p t) m p
-- getObsProbability s o = reader observationProbability >>= \f -> return $ f s o

-- getStateTransPs :: (Monad m) => s -> s -> ReaderT (HMM s o p t) m p
-- getStateTransPs si sj = reader stateTransitions >>= \f -> return $ f si sj

-- getStateInfo :: (Monad m) => s -> ReaderT (HMM s o p t) m (StateInfo s o p)
-- getStateInfo st = reader $ flip stateInfo st


-- NOTES
{-
========================================
Creating an HMM Computation Monad Stack
========================================

I want to create a Monad stack that:

a.) is safe and doesn't allow for bogus computations
b.) allows for the following variables to be calculated somewhat easily:
    - alpha_t(i) = P(O_1 ... O_t, q_t = S_i | lambda)
    - beta_t(i)  = P(O_(t+1) ... O_T | q_t = S_i, lambda)
    - gamma_t(i) = P(q_t = S_i | OSeq, lambda)


We have some parts of the computation
a.) information from without(input variables)
    - these data can be present at different "levels" of the environment
      example: the hmm configuration has a higher order than state and observation
               information
b.) contributing calculations from within(memoized or cps data)
-}


-------------------------------------------------------------------------------------
-- * Towards a more declarative algorithm
-------------------------------------------------------------------------------------
{-
While working the above problems out, it came to me that it would make sense to wrap
all of these algorithms in a WriterT StateT Monad stack so that constants, such as the
HMM parameters, and variables, such as alpha, beta, etc, can be seperated but still
accessed in a convenient way.

The downside to this approach is that this essentially creates an imperative sandbox
inside of our functional space. The better approach would be to create a declarative
space to built out these problems. The issue with this is mostly that many of the
values present in these algorithms depend(in a recursive manner) on values before them.

This trait is well suited to be solved in a monadic space as mentioned above, but it
is less than desirable to remain consistent with my usual design methodology.

The likely solution to this problem is creating more data types in order to abstract
the problem so that it doesn't call for the use of side effects in the way prescribed
in the current solution for step 1.

For instance we could create a data type called Node that captures the notion of a
single observation or state in an observation sequence. One possibility is that a
fold exists where a Node starts off in some super-state and is collapsed into a solution
value. This idea is what I think is really trying to be captured in the algorithms that
deal with HMMs.

The example in the tutorial paper has a bunch of diagrams that are nodes being connected
to every node in the next, previous, or both directions with respect to time. This appears
to be at least a slightly more declarative version of the algorithms presented in the
normal mathematical notation.
-}

