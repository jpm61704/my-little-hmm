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

type Probability = CReal



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

stateCount :: (Traversable t) => HMM s o p t -> Int
stateCount = length . states

observationCount :: (Traversable t) => HMM s o p t -> Int
observationCount = length . observations

type Time = Int

data ObservationSequence o = ObsSeq { getObservationAt :: (Time -> o)
                                    , endOfSeq         :: Time
                                    }

fromList :: [o] -> ObservationSequence o
fromList os = ObsSeq (os !!) (length os)

data StateInfo s o p = StateInfo { pi_s :: p
                                 , p_inbound :: s -> p
                                 , p_outbound :: s -> p
                                 , p_obs :: o -> p
                                 }

-------------------------------------------------------------------------------------
-- ** Helper functions for the Reader Monad on HMM
-------------------------------------------------------------------------------------


getStates :: (Monad m) => ReaderT (HMM s o p t) m (t s)
getStates = reader states

foreachState :: (Traversable t, Monad m)
             => (a -> ReaderT (HMM a o p t) m b)
             -> ReaderT (HMM a o p t) m (t b)
foreachState f = do
  sts <- getStates
  forM sts f

foreachStateMap :: (Ord a, Traversable t, Monad m)
                => (a -> ReaderT (HMM a o p t) m b)
                -> ReaderT (HMM a o p t) m (M.Map a b)
foreachStateMap f = do
  sts <- getStates
  foldl (\mm x -> do
            m <- mm
            y <- f x
            return $ M.insert x y m
        ) (return M.empty) sts

getInitialProbability :: (Monad m) => s -> ReaderT (HMM s o p t) m p
getInitialProbability s = reader initialProbability >>= \f -> return $ f s

getObsProbability :: (Monad m) => s -> o -> ReaderT (HMM s o p t) m p
getObsProbability s o = reader observationProbability >>= \f -> return $ f s o

getStateTransPs :: (Monad m) => s -> s -> ReaderT (HMM s o p t) m p
getStateTransPs si sj = reader stateTransitions >>= \f -> return $ f si sj


-------------------------------------------------------------------------------------
-- * Viterbi Algorithm: Finding the most likely state sequence given an observation sequence (Problem 2)
-------------------------------------------------------------------------------------
{-
The only main difference between this algorithm and the one above is keeping track
of the psi-values for each state along the sequence and the use of max instead of
the sum. This should also help us generalize the above so that code reuse can be
maximized.

The most efficient data structure to keep track of the psi-values should be a regular
hash map.

-}

data ViterbiVariable p s = VV { delta :: p
                              , phi   :: s
                              } deriving (Eq)

instance (Ord p, Eq s) => Ord (ViterbiVariable p s) where
  a <= b = (delta a) <= (delta b)

nextViterbiVariable :: (Monad m, Traversable t, Functor t, Foldable t, Num p, Ord p, Ord s)
                     => (s -> p)
                     -> o
                     -> s
                     -> ReaderT (HMM s o p t) m (ViterbiVariable p s)
nextViterbiVariable v_i obs s_j = do
  b_j <- getObsProbability s_j obs
  (phi, amax) <- liftM argmax $ foreachStateMap $ \s_i -> do
    p_itoj <- (getStateTransPs s_i s_j)
    return $ p_itoj * (v_i s_i)
  let delta = b_j * amax
  return $ VV delta phi

type ViterbiMap p s = M.Map s (ViterbiVariable p s)

nextViterbiRow :: (Monad m, Traversable t, Num p, Ord p, Ord s)
               => (s -> p)
               -> o
               -> ReaderT (HMM s o p t) m (ViterbiMap p s)
nextViterbiRow v_i obs = foreachStateMap (nextViterbiVariable v_i obs)

viterbi :: (Num p, Ord p, Ord s, Traversable t, Show s)
        => HMM s o p t
        -> ObservationSequence o
        -> (p, StateSequence s)
viterbi hmm os = runReader (viterbiWReader os) hmm

viterbiWReader :: (Num p, Ord p, Ord s, Traversable t, Show s)
        => ObservationSequence o
        -> Reader (HMM s o p t) (p, StateSequence s)
viterbiWReader os = do
  let getObsAt = getObservationAt os
  v_1 <- initialViterbi $ getObsAt 0
  let initialMap = M.singleton 0 v_1

  -- The next line is really gross and needs to be cleaned up
  -- TODO: The line below is what is breaking shit
  vs <- F.foldlM (\m i -> nextViterbiRow (\s -> delta $ m <!!> (i,s)) (getObsAt i) >>= \x -> return (M.insert i x m)) initialMap $ [1..((endOfSeq os) - 1)]
  viterbiTerminate vs

argmax :: (Ord v) => M.Map k v -> (k, v)
argmax m = M.foldrWithKey (\k x mx@(k_m, v_m) -> if x > v_m then (k, x) else mx ) kv_1 m
  where kv_1 = M.elemAt 0 m

initialViterbi :: (Monad m, Num p, Ord s, Traversable t) => o -> ReaderT (HMM s o p t) m (ViterbiMap p s)
initialViterbi obs = foreachStateMap $ \s -> do
  b <- getObsProbability s obs
  y <- getInitialProbability s
  return $ VV (b * y) undefined

type StateSequence s = M.Map Time s

viterbiTerminate :: (Monad m, Ord p, Ord s) => M.Map Time (ViterbiMap p s) -> ReaderT (HMM s o p t) m (p, StateSequence s)
viterbiTerminate m = do
  let (t_max, t_map) = M.findMax m
      (q_T', vv) = argmax t_map
      (VV p' _) = vv
  -- next step is to finish up with backtracking
  return $ (p', backtrack q_T' m)

backtrack :: (Ord s) => s -> M.Map Time (ViterbiMap p s) -> StateSequence s
backtrack q_T' m = M.foldlWithKey findNextState (M.empty) m
  where findNextState ss t vmap = M.insert t (phi $ vmap M.! (ss M.! t)) ss

lookup2 :: (Ord k1, Ord k2) => k1 -> k2 -> M.Map k1 (M.Map k2 a) -> Maybe a
lookup2 k_outer k_inner m = do
  m' <- M.lookup k_outer m
  M.lookup k_inner m'

(<!!>) :: (Ord k1, Ord k2, Show k1, Show k2) => M.Map k1 (M.Map k2 a) -> (k1,k2) -> a
(<!!>) m (ko, ki) = case lookup2 ko ki m of
                      Just x -> x
                      Nothing -> error $ (show ko) ++ " -> " ++ (show ki) ++ " did not map to a value"


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

-- These are the type of ReaderT operations that describe evaluations of an HMM

type Alpha p = p

type AlphaMap s p = M.Map s p

type HMMComp s o p t env m = StateT env (ReaderT (HMM s o p t) m)


overStateSpace :: (Monad m, Ord s, Traversable t)
               => (s -> HMMComp s o p t st m a)
               -> HMMComp s o p t st m (M.Map s a)
overStateSpace f = do
  sts <- lift $ getStates
  foldl (\mm x -> do
            m <- mm
            y <- f x
            return $ M.insert x y m
         ) (return M.empty) sts


-- the alphamap in this type is the previous observation's alphas
type ForwardCalc s o p t m = HMMComp s o p t (Maybe (AlphaMap s p)) m


prob1 :: (Traversable t, Functor t, Num p, Foldable seq, Ord s) => HMM s o p t -> seq o -> p
prob1 hmm os = runIdentity $ runReaderT (evalStateT (runForwardCalc os) Nothing) hmm

runForwardCalc :: (Monad m, Ord s, Traversable t, Functor t, Num p, Foldable seq)
               => seq o
               -> ForwardCalc s o p t m p
runForwardCalc os = do
  forM_ os alphas
  sumState

sumState :: (Monad m, Num p) => ForwardCalc s o p t m p
sumState = gets $ sum . fromJust


alphas :: (Monad m, Num p, Traversable t, Ord s)
       => o
       -> ForwardCalc s o p t m ()
alphas obs = do
  first_pass <- firstPass
  as' <- overStateSpace $ if first_pass then alpha_1 obs else (alpha_t obs)
  put $ Just as'
  return ()


-- The computation below only relies on the HMM reader so it is lifted
-- into the ForwardCalc monad for brevity
alpha_1 :: (Monad m, Num p)
        => o
        -> s
        -> ForwardCalc s o p t m (Alpha p)
alpha_1 obs s_i = lift $ do
  b_i <- getObsProbability s_i obs
  pi <- getInitialProbability s_i
  return $ pi * b_i

alpha_t :: (Monad m, Num p, Foldable t, Ord s)
        => o
        -> s
        -> ForwardCalc s o p t m (Alpha p)
alpha_t obs s_j = do
  b_j <- lift $ getObsProbability s_j obs
  p_into <- pInto s_j -- this is the sum of previous alphas and transition probabilities
  return $ p_into * b_j

firstPass :: (Monad m) => ForwardCalc s o p t m Bool
firstPass = gets isNothing

-- ** Helpers for alpha_t

pInto :: (Monad m, Num p, Foldable t, Ord s) => s -> ForwardCalc s o p t m p
pInto s_j = do
  states <- lift $ getStates
  a_alphaproductsum s_j states

a_alphaproductsum :: (Monad m, Num p, Foldable t, Ord s) => s -> t s -> ForwardCalc s o p t m p
a_alphaproductsum s_j sts = foldrM (return 0) sts $ \s_i sm -> do
  sum <- sm
  a_ij <- lift $ getStateTransPs s_i s_j
  alpha_ti <- getAlphaFor s_i
  return $ sum + (a_ij * alpha_ti)

getAlphaFor :: (Monad m, Ord s) => s -> ForwardCalc s o p t m (Alpha p)
getAlphaFor s = gets $ \mm -> case mm of
                               Just m -> m M.! s
                               Nothing -> error "error in getAlphaFor, queried nothing"


foldrM :: (Foldable t) => b -> (t a) -> (a -> b -> b) -> b
foldrM x y z = foldr z x y

-- continue with fold definition

-------------------------------------------------------------------------------------
-- * Example
-------------------------------------------------------------------------------------

data Weather = Start | Hot | Cold deriving (Eq, Show, Ord)
data IceCreams = One | Two | Three deriving (Eq, Show, Ord)

obs_prob :: Weather -> IceCreams -> Probability
obs_prob Hot n = case n of
                   One   -> 0.2
                   Two   -> 0.4
                   Three -> 0.4
obs_prob Cold n = case n of
                    One   -> 0.5
                    Two   -> 0.4
                    Three -> 0.1
obs_prob _ _ = error "Start or Ends cant eat icecream"

trans_prob :: Weather -> Weather -> Probability
trans_prob Start w = case w of
                       Hot  -> 0.8
                       Cold -> 0.2
                       _    -> 0
trans_prob Hot w = case w of
                     Hot  -> 0.7
                     Cold -> 0.3
trans_prob Cold w = case w of
                      Hot  -> 0.4
                      Cold -> 0.6

exampleHMM :: HMM Weather IceCreams Probability []
exampleHMM = HMM [Hot, Cold] [One, Two, Three] (trans_prob Start) (trans_prob) (obs_prob)

trial1 :: [IceCreams]
trial1 = [Three, Three, One, One, Two, Three, Three, One, Two]

trial2 :: [IceCreams]
trial2 = [Three, Three, One, One, Two, Two, Three, One, Three]
