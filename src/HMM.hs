module HMM where

import qualified Data.Foldable    as F
import qualified Data.Matrix      as M
import qualified Data.Traversable as T
import qualified Data.Vector      as V
import Data.Ratio
import Data.Number.CReal
import Control.Monad.Reader

type Probability = CReal



-------------------------------------------------------------------------------------
-- * Hidden Markov Models
-------------------------------------------------------------------------------------

-- | The definition of a hidden markov model
--   s := data type of states
--   o := data type of observations
--   p := data type of probabilities
--   t := a traversable functor to contain the probabilities
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

data ObservationSequence t o = ObsSeq { getObservationAt :: (t -> o)
                                      , endOfSeq :: t
                                      }


-------------------------------------------------------------------------------------
-- ** Helper functions for the Reader Monad on HMM
-------------------------------------------------------------------------------------


getStates :: Reader (HMM s o p t) (t s)
getStates = reader states

foreachState :: Traversable t
             => (a -> Reader (HMM a o p t) b)
             -> Reader (HMM a o p t) (t b)
foreachState f = do
  sts <- getStates
  forM sts f

getInitialProbability :: s -> Reader (HMM s o p t) p
getInitialProbability s = reader initialProbability >>= \f -> return $ f s

getObsProbability :: s -> o -> Reader (HMM s o p t) p
getObsProbability s o = reader observationProbability >>= \f -> return $ f s o

getStateTransPs :: s -> s -> Reader (HMM s o p t) p
getStateTransPs si sj = reader stateTransitions >>= \f -> return $ f si sj

-------------------------------------------------------------------------------------
-- * Finding the probability of a sequence of observations(Problem 1)
-------------------------------------------------------------------------------------

prob1 :: (Traversable t, Functor t, Num p) => HMM s o p t -> [o] -> p
prob1 hmm os = sum alphas
  where alphas = runReader (forwardVariable os) hmm

-- | The probability of a sequence of observations AND a state at time t given the model
-- a_t(i) = P(O_1, O_2, ... O_t, q_t = S_i | lambda)
forwardVariable :: (Traversable t, Num p)
                => [o]
                -> Reader (HMM s o p t) (t p)
forwardVariable (o:os) = do
  alpha1 <- initialForwardVariable o
  F.foldlM nextForwardVariable alpha1 os


initialForwardVariable :: (Traversable t, Functor t, Num p)
                       => o
                       -> Reader (HMM s o p t) (t p)
initialForwardVariable obs = abstractForwardVariableCalculation obs getInitialProbability

nextForwardVariable :: (Traversable t, Functor t, Foldable t, Num p)
                    => t p
                    -> o
                    -> Reader (HMM s o p t) (t p)
nextForwardVariable prev_row obs = abstractForwardVariableCalculation obs $ \s -> do
  trans <- foreachState (\si -> getStateTransPs si s)
  return $ sum $ zipWithT (*) prev_row trans -- can be generalized to a fold

abstractForwardVariableCalculation :: (Traversable t, Num p)
                                   => o
                                   -> (s -> Reader (HMM s o p t) p)
                                   -> Reader (HMM s o p t) (t p)
abstractForwardVariableCalculation obs getAlpha =
  foreachState $ \s -> do
    b <- getObsProbability s obs
    y <- getAlpha s
    return $ b * y

-- SOURCE OF INEFFICIENCY
zipT :: (Traversable t, Foldable t) => t a -> t a -> t (a, a)
zipT x y = snd $ T.mapAccumL f (F.toList y) x
  where f (y':ys) x' = (ys, (x',y'))

-- This improves on zipT by doing everything in one pass
zipWithT :: (Traversable t, Foldable t) => (a -> a -> b) -> t a -> t a -> t b
zipWithT combine x y = snd $ T.mapAccumL f (F.toList y) x
  where f (y':ys) x' = (ys, (combine x' y'))

-------------------------------------------------------------------------------------
-- * Example
-------------------------------------------------------------------------------------

data Weather = Start | Hot | Cold deriving (Eq, Show)
data IceCreams = One | Two | Three deriving (Eq, Show)

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
                       Hot -> 0.8
                       Cold -> 0.2
                       _ -> 0
trans_prob Hot w = case w of
                     Hot -> 0.7
                     Cold -> 0.3
trans_prob Cold w = case w of
                      Hot -> 0.4
                      Cold -> 0.6

exampleHMM :: HMM Weather IceCreams Probability []
exampleHMM = HMM [Hot, Cold] [One, Two, Three] (trans_prob Start) (trans_prob) (obs_prob)

trial1 :: [IceCreams]
trial1 = [Three, Three, One, One, Two, Three, Three, One, Two]

trial2 :: [IceCreams]
trial2 = [Three, Three, One, One, Two, Two, Three, One, Three]
