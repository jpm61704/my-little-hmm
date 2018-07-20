module HMM where

import qualified Data.Foldable    as F
import qualified Data.Matrix      as M
import qualified Data.Traversable as T
import qualified Data.Vector      as V
import Data.Ratio
import Data.Number.CReal

type Probability = CReal

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

prob1 :: (Traversable t, Functor t, Num p) => HMM s o p t -> [o] -> p
prob1 hmm (o:os) = sum $ foldl (\alpha_prev o_t -> nextForwardVariable hmm alpha_prev o_t) (initialForwardVariable hmm o) os

initialForwardVariable :: (Functor t, Num p) => HMM s o p t -> o -> t p
initialForwardVariable hmm obs = fmap initialAlpha (states hmm)
  where initialAlpha s = (initialProbability hmm s) * (observationProbability hmm s obs)

nextForwardVariable :: (Traversable t, Functor t, Foldable t, Num p) => HMM s o p t -> t p -> o -> t p
nextForwardVariable hmm prev_row obs = fmap nextAlpha (states hmm)
  where nextAlpha s = prevRowSum * observationProbability hmm s obs
          where prevRowSum = sum $ fmap (\(x,y)->x*y) $ zipT prev_row $ fmap (\sj -> stateTransitions hmm sj s) (states hmm)

-- SOURCE OF INEFFICIENCY
zipT :: (Traversable t, Foldable t) => t a -> t a -> t (a, a)
zipT x y = snd $ T.mapAccumL f (F.toList y) x
  where f (y':ys) x' = (ys, (x',y'))

-- * Example

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
