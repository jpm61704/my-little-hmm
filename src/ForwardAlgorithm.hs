module ForwardAlgorithm where

import HMM
import qualified Data.Map.Lazy as M
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Maybe

-- * Likelihood (Problem 1)

{- |
the probability of an observation sequence occurring given a model

this is problem 1, or the likelihood problem, in most HMM texts
-}
likelihood :: (Traversable t, Functor t, Num p, Foldable seq, Ord s)
       => HMM s o p t
       -> ObservationSeq o seq
       -> p
likelihood hmm = (sum . alphas_T hmm)


-- ** Calculation of the Forward Variable(Alpha)

type Alpha p = p

type AlphaMap s p = M.Map s p


-- | The alpha calculation made for an entire observation sequence
-- This is summed over to achieve the probability of a sequence occurring
alphas_T :: (Traversable t, Functor t, Num p, Foldable seq, Ord s)
       => HMM s o p t
       -> ObservationSeq o seq
       -> AlphaMap s p
alphas_T hmm (ObsSeq frst rst) = foldl al_t al_1 rst
  where al_t am o = alphas_t am hmm o
        al_1 = alphas_1 hmm frst

-- | the alphas for observation o_t with respect to the alphas from the
-- previous observation.
alphas_t :: (Num p, Ord s, Foldable t)
         => AlphaMap s p -> HMM s o p t -> o -> AlphaMap s p
alphas_t amap = alphas' (alpha_t amap)

-- | The alphas for observation o_1 given a model
alphas_1 :: (Num p, Ord s, Foldable t)
         => HMM s o p t -> o -> AlphaMap s p
alphas_1 = alphas' (alpha1)

-- | the abstract calculation for the alphas of an observation o with respect
-- to a model and observation. The first argument is a function that calculates
-- the alpha value for a given state s_i given its StateInfo view and the observation. \\
-- This is used to provide an abstraction so that addition data can be curried into the
-- function before alpha's is called.
alphas' :: (Num p, Ord s, Foldable t)
        => (StateInfo s o p -> o -> Alpha p) -> HMM s o p t -> o -> AlphaMap s p
alphas' f hmm obs = overStateSpace hmm op
  where op s = f (stateInfo hmm s) obs

-- | the calculation of a forward variable with respect to the first observation in a sequence
alpha1 :: (Num p)
       => StateInfo s o p -> o -> Alpha p
alpha1 st_info obs = pi * b_i
  where b_i = p_obs st_info obs
        pi   = pi_s st_info

-- | the calculation for a forward variable for any (non-initial) observation with respect to
-- the previous observation's forward variables, the current state's(s_j) state info
alpha_t :: (Num p) => AlphaMap s p -> StateInfo s o p -> o -> Alpha p
alpha_t a_map stinfo_j obs = p_into * b_j
  where b_j = p_obs stinfo_j obs
        p_into = pInto stinfo_j a_map

-- | The probability of transitioning into state s_j from all other states given the forward
-- variable from the previous observation
pInto :: (Num p) => StateInfo s o p -> AlphaMap s p -> p
pInto = pIntoG (+)



