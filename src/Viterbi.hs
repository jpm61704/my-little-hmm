module Viterbi where

import HMM
import ForwardAlgorithm
import Data.Sequence
import Data.Foldable
import qualified Data.Map as M


-- * Decoding (Problem 2)

decode :: (Traversable t, Functor t, Num p, Ord p, Ord s)
       => HMM s o p t -> ObservationSeq o -> V s p
decode hmm obseq = maximum $ overStateSpace hmm (\st -> let inf = stateInfo hmm st in pIntoV inf v_T)
  where v_T = viterbis_T hmm obseq

-- ** Viterbi Variable

-- | The viterbi variable aggregate which includes the probability
-- and (most likely) previous state for backtracing
data V s p = V (Seq s) p
           | Vi p
           deriving (Show, Eq)

instance (Eq s, Eq p, Ord p) => Ord (V s p) where
  compare (V _ p1) (V _ p2) = if p1 > p2 then GT else LT

instance Functor (V s) where
  fmap f (V s p) = V s $ f p


-- | an empty viterbi variable for use in the v_1 step
vNil :: (Num p) => V s p
vNil = Vi 0

-- | The probability of a Viterbi Variable
probv :: V s p -> p
probv (V _ x) = x
probv (Vi x) = x

-- | The backtrace of a Viterbi Variable(for V_i it is an empty sequence)
seqV :: V s p -> Seq s
seqV (Vi _) = empty
seqV (V s _) = s

-- | the chaining of viterbi variables, maintains the backtrace while adding a new
-- state and corresponding probability
chain :: s -> p -> V s p -> V s p
chain st p' (V seq p) = V (seq |> st) p'
chain st p' (Vi p) = V (pure st) p'

-- | Type Synonym for Maps of viterbi variables corresponding to state over the observation domain
type ViterbiMap s p = M.Map s (V s p)


-- *** Viterbi Variable Calculations

viterbis_T :: (Num p, Ord p, Foldable t, Ord s)
           => HMM s o p t
           -> ObservationSeq o
           -> ViterbiMap s p
viterbis_T hmm obsSeq = foldl v_t v_1 (rest obsSeq)
  where v_t vm o = viterbis_t vm hmm o
        v_1      = viterbis_1 hmm $ first obsSeq

-- | viterbi variable for the first observation(O_1) in a sequence
viterbis_1 :: (Num p, Ord s, Foldable t) => HMM s o p t -> o -> ViterbiMap s p
viterbis_1 hmm obs = fmap Vi $ alphas' (alpha1) hmm obs

viterbis_t :: (Num p, Ord p, Foldable t, Ord s) => ViterbiMap s p -> HMM s o p t -> o -> ViterbiMap s p
viterbis_t vmap hmm obs = overStateSpace hmm $ \st -> viterbi_t vmap (stateInfo hmm st) obs

viterbi_t :: (Num p, Ord p) => ViterbiMap s p -> StateInfo s o p -> o -> V s p
viterbi_t vmap stinfo_j obs = fmap (* b_j) p_into
  where b_j = p_obs stinfo_j obs
        p_into = pIntoV stinfo_j vmap

-- * Utility Functions

-- | calculations the maximal value and its argument when a function f is applied to
-- a foldable functor
argmax :: (Ord a, Foldable t, Functor t) => (b -> a) -> t b -> (a, b)
argmax f xs = maximumBy (\(fx, x) (fy, y) -> if max fx fy == fx then GT else LT) $ fmap (\x -> (f x, x)) xs

pIntoV :: (Num p, Ord p) => StateInfo s o p -> M.Map s (V s p) -> (V s p)
pIntoV st_info v_map = M.foldrWithKey f vNil v_map
  where f s_i v_i max = let a = p_inbound st_info s_i
                            v' = a * (probv v_i)
                            seqv = seqV v_i
                            max' = chain s_i v' v_i
                            v_max = probv max
                        in if v_max > v' then max else max'


