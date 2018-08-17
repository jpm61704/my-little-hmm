module BackwardAlgorithm where

import HMM
import qualified Data.Map as M
import qualified Data.Sequence as S

-- | Label Type for the Backward Variable(Beta)
type Beta p = p

type BetaMap s p = M.Map s p

-- | The initialization step for the backward algorithm
beta_T :: (Num p) => st -> Beta p
beta_T s_i = fromInteger $ 1

betas_T :: (Num p, Foldable t, Ord s)
        => HMM s o p t -> BetaMap s p
betas_T hmm = overStateSpace hmm (\s -> fromInteger 1)

beta_t :: (Foldable t, Num p, Ord s) => HMM s o p t -> BetaMap s p -> o -> s -> Beta p
beta_t hmm betaMap obs s_i = foreachState hmm f 0
  where f s_j acc = let stinfo_j = stateInfo hmm s_j
                        a_ij   = p_inbound stinfo_j s_i
                        b_j    = p_obs stinfo_j obs
                        beta_j = betaMap M.! s_j
                    in acc + (a_ij * b_j * beta_j)

-- | calculation of one time unit's beta variables
betas_t :: (Foldable t, Num p, Ord s) => HMM s o p t -> o -> BetaMap s p -> BetaMap s p
betas_t hmm o bmap = overStateSpace hmm $ beta_t hmm bmap o

{- |
calculates the beta values for the first observation in a sequence

Very sure this is not a very efficient implementation
-}
betas :: (Foldable t, Num p, Ord s) => HMM s o p t -> ObservationSeq o -> BetaMap s p
betas hmm (ObsSeq os) = case S.viewr os of
                          S.EmptyR -> error "empty observation sequence"
                          rest S.:> o_T -> foldr (betas_t hmm) b_T rest -- MAYBEBUG: This may need to be foldl
                            where b_T = overStateSpace hmm beta_T
