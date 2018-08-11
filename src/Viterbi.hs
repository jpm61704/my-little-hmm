module Viterbi where

import HMM
import ForwardAlgorithm
import qualified Data.Sequence as S
import Data.Foldable
import qualified Data.Map as M


-- * Decoding (Problem 2)

decode :: (Traversable t, Functor t, Num p, Ord p, Foldable seq, Ord s)
       => HMM s o p t -> ObservationSeq o seq -> V s p
decode hmm = (maximum . viterbis_T hmm)

-- ** Viterbi Variable

-- | The viterbi variable aggregate which includes the probability
-- and (most likely) previous state for backtracing
data V s p = V s p
           | Vi p
           deriving (Show, Eq, Ord)

instance Functor (V s) where
  fmap f (V s p) = V s $ f p

vNil :: (Num p) => V s p
vNil = Vi 0

probv :: V s p -> p
probv (Vi x) = x
probV (V _ x) = x

type ViterbiMap s p = M.Map s (V s p)

viterbis_T :: (Foldable seq, Num p, Ord p, Foldable t, Ord s)
           => HMM s o p t
           -> ObservationSeq o seq
           -> ViterbiMap s p
viterbis_T hmm (ObsSeq fst rst) = foldl v_t v_1 rst
  where v_t vm o = viterbis_t vm hmm o
        v_1      = viterbis_1 hmm fst

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
  where f s_i v_i max@(V s_max v_max) = let a = p_inbound st_info s_i
                                            v' = a * (probv v_i)
                                            max' = V s_i v'
                                        in if v_max > v' then max else max'



-- pIntoG'' :: (Num p) => (p -> p -> a) -> StateInfo s o p -> M.Map s a -> a
-- pIntoG'' f st_info a_map = M.foldrWithKey sumIncomingProbability 0 a_map
--   where sumIncomingProbability s_i alpha_i sum = let a = p_inbound st_info s_i
--                                                  in f sum (a * alpha_i)

-- selector :: (p -> p -> Ordering) -> s -> s -> p -> p -> (p,s)
-- selector f s_1 s_2 p_1 p_2 = case f p_1 p_2 of
--                                GT -> (p_1, s_1)
--                                otherwise -> (p_2, s_2)

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

-- data ViterbiVariable p s = VV { delta :: p
--                               , phi   :: s
--                               } deriving (Eq)

-- instance (Ord p, Eq s) => Ord (ViterbiVariable p s) where
--   a <= b = (delta a) <= (delta b)

-- nextViterbiVariable :: (Monad m, Traversable t, Functor t, Foldable t, Num p, Ord p, Ord s)
--                      => (s -> p)
--                      -> o
--                      -> s
--                      -> ReaderT (HMM s o p t) m (ViterbiVariable p s)
-- nextViterbiVariable v_i obs s_j = do
--   b_j <- getObsProbability s_j obs
--   (phi, amax) <- liftM argmax $ foreachStateMap $ \s_i -> do
--     p_itoj <- (getStateTransPs s_i s_j)
--     return $ p_itoj * (v_i s_i)
--   let delta = b_j * amax
--   return $ VV delta phi

-- type ViterbiMap p s = M.Map s (ViterbiVariable p s)

-- nextViterbiRow :: (Monad m, Traversable t, Num p, Ord p, Ord s)
--                => (s -> p)
--                -> o
--                -> ReaderT (HMM s o p t) m (ViterbiMap p s)
-- nextViterbiRow v_i obs = foreachStateMap (nextViterbiVariable v_i obs)

-- viterbi :: (Num p, Ord p, Ord s, Traversable t, Show s)
--         => HMM s o p t
--         -> ObservationSequence o
--         -> (p, StateSequence s)
-- viterbi hmm os = runReader (viterbiWReader os) hmm

-- viterbiWReader :: (Num p, Ord p, Ord s, Traversable t, Show s)
--         => ObservationSequence o
--         -> Reader (HMM s o p t) (p, StateSequence s)
-- viterbiWReader os = do
--   let getObsAt = getObservationAt os
--   v_1 <- initialViterbi $ getObsAt 0
--   let initialMap = M.singleton 0 v_1

--   -- The next line is really gross and needs to be cleaned up
--   -- TODO: The line below is what is breaking shit
--   vs <- F.foldlM (\m i -> nextViterbiRow (\s -> delta $ m <!!> (i,s)) (getObsAt i) >>= \x -> return (M.insert i x m)) initialMap $ [1..((endOfSeq os) - 1)]
--   viterbiTerminate vs

-- argmax :: (Ord v) => M.Map k v -> (k, v)
-- argmax m = M.foldrWithKey (\k x mx@(k_m, v_m) -> if x > v_m then (k, x) else mx ) kv_1 m
--   where kv_1 = M.elemAt 0 m

-- initialViterbi :: (Monad m, Num p, Ord s, Traversable t) => o -> ReaderT (HMM s o p t) m (ViterbiMap p s)
-- initialViterbi obs = foreachStateMap $ \s -> do
--   b <- getObsProbability s obs
--   y <- getInitialProbability s
--   return $ VV (b * y) undefined

-- type StateSequence s = M.Map Time s

-- viterbiTerminate :: (Monad m, Ord p, Ord s) => M.Map Time (ViterbiMap p s) -> ReaderT (HMM s o p t) m (p, StateSequence s)
-- viterbiTerminate m = do
--   let (t_max, t_map) = M.findMax m
--       (q_T', vv) = argmax t_map
--       (VV p' _) = vv
--   -- next step is to finish up with backtracking
--   return $ (p', backtrack q_T' m)

-- backtrack :: (Ord s) => s -> M.Map Time (ViterbiMap p s) -> StateSequence s
-- backtrack q_T' m = M.foldlWithKey findNextState (M.empty) m
--   where findNextState ss t vmap = M.insert t (phi $ vmap M.! (ss M.! t)) ss

-- lookup2 :: (Ord k1, Ord k2) => k1 -> k2 -> M.Map k1 (M.Map k2 a) -> Maybe a
-- lookup2 k_outer k_inner m = do
--   m' <- M.lookup k_outer m
--   M.lookup k_inner m'

-- (<!!>) :: (Ord k1, Ord k2, Show k1, Show k2) => M.Map k1 (M.Map k2 a) -> (k1,k2) -> a
-- (<!!>) m (ko, ki) = case lookup2 ko ki m of
--                       Just x -> x
--                       Nothing -> error $ (show ko) ++ " -> " ++ (show ki) ++ " did not map to a value"

