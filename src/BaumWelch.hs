{-# LANGUAGE BangPatterns #-}
module BaumWelch where

import HMM
import ForwardAlgorithm
import BackwardAlgorithm
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Foldable
import Control.Monad.Reader
import Control.Monad.Identity
import Debug.Trace
import Pretty


monitoredTraining :: (Traversable t, Fractional p, Show s, Show o, Show p, Ord s, Ord o)
                  => ObservationSeq o -> Int -> Int -> HMM s o p t -> IO (HMM s o p t)
monitoredTraining os 0 sep hmm = return hmm
monitoredTraining os n sep hmm = do
      let hmm' = reestimate os hmm
      if mod n sep == 0 then putStr $ showHMM hmm' else return ()
      monitoredTraining os (n-1) sep hmm'

-- * Estimation Step

type EstimationStep s o p t m = ReaderT (TimeMap (TimeData s o p), HMM s o p t, p) m

getsTM :: (Monad m) => (TimeMap (TimeData s o p) -> a) -> EstimationStep s o p t m a
getsTM f = do
  (tm,_,_) <- ask
  return $ f tm

getsTD :: (Monad m) => Int -> (TimeData s o p -> a) -> EstimationStep s o p t m a
getsTD i f = getsTM (IM.! i) >>= return . f

getAlpha :: (Monad m, Num p, Ord s) => Int -> s -> EstimationStep s o p t m (Alpha p)
getAlpha i s_i = getsTD i (\td -> alpha td M.! s_i)

getBeta :: (Monad m, Num p, Ord s) => Int -> s -> EstimationStep s o p t m (Beta p)
getBeta i s_i = getsTD i (\td -> beta td M.! s_i)

getObs :: (Monad m) => Int -> EstimationStep s o p t m o
getObs = flip getsTD t_obs

getsHMM :: (Monad m) => (HMM s o p t -> a) -> EstimationStep s o p t m a
getsHMM f = do
  (_,hmm,_) <- ask
  return $ f hmm

getA_ij :: (Monad m) => s -> s -> EstimationStep s o p t m p
getA_ij s_i s_j = getsHMM $ \(HMM _ _ _ a _) -> a s_i s_j

getB_j :: (Monad m) => s -> EstimationStep s o p t m (o -> p)
getB_j s_j = getsHMM $ \(HMM _ _ _ _ b) -> b s_j

getLikelihood :: (Monad m) => EstimationStep s o p t m p
getLikelihood = do
  (_,_,l) <- ask
  return l

calcXi :: (Monad m, Fractional p, Ord s) => Int -> s -> s -> EstimationStep s o p t m p
calcXi t s_i s_j =  do
  alpha <- getAlpha t s_i
  beta <- getBeta (t+1) s_j -- BUG: This will cause an error on termination step
  a_ij <- getA_ij s_i s_j
  b_j <- getB_j s_j
  onext <- getObs (t+1)
  lhood <- getLikelihood
  return $ (alpha * beta * a_ij * b_j onext) / lhood

{- |
Note: Source of inefficiency because this recalculations all Xi a bunch of times
-}
calcGamma' :: (Traversable t, Monad m, Num p, Fractional p, Foldable t, Ord s)
          => Int -> s -> EstimationStep s o p t m p
calcGamma' t s_i = do
  sts <- getsHMM states
  xis <- forM sts $ calcXi t s_i
  return $ sum xis

calcGamma :: (Traversable t, Monad m, Num p, Fractional p, Foldable t, Ord s)
          => Int -> s -> EstimationStep s o p t m p
calcGamma t s_i = do
  alpha <- getAlpha t s_i
  beta <- getBeta t s_i
  lhood <- getLikelihood
  return $ (alpha * beta) / lhood

estData_t :: (Traversable t, Monad m, Ord s, Num p, Fractional p)
          => Int -> EstimationStep s o p t m (EstimationData s o p)
estData_t t = do
  sts <- getsHMM states
  xit <- forM sts $ \s_i -> do
    xs <- forM sts $ \s_j -> do
      x <- calcXi t s_i s_j
      return $ M.singleton s_j x
    let xs' = foldr M.union M.empty xs
    return $ M.singleton s_i xs'
  let xi = foldr M.union M.empty xit
  gmt <- forM sts $ \st -> do
    g <- calcGamma t st
    return $ M.singleton st g
  let gm = foldr M.union M.empty gmt
  obs <- getObs t
  return $ EstData gm xi obs

estData :: (Traversable t, Monad m, Ord s, Num p, Fractional p)
        => EstimationStep s o p t m (TimeMap (EstimationData s o p))
estData = do
  x <- getsTM IM.size
  dt <- forM [0..(x-1)] $ \i -> do
    e <- estData_t i
    return (i,e)
  return $ IM.fromList dt

-- * Maximization Step
{- $doc
We can reuse the estimation monad stack for this section
-}

type MaximizationStep s o p t = ReaderT (TimeMap (EstimationData s o p), HMM s o p t)

getGamma :: (Monad m) => Int -> MaximizationStep s o p t m (M.Map s p)
getGamma t = asks $ gamma . (IM.! t) . fst

getXi :: (Monad m) => Int -> MaximizationStep s o p t m (M.Map s (M.Map s p))
getXi t = asks $ xi . (IM.! t) . fst

getObsActualAt :: (Monad m) => Int -> MaximizationStep s o p t m (Int,o)
getObsActualAt i = do
  x <- asks $ e_obs . (IM.! i) . fst
  return (i,x)

getsHMM_M :: (Monad m) => (HMM s o p t -> a) -> MaximizationStep s o p t m a
getsHMM_M f = asks $ f . snd


pi' :: (Monad m, Traversable t, Fractional p, Ord s) => MaximizationStep s o p t m (s -> p)
pi' = getGamma 0 >>= return . (M.!) . normalizeDist

-- | complete some monadic action for each state transition
foreachStateTransition :: (Monad m, Traversable t, Ord s) => (s -> s -> MaximizationStep s o p t m a) -> MaximizationStep s o p t m (M.Map s (M.Map s a))
foreachStateTransition f = do
  sts <- getsHMM_M states
  let foldDo = \f -> foldrM f M.empty sts
  foldDo $ \s_i m -> do
    y <- foldDo $ \s_j m' -> do
      x <- f s_i s_j
      return $ M.insert s_j x m'
    return $ M.insert s_i y m


foreachStateAndObs :: (Monad m, Traversable t, Ord s, Ord o) => (s -> o -> MaximizationStep s o p t m a) -> MaximizationStep s o p t m (M.Map s (M.Map o a))
foreachStateAndObs f = do
  sts <- getsHMM_M states
  obs <- getsHMM_M observations
  let foldDo = \over f -> foldrM f M.empty over
  foldDo sts $ \s_i m -> do
    y <- foldDo obs $ \s_j m' -> do
      x <- f s_i s_j
      return $ M.insert s_j x m'
    return $ M.insert s_i y m


mapToTwoArgFunc :: (Ord k1, Ord k2) => M.Map k1 (M.Map k2 v) -> (k1 -> k2 -> v)
mapToTwoArgFunc m = \k1 k2 -> (m M.! k1) M.! k2

-- CLEAN THIS UP!!!
alpha' :: (Monad m, Traversable t, Fractional p, Ord s) => MaximizationStep s o p t m (s -> s -> p)
alpha' = do
  range <- asks (init . IM.keys . fst)
  gs <- sequence $ map getGamma range
  xs <- sequence $ map getXi range
  mp <- foreachStateTransition $ \s_i s_j -> do
    let gsum = sum $ map (M.! s_i) gs
        xsum = sum $ map ((M.! s_j) . (M.! s_i)) xs
    return $ xsum / gsum
  return $ mapToTwoArgFunc $ fmap normalizeDist mp


beta' :: (Monad m, Traversable t, Fractional p, Ord s, Ord o, Eq o)
      => MaximizationStep s o p t m (s -> o -> p)
beta' = do
  range <- asks (IM.keys . fst)
  gs <- sequence $ map getGamma range
  oss <- sequence $ map getObsActualAt range
  mp <- foreachStateAndObs $ \s_j v_k -> do
    gs_v <- sequence $ map getGamma $ map fst $ filter (\(i,o)-> o == v_k) oss
    let denom = sum $ map (M.! s_j) gs
        numer = sum $ map (M.! s_j) gs_v
    return $ numer / denom
  return $ mapToTwoArgFunc $ fmap normalizeDist mp

normalizeDist :: (Fractional p, Ord k) => M.Map k p -> M.Map k p
normalizeDist m = fmap (\x -> x / s) m
  where s = sum m

maximization :: (Monad m, Traversable t, Fractional p, Ord s, Ord o, Eq o)
             => MaximizationStep s o p t m (HMM s o p t)
maximization = do
  a_hat <- alpha'
  b_hat <- beta'
  pi_hat <- pi'
  (sts, os, a_old, b_old) <- getsHMM_M (\hmm -> (states hmm, observations hmm, stateTransitions hmm , observationProbability hmm))
  return $ HMM sts os pi_hat a_hat b_hat

mapToFunc :: (Ord k) => M.Map k a -> (k -> a)
mapToFunc m = \s -> m M.! s

-- * Bringing it All Together

{-
1.) run forward and backward algorithms and save the traversals w.r.t. time
    - stuff this data in the proper time-map
2.) run the estimation step, put that stuff in the proper time-map
3.) run the maximization step to obtain A', B', pi'
4.) place the results in an updated HMM
-}

reestimate :: (Traversable t, Ord s, Num p, Fractional p, Ord o, Eq o)
           => ObservationSeq o -> HMM s o p t -> HMM s o p t
reestimate !os !hmm = runIdentity $ runReaderT maximization (est, hmm)
  where est = runIdentity $ runReaderT estData (initData hmm os)


initData :: (Traversable t, Num p, Ord s)
         => HMM s o p t -> ObservationSeq o -> (TimeMap (TimeData s o p), HMM s o p t, p)
initData hmm os = (tmap, hmm, lhood)
  where tmap = initialTimeData hmm os
        lhood = computeLikelihood tmap

initialTimeData :: (Traversable t, Num p, Ord s)
                => HMM s o p t -> ObservationSeq o -> TimeMap (TimeData s o p)
initialTimeData hmm os = mkTimeData tm_a tm_b os
  where tm_a = computeAlphas hmm os
        tm_b = computeBetas hmm os

computeAlphas :: (Traversable t, Num p, Ord s)
              => HMM s o p t -> ObservationSeq o -> TimeMap (AlphaMap s p)
computeAlphas hmm os = foldl al_t al_1 (rest os)
  where al_t tm o = let cur = IM.size tm
                        am  = tm IM.! (cur - 1)
                    in IM.insert cur (alphas_t am hmm o) tm
        al_1 = IM.singleton 0 $ alphas_1 hmm $ first os

computeBetas :: (Traversable t, Num p, Ord s)
             => HMM s o p t -> ObservationSeq o -> TimeMap (BetaMap s p)
computeBetas hmm (ObsSeq os) = case S.viewr os of
                                 S.EmptyR -> error "empty observation sequence"
                                 rest S.:> o_T -> foldr be_t be_Tm rest
                                   where index_T = S.length os - 1
                                         be_T  = betas_T hmm
                                         be_Tm = IM.singleton index_T be_T
                                         be_t o tm = let cur = index_T - IM.size tm
                                                         bm  = tm IM.! (cur + 1)
                                                     in IM.insert cur (betas_t hmm o bm) tm

computeLikelihood :: (Num p) => TimeMap (TimeData s o p) -> p
computeLikelihood tm = sum amap_T
  where t_final = IM.size tm - 1
        amap_T  = alpha $ tm IM.! t_final

-- * Time-Tagged Data

type Time = Int

type TimeMap a = IM.IntMap a

-- | Time ordered forward/backward variables with their respective observations
data TimeData s o p = TimeData { alpha :: AlphaMap s p
                               , beta :: BetaMap s p
                               , t_obs :: o
                               } deriving (Show)

-- | Time ordered estimation-step data
data EstimationData s o p = EstData { gamma :: M.Map s p
                                    , xi :: M.Map s (M.Map s p)
                                    , e_obs :: o
                                    }

-- | aggregation of TimeMaps AlphaMap and BetaMap with their observation sequence
mkTimeData :: TimeMap (AlphaMap s p) -> TimeMap (BetaMap s p) -> ObservationSeq o
           -> TimeMap (TimeData s o p)
mkTimeData tm_alpha tm_beta os = IM.intersectionWithKey (\i a b -> TimeData a b (obsAt os i)) tm_alpha tm_beta

