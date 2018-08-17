module Pretty where

import HMM
import qualified Data.Map as M
import qualified Text.PrettyPrint.Boxes as Box
import Text.Printf


showHMM :: (Foldable t, Ord s, Ord o, Show p, Show s, Show o) => HMM s o p t -> String
showHMM hmm = Box.render $ Box.hsep 3 Box.left [pi, a , b]
  where pi = (Box.text "PI") Box.// showPIs hmm
        a = (Box.text "ALPHA") Box.// showAlphas hmm
        b = (Box.text "BETA") Box.// showBetas hmm

showPIs :: (Foldable t, Ord s, Show p, Show s) => HMM s o p t -> Box.Box
showPIs hmm@(HMM _ _ pi _ _) = prettyMap $ overStateSpace hmm (\s -> pi s)

showAlphas :: (Foldable t, Ord s, Show p, Show s) => HMM s o p t -> Box.Box
showAlphas hmm@(HMM _ _ _ a _) = prettyMap' showTransition $ collapseDoubleMap $ overStateSpace hmm (\s_i -> overStateSpace hmm (\s_j -> a s_i s_j))

showBetas :: (Foldable t, Ord s, Ord o, Show p, Show s, Show o) => HMM s o p t -> Box.Box
showBetas hmm@(HMM _ os _ _ b) = prettyMap' showEmission $ collapseDoubleMap $ overStateSpace hmm (\s_i -> overObsSpace hmm (\s_j -> b s_i s_j))

collapseDoubleMap :: (Ord k1, Ord k2) => M.Map k1 (M.Map k2 v) -> M.Map (k1,k2) v
collapseDoubleMap m = M.foldrWithKey (\k1 y z -> z <> M.mapKeys (\k2 -> (k1, k2)) y) M.empty m

showEmission :: (Show k, Show e) => (k,e) -> String
showEmission (k1,k2) = concat ["P[ ",s1," ➢ ",s2," ]" ]
  where s1 = show k1
        s2 = show k2


showTransition :: (Show k1, Show k2) => (k1,k2) -> String
showTransition (k1,k2) = concat ["P[ ",s1," → ",s2," ]" ]
  where s1 = show k1
        s2 = show k2

prettyMap' :: (Show v) => (k -> String) -> M.Map k v -> Box.Box
prettyMap' sh m = key_boxes Box.<+> val_boxes
  where val_boxes = Box.vcat Box.left $ M.map (\v -> Box.text (show v)) m
        key_boxes = Box.vcat Box.left $ fmap (\k -> Box.text (sh k)) $ M.keys m

prettyMap :: (Show v, Show k) => M.Map k v -> Box.Box
prettyMap m = key_boxes Box.<+> val_boxes
  where val_boxes = Box.vcat Box.left $ M.map (\v -> Box.text (show v)) m
        key_boxes = Box.vcat Box.left $ fmap (\k -> Box.text (show k)) $ M.keys m

prettyPercent :: (PrintfArg a) => a -> String
prettyPercent x = printf "%.5f" x
