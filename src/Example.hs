module Example where

import HMM
import ForwardAlgorithm
import Viterbi
import BaumWelch

type Probability = Double

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

-- [3,3,1,1,2,2,3,1,3]
trial2 :: ObservationSeq IceCreams
trial2 = fromList [Three, Three, One, One, Two, Two, Three, One, Three]

-- [3,3,1,1,2,3,3,1,2]
trial1 :: ObservationSeq IceCreams
trial1 = fromList [Three, Three, One, One, Two, Three, Three, One, Two]


triald :: ObservationSeq IceCreams
triald = fromList $ replicate 100 Three


test1 = monitoredTraining trial1 1000 100 exampleHMM
