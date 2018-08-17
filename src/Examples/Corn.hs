module Examples.Corn where

import           BaumWelch
import           Data.List
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Format
import           HMM
import           System.IO
import           Text.Parsec
import           Text.Parsec.Token
import Viterbi
import ForwardAlgorithm

type Week = Day

type Price = Double

type CornData = [(Week, Price)]


dataFormat :: String
dataFormat = "%F"

parseWeek :: String -> Maybe (Week, Price)
parseWeek str = do
  let (date,price) = break (== ',') str
  date' <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d" date
  return $ (date', read $ tail price)

readCornFile :: IO (CornData)
readCornFile = readFile "./src/Examples/Corn.dat" >>= \contents -> return $ catMaybes $ fmap parseWeek $ lines contents

test = do
  corn <- readCornFile
  let ms = fmap monthView corn
      ts = fmap (\(w,p,t) -> t) $ weeklyTrend corn
      hmm = HMM ["Spring", "Summer", "Fall", "Winter"] [Up, Down, Hold] initPi initA initB
  hmm' <- monitoredTraining (fromList ts) 10 1 hmm
  putStrLn $ show $ decode hmm' (fromList ts)
  putStr $ show ts
  putStrLn $ show $ likelihood hmm' (fromList ts)
  return ()

initPi :: String -> Double
initPi "Summer" = 0.00
initPi "Winter" = 1.0
initPi "Spring" = 0.00
initPi "Fall" = 0.00

initA :: String -> String -> Double
initA "Winter" "Winter" = 0.7
initA "Winter" "Spring" = 0.24
initA "Spring" "Spring" = 0.7
initA "Spring" "Summer" = 0.24
initA "Summer" "Summer" = 0.75
initA "Summer" "Fall"   = 0.24
initA "Fall"   "Fall"   = 0.75
initA "Fall" "Winter"   = 0.24
initA _ _               = 0.01

initB :: String -> Trend -> Double
initB "Summer" Up   = 0.6
initB "Summer" Down = 0.2
initB "Summer" Hold = 0.2
initB "Winter" Up   = 0.5
initB "Winter" Down = 0.3
initB "Winter" Hold = 0.2
initB "Spring" Up   = 0.5
initB "Spring" Down = 0.3
initB "Spring" Hold = 0.2
initB "Fall" Up     = 0.5
initB "Fall" Down   = 0.3
initB "Fall" Hold   = 0.2

data MonthView = MonthView Month Price

instance Show MonthView where
  show (MonthView m p) = (show m) ++ " -> " ++ (show p)

monthView :: (Week, Price) -> MonthView
monthView (w,p) = MonthView (month w) p

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Eq, Ord, Show, Enum)

theMonths :: [Month]
theMonths = [Jan .. Dec]

month :: Week -> Month
month w = let (_,m,_) = toGregorian w in fromNum m

fromNum :: Int -> Month
fromNum 1  = Jan
fromNum 2  = Feb
fromNum 3  = Mar
fromNum 4  = Apr
fromNum 5  = May
fromNum 6  = Jun
fromNum 7  = Jul
fromNum 8  = Aug
fromNum 9  = Sep
fromNum 10 = Oct
fromNum 11 = Nov
fromNum 12 = Dec
fromNum x  = error $ (show x) ++ "Does not correspond to a month"

data Trend = Up | Down | Hold deriving (Show, Eq, Ord)

weeklyTrend :: [(Week,Price)] -> [(Week,Price,Trend)]
weeklyTrend ((w,p):xs) = foldr (\(w,p) xs'@((w1,p1,t1):_) -> let t
                                                                  | p > p1 = Up
                                                                  | p < p1 = Down
                                                                  | p == p1 = Hold
                                                            in (w,p,t) : xs') [(w,p,Hold)] xs
