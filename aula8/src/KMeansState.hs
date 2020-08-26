module KMeansState where


-- definition of KMeans state

data KMeansState e v
  = KMeansState {
      _centroids :: [v]     -- current centroids
    , _points    :: [e]     -- points considered
    , _err       :: Double  -- current error value
    , _limit     :: Double  -- mininum accepted erroer
    , _steps     :: Int     -- number of steps
    }

-- initialize state

initialState :: (Int -> [e] -> [v]) ->
                Int                 ->
                [e]                 ->
                Double              ->
                KMeansState e v
initialState i n pts t
  = KMeansState (i n pts) pts (1.0 / 0.0) t 0
