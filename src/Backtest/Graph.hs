module Backtest.Graph where

import Graphics.Vega.VegaLite
import Backtest.Prelude
import Backtest.Types


toExampleChart :: VegaLite -> IO ()
toExampleChart chart = do
  toHtmlFile "graphs/example.html" chart

toChartFile :: FilePath -> VegaLite -> IO ()
toChartFile fp chart = toHtmlFile fp chart


withdrawalBinChart :: Data -> VegaLite
withdrawalBinChart dt =
    let enc = encoding
                . position X [ PName "Year", PmType Quantitative, PBin [ MaxBins 50 ] ]
                . position Y [ PName "Withdrawal", PmType Quantitative, PBin [ MaxBins 60 ], PScale [SDomain (DNumbers [0, 200])] ]
                . color [ MAggregate Count, MScale [SType ScSqrt]  ]

        bkg = background "rgba(255, 255, 255, 1.00)"

    in toVegaLite [ bkg, dt, width 300, height 200, mark Rect [MTooltip TTEncoding], enc [] ]


withdrawalLineChart :: Data -> VegaLite
withdrawalLineChart dt =
    let enc = encoding
                . position X [ PName "Year", PmType Quantitative ]
                . position Y [ PName "Withdrawal", PmType Quantitative]
                . color [ MName "Start", MmType Nominal ]

        mrk = mark Line []

        bkg = background "rgba(255, 255, 255, 1.00)"

    in toVegaLite [ bkg, dt, width 300, height 200, mark Rect [MTooltip TTEncoding], mrk, enc []]




simData :: [SimResult] -> Data
simData srs = dataFromRows [] $ mconcat $ map simDataRows $ srs

simDataRows :: SimResult -> [DataRow]
simDataRows sr = foldr (yearDataRow sr.startYear) [] sr.years

yearDataRow :: Year -> YearStart -> [DataRow] -> [DataRow]
yearDataRow sy ys =
  dataRow
    [ ("Year", Number (fromIntegral ys.yearIndex))
    , ("Start", Str (cs $ show $ fromYear sy))
    , ("Withdrawal", Number (fromIntegral $ dollars ys.withdrawal))
    ]




-- carsChart :: VegaLite
-- carsChart =
--     let cars =  dataFromUrl "https://vega.github.io/vega-datasets/data/cars.json" []

--         enc = encoding
--                 . position X [ PName "Horsepower", PmType Quantitative ]
--                 . position Y [ PName "Miles_per_Gallon", PmType Quantitative, PTitle "Miles per Gallon" ]
--                 . color [ MName "Origin" ]

--         bkg = background "rgba(0, 0, 0, 0.05)"

--     in toVegaLite [ bkg, cars, mark Circle [MTooltip TTEncoding], enc [] ]



-- moviesChart :: VegaLite
-- moviesChart =
--     let movies =  dataFromUrl "https://vega.github.io/vega-datasets/data/movies.json" []

--         enc = encoding
--                 . position X [ PName "IMDB Rating", PmType Quantitative, PBin [ MaxBins 60 ] ]
--                 . position Y [ PName "Rotten Tomatoes Rating", PmType Quantitative, PBin [ MaxBins 40 ]]
--                 . color [ MName "Origin", MAggregate Count ]

--         bkg = background "rgba(255, 255, 255, 1.00)"

--     in toVegaLite [ bkg, movies, width 300, height 200, mark Rect [MTooltip TTEncoding], enc [] ]
