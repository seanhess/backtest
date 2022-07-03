module Backtest.Graph where

import Graphics.Vega.VegaLite
import Backtest.Prelude
import Backtest.Types
import qualified Data.List.NonEmpty as NE


toExampleChart :: VegaLite -> IO ()
toExampleChart chart = do
  toHtmlFile "graphs/example.html" chart

toChartFile :: FilePath -> [VLSpec] -> IO ()
toChartFile fp chart = toHtmlFile fp $ toVegaLite [vConcat chart]


withdrawalStackChart :: NonEmpty MedianWithdrawal -> VLSpec
withdrawalStackChart mws = 
    let enc = encoding
                . position X [ PName "Year",   PmType Quantitative, PTitle "Year" ]
                . position Y [ PName "Amount", PmType Quantitative, PTitle "Median Amount", PAggregate Sum ]
                . color [ MName "Type", MmType Nominal ]

        bkg = background "rgba(255, 255, 255, 1.00)"

    in asSpec [ bkg, toData mws, width 500, mark Bar [MTooltip TTEncoding], enc [] ]

  where
    toData ms = dataFromRows [] $ (foldr toRows [] ms)

    toRows :: MedianWithdrawal -> [DataRow] -> [DataRow]
    toRows m =
        dataRow
          [ ("Year", Number (fromIntegral m.yearIndex))
          , ("Amount", Number (fromIntegral $ dollars m.withdrawal))
          , ("Type", Str "withdrawal")
          ]
        . dataRow
          [ ("Year", Number (fromIntegral $ m.yearIndex))
          , ("Amount", Number (fromIntegral $ dollars m.netExpenses))
          , ("Type", Str "expense")
          ]



withdrawalBinChart :: NumYears -> Data -> VLSpec
withdrawalBinChart (NumYears yrs) dt =
    let enc = encoding
                . position X [ PName "Year",       PmType Quantitative, PBin [ MaxBins yrs ] ]
                . position Y [ PName "Withdrawal", PmType Quantitative, PBin [ MaxBins 60 ] ] -- , PScale [SDomain (DNumbers [0, 200])] ]
                . color [ MAggregate Count, MScale [SType ScSqrt]  ]

        bkg = background "rgba(255, 255, 255, 1.00)"

    in asSpec [ bkg, dt, width 500, mark Rect [MTooltip TTEncoding], enc [] ]


withdrawalLineChart :: Data -> VLSpec
withdrawalLineChart dt =
    let enc = encoding
                . position X [ PName "Year", PmType Quantitative ]
                . position Y [ PName "Withdrawal", PmType Quantitative]
                . color [ MName "Start", MmType Nominal, MScale [SType ScLinear] ]

        bkg = background "rgba(255, 255, 255, 1.00)"

    in asSpec [ bkg, dt, width 500, mark Line [MTooltip TTEncoding], enc []]


simData :: USD (Amt Withdrawal) -> NonEmpty SimResult -> Data
simData maxW srs = dataFromRows [] $ mconcat $ map simDataRows $ NE.toList srs
  where
    simDataRows :: SimResult -> [DataRow]
    simDataRows sr = foldr (yearDataRow sr.startYear) [] sr.years

    yearDataRow :: Year -> YearStart -> [DataRow] -> [DataRow]
    yearDataRow sy ys =
      dataRow
        [ ("Year", Number (fromIntegral ys.yearIndex))
        , ("Start", Str (cs $ show $ fromYear sy))
        , ("Withdrawal", Number (fromIntegral $ dollars (min maxW ys.withdrawal)))
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
