# backtest

- TODO is there a correlation between recent returns and crashes?
- TODO getting to 100% stocks 
- TODO stepping up methods?


Floor method
------------------------------

60y retirements, Best results from 75/25, rebalance fixed

    Rebalance Fixed
    ----------------
    |          |     low% |     2.0% |     2.5% |     3.0% |     3.5% |     4.0% |     4.5% |    high% |
    |    Total |       10 |      165 |      504 |      650 |      724 |      646 |      692 |     4987 |
    |    Worst |        4 |       19 |       14 |        7 |        2 |        4 |        8 |        1 |
    |      Num |   2.817% |  22.535% |  39.437% |  54.225% |  65.493% |  79.577% |  85.916% |  98.592% |
    [1899,1902,1906,1907]

    Fixed Steps
    ----------------
    |          |     low% |     2.0% |     2.5% |     3.0% |     3.5% |     4.0% |     4.5% |    high% |
    |    Total |       19 |      142 |      382 |      502 |      530 |     1095 |     1145 |     4563 |
    |    Worst |        9 |       28 |        5 |        2 |        1 |        5 |        9 |        0 |
    |      Num |   2.113% |  10.563% |  22.535% |  33.803% |  45.775% | 100.000% |  98.592% |  89.437% |
    [1899,1902,1906]

    Swedroe 5/25
    ----------------
    |          |     low% |     2.0% |     2.5% |     3.0% |     3.5% |     4.0% |     4.5% |    high% |
    |    Total |        8 |      162 |      492 |      689 |      683 |      612 |      685 |     5047 |
    |    Worst |        3 |       18 |       16 |        7 |        2 |        3 |       10 |        0 |
    |      Num |   2.817% |  22.535% |  40.141% |  52.817% |  65.493% |  76.056% |  86.620% |  97.887% |
    [1899,1902,1906,1907]
    Prime Harvesting ABW
    ----------------
    |          |     low% |     2.0% |     2.5% |     3.0% |     3.5% |     4.0% |     4.5% |    high% |
    |    Total |      199 |      370 |      479 |      463 |      453 |      431 |      518 |     5465 |
    |    Worst |       20 |       15 |        7 |        3 |        1 |        1 |        6 |        6 |
    |      Num |  19.718% |  30.282% |  39.437% |  47.183% |  56.338% |  71.127% |  78.873% | 100.000% |
    [1886,1887,1888,1889,1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913]

    Floor Fixed 3.3
    ----------------
    |          |     low% |     2.0% |     2.5% |     3.0% |     3.5% |     4.0% |     4.5% |    high% |
    |    Total |        0 |        0 |        0 |      699 |      793 |      795 |      770 |     5321 |
    |    Worst |        0 |        0 |        0 |       59 |        0 |        0 |        0 |        0 |
    |      Num |   0.000% |   0.000% |   0.000% |  59.155% |  80.282% |  81.690% |  70.423% |  88.732% |
    []

    Floor 525 3.3
    ----------------
    |          |     low% |     2.0% |     2.5% |     3.0% |     3.5% |     4.0% |     4.5% |    high% |
    |    Total |        2 |        0 |        0 |      707 |      760 |      758 |      776 |     5375 |
    |    Worst |        2 |        0 |        0 |        1 |       56 |        0 |        0 |        0 |
    |      Num |   0.704% |   0.000% |   0.000% |  59.155% |  78.169% |  80.986% |  73.239% |  89.437% |
    [1964]

    Floor No Rebalance
    ----------------
    |          |     low% |     2.0% |     2.5% |     3.0% |     3.5% |     4.0% |     4.5% |    high% |
    |    Total |        0 |        0 |        0 |      652 |      551 |      490 |      473 |     6212 |
    |    Worst |        0 |        0 |        0 |       59 |        0 |        0 |        0 |        0 |
    |      Num |   0.000% |   0.000% |   0.000% |  58.451% |  77.465% |  64.084% |  67.606% |  93.662% |
    []

    Floor 525 70/30 3.3
    ----------------
    |          |     low% |     2.0% |     2.5% |     3.0% |     3.5% |     4.0% |     4.5% |    high% |
    |    Total |        5 |        1 |        0 |      772 |      766 |      874 |      847 |     5113 |
    |    Worst |        3 |        0 |        0 |        0 |       56 |        0 |        0 |        0 |
    |      Num |   2.113% |   0.704% |   0.000% |  60.563% |  79.577% |  79.577% |  73.944% |  88.028% |
    [1964,1965,1966]

Rebalance vs Prime vs PrimeNew
------------------------------

100% Maximum Safe Withdrawal Rates, 50 year retirements

    Method           | MSWR | Median End Portfolio
    ---------------------------------------------
    Rebalance Fixed  | 3.3% | $2.770M
    Swedroe 5/25     | 3.4% | $5.952M
    Prime Harvesting | 3.4% | $2.380M
    Prime Buy Stocks | 3.4% | $2.578M

98% Maximum Safe Withdrawal Rates, 50 year retirements

    Method           | MSWR | Median End Portfolio
    ---------------------------------------------
    Rebalance Fixed  | 3.5% | $2.370M
    Swedroe 5/25     | 3.7% | $5.071M
    Prime Harvesting | 3.7% | $2.018M
    Prime Buy Stocks | 3.8% | $2.171M

96% Maximum Safe Withdrawal Rates, 50 year retirements

    Method           | MSWR | Median End Portfolio
    ---------------------------------------------
    Rebalance Fixed  | 3.5% | $2.370M
    Swedroe 5/25     | 3.7% | $5.071M
    Prime Harvesting | 3.8% | $1.929M
    Prime Buy Stocks | 3.9% | $2.080M

Raw Results (MSWR, Success%, Median End Portfolio)

    Years: 50

    Rebalance Fixed
    ----------------
    ("RateResult",3.300%,100.000%,$2.770M)
    ("RateResult",3.400%,97.849%,$2.541M)
    ("RateResult",3.500%,97.849%,$2.370M)
    ("RateResult",3.600%,92.473%,$2.186M)
    ("RateResult",3.700%,91.398%,$2.028M)
    ("RateResult",3.800%,90.323%,$1.812M)
    ("RateResult",3.900%,82.796%,$1.496M)
    ("RateResult",4.000%,79.570%,$1.299M)
    ("RateResult",4.100%,74.194%,$1.114M)
    ("RateResult",4.200%,66.667%,$0.871M)
    ("RateResult",4.300%,63.441%,$0.744M)
    ("RateResult",4.400%,60.215%,$0.559M)
    ("RateResult",4.500%,56.989%,$0.374M)

    Swedroe 5/25 bands
    ------------------
    ("RateResult",3.300%,100.000%,$6.289M)
    ("RateResult",3.400%,100.000%,$5.952M)
    ("RateResult",3.500%,98.925%,$5.645M)
    ("RateResult",3.600%,97.849%,$5.347M)
    ("RateResult",3.700%,97.849%,$5.071M)
    ("RateResult",3.800%,93.548%,$4.833M)
    ("RateResult",3.900%,92.473%,$4.621M)
    ("RateResult",4.000%,91.398%,$4.404M)
    ("RateResult",4.100%,88.172%,$4.176M)
    ("RateResult",4.200%,83.871%,$3.752M)
    ("RateResult",4.300%,78.495%,$3.419M)
    ("RateResult",4.400%,76.344%,$3.137M)
    ("RateResult",4.500%,73.118%,$2.850M)

    Prime Harvesting
    ----------------
    ("RateResult",3.300%,100.000%,$2.477M)
    ("RateResult",3.400%,100.000%,$2.380M)
    ("RateResult",3.500%,98.925%,$2.276M)
    ("RateResult",3.600%,97.849%,$2.183M)
    ("RateResult",3.700%,97.849%,$2.018M)
    ("RateResult",3.800%,96.774%,$1.929M)
    ("RateResult",3.900%,92.473%,$1.840M)
    ("RateResult",4.000%,92.473%,$1.762M)
    ("RateResult",4.100%,91.398%,$1.687M)
    ("RateResult",4.200%,87.097%,$1.611M)
    ("RateResult",4.300%,86.022%,$1.497M)
    ("RateResult",4.400%,81.720%,$1.405M)
    ("RateResult",4.500%,75.269%,$1.236M)

    Prime Harvesting 2
    ------------------
    ("RateResult",3.300%,100.000%,$2.664M)
    ("RateResult",3.400%,100.000%,$2.578M)
    ("RateResult",3.500%,98.925%,$2.493M)
    ("RateResult",3.600%,98.925%,$2.398M)
    ("RateResult",3.700%,97.849%,$2.262M)
    ("RateResult",3.800%,97.849%,$2.171M)
    ("RateResult",3.900%,96.774%,$2.080M)
    ("RateResult",4.000%,92.473%,$1.958M)
    ("RateResult",4.100%,91.398%,$1.870M)
    ("RateResult",4.200%,90.323%,$1.734M)
    ("RateResult",4.300%,87.097%,$1.640M)
    ("RateResult",4.400%,83.871%,$1.548M)
    ("RateResult",4.500%,75.269%,$1.436M)


Notes

* 6040 is optimal for PrimeNew
* At higher stock allocations prime strategies lose their edge
* At 8020 Prime New is identical to Prime
* Both still outperform Basic at all portfolios, but less with more stocks
* 120 / 80 targets for prime new are ideal