# backtest


TODO compare to other momentum strategies, 5/25 bands
TODO variable withdrawals

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