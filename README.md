# backtest



TODO compare to other momentum strategies, 5/25 bands
TODO variable withdrawals

Rebalance vs Prime vs PrimeNew
------------------------------


Maximum Safe Withdrawal Rates, 50 year retirements

    Success | Fixed  | 5/25   | Prime  | Prime New
    ----------------------------------------------
    100%    |  3.4%  |  3.5%  |  3.5%  |  3.6%   |
    99%     |  3.4%  |  3.6%  |  3.6%  |  3.7%   |
    98%     |  3.6%  |  3.8%  |  3.8%  |  4.0%   |
    97%     |  3.6%  |  3.8%  |  3.9%  |  4.0%   |
    96%     |  3.7%  |  3.8%  |  3.9%  |  4.0%   |
    95%     |  3.7%  |  3.8%  |  3.9%  |  4.0%   |


Raw Results (MSWR, Success%, Median End Portfolio)

    Rebalance Fixed
    ----------------
    ("RateResult",3.400%,100.000%,$3.436M)
    ("RateResult",3.500%,98.039%,$3.153M)
    ("RateResult",3.600%,98.039%,$3.051M)
    ("RateResult",3.700%,96.078%,$2.896M)
    ("RateResult",3.800%,92.157%,$2.566M)
    ("RateResult",3.900%,91.176%,$2.267M)
    ("RateResult",4.000%,89.216%,$2.081M)
    ("RateResult",4.100%,84.314%,$1.900M)
    ("RateResult",4.200%,80.392%,$1.727M)
    ("RateResult",4.300%,74.510%,$1.533M)
    ("RateResult",4.400%,68.627%,$1.341M)
    ("RateResult",4.500%,67.647%,$1.064M)

    Swedroe 5/25 bands
    ------------------
    ("RateResult",3.400%,100.000%,$7.770M)
    ("RateResult",3.500%,100.000%,$7.471M)
    ("RateResult",3.600%,99.020%,$7.172M)
    ("RateResult",3.700%,98.039%,$6.801M)
    ("RateResult",3.800%,98.039%,$6.361M)
    ("RateResult",3.900%,94.118%,$6.091M)
    ("RateResult",4.000%,93.137%,$5.807M)
    ("RateResult",4.100%,92.157%,$5.516M)
    ("RateResult",4.200%,90.196%,$5.225M)
    ("RateResult",4.300%,88.235%,$4.776M)
    ("RateResult",4.400%,84.314%,$4.548M)
    ("RateResult",4.500%,79.412%,$4.241M)

    Prime Harvesting
    ----------------
    ("RateResult",3.400%,100.000%,$2.565M)
    ("RateResult",3.500%,100.000%,$2.497M)
    ("RateResult",3.600%,99.020%,$2.429M)
    ("RateResult",3.700%,98.039%,$2.356M)
    ("RateResult",3.800%,98.039%,$2.283M)
    ("RateResult",3.900%,97.059%,$2.207M)
    ("RateResult",4.000%,94.118%,$2.069M)
    ("RateResult",4.100%,93.137%,$1.978M)
    ("RateResult",4.200%,92.157%,$1.888M)
    ("RateResult",4.300%,91.176%,$1.787M)
    ("RateResult",4.400%,89.216%,$1.680M)
    ("RateResult",4.500%,86.275%,$1.569M)

    Prime Harvesting New
    ------------------
    ("RateResult",3.400%,100.000%,$2.819M)
    ("RateResult",3.500%,100.000%,$2.686M)
    ("RateResult",3.600%,100.000%,$2.594M)
    ("RateResult",3.700%,99.020%,$2.508M)
    ("RateResult",3.800%,98.039%,$2.428M)
    ("RateResult",3.900%,98.039%,$2.323M)
    ("RateResult",4.000%,98.039%,$2.233M)
    ("RateResult",4.100%,94.118%,$2.143M)
    ("RateResult",4.200%,93.137%,$2.085M)
    ("RateResult",4.300%,91.176%,$1.993M)
    ("RateResult",4.400%,90.196%,$1.886M)
    ("RateResult",4.500%,88.235%,$1.782M)


Notes

* 6040 is optimal for PrimeNew
* At higher stock allocations prime strategies lose their edge
* At 8020 Prime New is identical to Prime
* Both still outperform Basic at all portfolios, but less with more stocks
* 120 / 80 targets for prime new are ideal