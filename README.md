# backtest



TODO compare to other momentum strategies, 5/25 bands
TODO variable withdrawals

Rebalance vs Prime vs PrimeNew
------------------------------

* 6040 is optimal for PrimeNew
* At higher stock allocations prime strategies lose their edge
* At 8020 Prime New is identical to Prime
* Both still outperform Basic at all portfolios, but less with more stocks
* 120 / 80 targets for prime new are ideal

    Success | Prime    | Prime New
    -----------------------------
    100%    | +0.1 SWR | +0.2 SWR
    99%     | +0.2 SWR | +0.3 SWR
    98%     | +0.2 SWR | +0.4 SWR

    Swedroe got the same results as prime

    PrimeNew
      RateResult {years = 50, rate = 3.500%, success = 100.000%}
      RateResult {years = 50, rate = 3.600%, success = 100.000%}
      RateResult {years = 50, rate = 3.700%, success = 99.020%}
      RateResult {years = 50, rate = 3.800%, success = 98.039%}
      RateResult {years = 50, rate = 3.900%, success = 98.039%}
      RateResult {years = 50, rate = 4.000%, success = 98.039%}

    Prime
      RateResult {years = 50, rate = 3.500%, success = 100.000%}
      RateResult {years = 50, rate = 3.600%, success = 99.020%}
      RateResult {years = 50, rate = 3.700%, success = 98.039%}
      RateResult {years = 50, rate = 3.800%, success = 98.039%}
      RateResult {years = 50, rate = 3.900%, success = 97.059%}
      RateResult {years = 50, rate = 4.000%, success = 94.118%}

    5/25 Bands
      RateResult {years = 50, rate = 3.500%, success = 100.000%}
      RateResult {years = 50, rate = 3.600%, success = 99.020%}
      RateResult {years = 50, rate = 3.700%, success = 98.039%}
      RateResult {years = 50, rate = 3.800%, success = 98.039%}
      RateResult {years = 50, rate = 3.900%, success = 94.118%}
      RateResult {years = 50, rate = 4.000%, success = 93.137%}

    Rebalance
      RateResult {years = 50, rate = 3.400%, success = 100.000%}
      RateResult {years = 50, rate = 3.500%, success = 98.039%}
      RateResult {years = 50, rate = 3.600%, success = 98.039%}
      RateResult {years = 50, rate = 3.700%, success = 96.078%}
      RateResult {years = 50, rate = 3.800%, success = 92.157%}
      RateResult {years = 50, rate = 3.900%, success = 91.176%}
      RateResult {years = 50, rate = 4.000%, success = 89.216%}
