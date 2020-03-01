# tree-growth-nf
Results and analysis of US Northern Forest tree growth submodels

Some notes:
- dbh_rate is calculated by fia and is somehow cleaned. It is not simply (dbh_e - dbh_s)/interval
  - (dbh_e - dbh_s)/interval gives negative rates for about 2% of observations, but fia's dbh_rate is always positive
  - (dbh_e - dbh_s)/interval and dbh_rate are always different (they didn't just round up negative numbers), but the distribution of differences is centered around 0 (it's unbiased).
  - Using fia's rate gives a very acurate model (that's what we used), using (dbh_e - dbh_s)/interval does not
- ht_rate as calculated by fia does equal (ht_e - ht_s)/interval, and there's a lot of measurement error
  - 30% of observations have negative ht growth rates
  - The model has very low accuracy (r^2 of .1) and 15% of predictions are negative
  - Perhaps the ht rates could be processed in the same way the fia processed dbh rates to give better results
- bal is calculated based on all trees in each subplot. Does that include big and little trees (do the regen and overstory plots share a subplot ID), or are we getting weird numbers because we need to combine data from regen and overstory plots?
- Independent test data is reserved by holding back data from 20% of unique plot IDs (PLT_CN for plot at end of remeasurement period), but some of those could be plots in the training data, measured at a different time (each remeasurement gets a new plot ID). We should probably figure out which plots were remeasured multiple times and account for it explicitly. 
