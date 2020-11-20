# tree-growth-nf
Creation of tree growth models for the US Northern Forest using a machine learning approach with FIA data.

## Some notes
- dbh_rate is calculated by fia. It is essentially a prediction of growth based on the starting diameter, crown ratio, etc., etc. and is not (dbh_e - dbh_s)/interval.
  - (dbh_e - dbh_s)/interval gives negative rates for about 2% of observations, but fia's dbh_rate is always positive
- ht_rate as calculated by fia does equal (ht_e - ht_s)/interval, and there's a lot of measurement error.
  - 30% of observations have negative ht growth rates.
  - The model has very low accuracy (r^2 of .1) and 15% of predictions are negative.
- bal is calculated based on all trees in each subplot. Does that include big and little trees (do the regen and overstory plots share a subplot ID), or are we getting weird numbers because we need to combine data from regen and overstory plots?
- Independent test data is reserved by holding back data from 20% of unique plot IDs (PLT_CN for plot at end of remeasurement period), but some of those could be plots in the training data, measured at a different time (each remeasurement gets a new plot ID). We should probably figure out which plots were remeasured multiple times and account for it explicitly.

## Things to try:
- Build out models like Weiskettle's and Teck's, but use our data to get more direct comparisons between the methods.
- Look at accuracies for situations of interest (like growth rates of very high quality trees released and not released) to see if the non-parametric model does outperform the others in those situations. (Maybe the overall accuracy isn't much different, but it's better in an important minority of cases.)
