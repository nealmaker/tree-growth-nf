memory.limit(size=56000)

#######################
## Example ICE plot 
#######################

dbh_pdp_dbh <- pdp::partial(dbh_growth_model_full, pred.var = "dbh", ice = T)

# Sample from pdp object so there aren't so many lines on plot
ids <- sample(unique(dbh_pdp_dbh$yhat.id), 200, replace = F)
dbh_pdp_dbh_samp <- dbh_pdp_dbh[dbh_pdp_dbh$yhat.id %in% ids,]

# using lattice graph. autoplot() does ggplot2, but has no alpha arg
pdp::plotPartial(dbh_pdp_dbh_samp, pdp.lwd = 4, pdp.col = "#386cb0", alpha = .3)

