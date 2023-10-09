library("tidyverse")
library("caret")

temp <- tempfile()
download.file(
  "https://github.com/nealmaker/tree-growth-nf/raw/master/data/nf-fia.rda", 
  temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change == "lived") %>% 
  select(plot, dbh_rate, spp, dbh_mid, cr_mid,
         ba_mid, bal_mid, site_class, lat, lon, elev) %>% 
  rename(dbh = dbh_mid, cr = cr_mid, ba = ba_mid, bal = bal_mid)

dbh_rates <- nf_fia$dbh_rate
plots <- nf_fia$plot

# one-hot encoding for categorical factors (but not plot)
nf_fia <- select(nf_fia, -plot)

dummies <- dummyVars( ~ ., data = nf_fia[, -1])
features <- predict(dummies, newdata = nf_fia[, -1])

# test set is 20% of full dataset
test_size <- .2

# define test set based on plots (to make it truely independent)
set.seed(10)
test_plots <- sample(unique(plots), 
                     size = round(test_size * length(unique(plots))), 
                     replace = FALSE)

index <- which(plots %in% test_plots)
train <- features[-index, ]
trainy <- dbh_rates[-index]
test <- features[index, ]
testy <- dbh_rates[index]

# sample from training data to expediate algorithm testing
subsamp_size <- 50000
set.seed(201)
subsamp <- sample(1:nrow(train),
                  size = subsamp_size,
                  replace = FALSE)

x <- train[subsamp, ]
y <- trainy[subsamp]

# cross validation
control <- trainControl(method="cv", number = 10,
                        returnData = F, trim = T)
seed <- 7
metric <- "RMSE"

# train in parallel to speed
library(doParallel)
cl <- makePSOCKcluster(10)
registerDoParallel(cl)

set.seed(seed)
dbh_mod_base <- train(x, y, method = 'rqlasso', 
                      metric=metric, 
                      preProc=c("center", "scale"), 
                      trControl=control)

stopCluster(cl)

cl <- makePSOCKcluster(10)
registerDoParallel(cl)

set.seed(seed)
dbh_mod_2 <- train(x, y, method = 'rqlasso', 
                      metric=metric, 
                      preProc=c("center", "scale"), 
                      trControl=control,
                      tuneGrid = data.frame(lambda = seq(0, .01, by = 0.001)))

stopCluster(cl)


control2 <- trainControl(method = "none",
                         returnData = F, 
                         returnResamp = "none",
                         savePredictions = F,
                         trim = T)
  
# train in parallel to speed
library(doParallel)
cl <- makePSOCKcluster(10)
registerDoParallel(cl)

set.seed(seed)
dbh_mod_lasso <- train(features, dbh_rates, method = 'rqlasso', 
                      metric=metric, 
                      preProc=c("center", "scale"), 
                      trControl=control2,
                      tuneGrid = dbh_mod_2$bestTune)

stopCluster(cl)

dbh_mod_dummies <- dummies
save(dbh_mod_lasso, dbh_mod_dummies, 
     file = "../base-models-rf/dbh-mod-lasso.rda")
