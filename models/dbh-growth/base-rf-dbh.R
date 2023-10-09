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

# dummies <- dummyVars(dbh_rate ~ ., data = nf_fia)
# features <- predict(dummies, newdata = nf_fia)

# test set is 20% of full dataset
test_size <- .2

# define test set based on plots (to make it truly independent)
set.seed(10)
test_plots <- sample(unique(plots), 
                     size = round(test_size * length(unique(plots))), 
                     replace = FALSE)

index <- which(plots %in% test_plots)
train <- nf_fia[-index, -1] # features[-index, ]
trainy <- dbh_rates[-index]
test <- nf_fia[index, -1] # features[index, ]
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
dbh_mod <- 
  train(x, y, method = 'ranger', metric=metric, trControl=control,
        tuneGrid = expand.grid(mtry = 1:5, 
                               splitrule = c("variance", "extratrees"),
                               min.node.size = seq(4, 10, by = 2)))

stopCluster(cl)

# final w/o CV 
library("ranger")

control2 <- trainControl(method = "none",
                         returnData = F,
                         returnResamp = "none",
                         savePredictions = F,
                         trim = T)

cl <- makePSOCKcluster(18)
registerDoParallel(cl)

set.seed(seed)
# dbh_mod_rf_caret <- train(train, trainy, method = 'ranger',
#                        metric=metric,
#                        trControl=control2,
#                        tuneGrid = dbh_mod$bestTune)

# dbh_mod_rf <- ranger(y = trainy, x = train, 
#                      mtry = dbh_mod$bestTune$mtry, 
#                      min.node.size = dbh_mod$bestTune$min.node.size,
#                      splitrule = dbh_mod$bestTune$splitrule, 
#                      num.threads = 18)

dbh_mod_rf50 <- ranger(y = trainy, x = train,
                     mtry = dbh_mod$bestTune$mtry,
                     min.node.size = dbh_mod$bestTune$min.node.size,
                     splitrule = dbh_mod$bestTune$splitrule,
                     num.trees = 50,
                     num.threads = 18)

stopCluster(cl)

# load("../base-models-rf/dbh-mod-lasso.rda")
# sizes <- pred_times <- RMSEs <- vector(mode = "numeric", length = 5L)
# sizes[5] <- lobstr::obj_size(dbh_mod_rf_caret)
# sizes[2] <- lobstr::obj_size(dbh_mod_rf50)
# sizes[3:4] <- lobstr::obj_size(dbh_mod_rf)
# sizes[1] <- lobstr::obj_size(dbh_mod_dummies) + lobstr::obj_size(dbh_mod_lasso)
# preds <- list()
# preds[[5]] <- predict(dbh_mod_rf_caret, newdata = test)
# preds[[4]] <- predict(dbh_mod_rf, data = test)$predictions
# preds[[2]] <- predict(dbh_mod_rf50, data = test)$predictions
# preds[[3]] <- predict(dbh_mod_rf, data = test, num.trees = 200)$predictions
# preds[[1]] <- predict(dbh_mod_lasso, 
#                     newdata = predict(dbh_mod_dummies, newdata = test))
# RMSEs[1] <- RMSE(preds[[1]], testy)
# RMSEs[2] <- RMSE(preds[[2]], testy)
# RMSEs[3] <- RMSE(preds[[3]], testy)
# RMSEs[4] <- RMSE(preds[[4]], testy)
# RMSEs[5] <- RMSE(preds[[5]], testy)
# pred_times[1] <- mean(rep(system.time(predict(dbh_mod_lasso, 
#   newdata = predict(dbh_mod_dummies, newdata = test)), 5)[3], 5))
# pred_times[2] <- mean(rep(system.time(predict(dbh_mod_rf50, data = test)$predictions, 5)[3], 5))
# pred_times[3] <- mean(rep(system.time(predict(dbh_mod_rf, data = test, num.trees = 200)$predictions, 5)[3], 5))
# pred_times[4] <- mean(rep(system.time(predict(dbh_mod_rf, data = test)$predictions, 5)[3], 5))
# pred_times[5] <- mean(rep(system.time(predict(dbh_mod_rf_caret, newdata = test), 5)[3], 5))
# comp <- data.frame(model = c("lasso", "ranger50", "ranger200", "ranger500", "caret500"),
#                    size = sizes, pred_time = pred_times, rmse = RMSEs)
# 
# comp %>% ggplot(aes(model, size)) + geom_point()
# comp %>% ggplot(aes(model, pred_time)) + geom_point()
# comp %>% ggplot(aes(model, rmse)) + geom_point()

dbh_mod_rf <- dbh_mod_rf50

save(dbh_mod_rf, file = "../base-models-rf/dbh-mod-rf.rda")
