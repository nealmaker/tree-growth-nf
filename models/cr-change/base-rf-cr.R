library("tidyverse")
library("caret")
library("ranger")

MAE <- function(true_ratings, predicted_ratings){
  sum(abs(predicted_ratings - true_ratings)) / length(true_ratings)
}

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

temp <- tempfile()
download.file(
  "https://github.com/nealmaker/tree-growth-nf/raw/master/data/nf-fia.rda", 
  temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change == "lived") %>% 
  select(plot, cr_rate, spp, dbh_mid, cr_mid,
         ba_mid, bal_mid, site_class, lat, lon, elev) %>% 
  rename(dbh = dbh_mid, cr = cr_mid, ba = ba_mid, bal = bal_mid)

cr_rates <- nf_fia$cr_rate
plots <- nf_fia$plot

# one-hot encoding for categorical factors (but not plot)
nf_fia <- select(nf_fia, -plot)

# dummies <- dummyVars(ht_rate ~ ., data = nf_fia)
# features <- predict(dummies, newdata = nf_fia)

# test set is 20% of full dataset
test_size <- .2

# define test set based on plots (to make it truely independent)
set.seed(10)
test_plots <- sample(unique(plots), 
                     size = round(test_size * length(unique(plots))), 
                     replace = FALSE)

index <- which(plots %in% test_plots)
train <- nf_fia[-index, -1] # features[-index, ]
trainy <- cr_rates[-index]
test <- nf_fia[index, -1] # features[index, ]
testy <- cr_rates[index]

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
cr_mod_base <- train(x, y, method = 'ranger', metric=metric, 
                trControl=control)

stopCluster(cl)

cl <- makePSOCKcluster(10)
registerDoParallel(cl)

set.seed(seed)
cr_mod_base2 <- train(x, y, method = 'ranger', metric=metric, 
                     trControl=control,
                     num.trees = 50,
                     tuneGrid = expand.grid(mtry = 1:5, 
                                            splitrule = c("variance", "extratrees"),
                                            min.node.size = seq(4, 10, by = 2)))

stopCluster(cl)

cl <- makePSOCKcluster(18)
registerDoParallel(cl)

set.seed(seed)
cr_mod50 <- ranger(x = nf_fia[, -1], y = cr_rates,
                      mtry = cr_mod_base2$bestTune$mtry,
                      min.node.size = cr_mod_base2$bestTune$min.node.size,
                      splitrule = cr_mod_base2$bestTune$splitrule,
                      num.trees = 50,
                      num.threads = 18)

stopCluster(cl)

cl <- makePSOCKcluster(18)
registerDoParallel(cl)

set.seed(seed)
cr_mod200 <- ranger(x = nf_fia[, -1], y = cr_rates,
                   mtry = cr_mod_base2$bestTune$mtry,
                   min.node.size = cr_mod_base2$bestTune$min.node.size,
                   splitrule = cr_mod_base2$bestTune$splitrule,
                   num.trees = 200,
                   num.threads = 18)

stopCluster(cl)

cl <- makePSOCKcluster(18)
registerDoParallel(cl)

set.seed(seed)
cr_mod500 <- ranger(x = nf_fia[, -1], y = cr_rates,
                   mtry = cr_mod_base2$bestTune$mtry,
                   min.node.size = cr_mod_base2$bestTune$min.node.size,
                   splitrule = cr_mod_base2$bestTune$splitrule,
                   num.trees = 500,
                   num.threads = 18)

stopCluster(cl)

load("../base-models-rf/cr-mod-lasso.rda")
sizes <- pred_times <- RMSEs <- vector(mode = "numeric", length = 4L)
sizes[2] <- lobstr::obj_size(cr_mod50)
sizes[3] <- lobstr::obj_size(cr_mod200)
sizes[4] <- lobstr::obj_size(cr_mod500)
sizes[1] <- lobstr::obj_size(cr_mod_dummies) + lobstr::obj_size(cr_mod_lasso)
preds <- list()
preds[[4]] <- predict(cr_mod500, data = test)$predictions
preds[[2]] <- predict(cr_mod50, data = test)$predictions
preds[[3]] <- predict(cr_mod200, data = test)$predictions
preds[[1]] <- predict(cr_mod_lasso,
                    newdata = predict(cr_mod_dummies, newdata = test))
RMSEs[1] <- RMSE(preds[[1]], testy)
RMSEs[2] <- RMSE(preds[[2]], testy)
RMSEs[3] <- RMSE(preds[[3]], testy)
RMSEs[4] <- RMSE(preds[[4]], testy)
pred_times[1] <- mean(rep(system.time(predict(cr_mod_lasso,
  newdata = predict(cr_mod_dummies, newdata = test)), 5)[3], 5))
pred_times[2] <- mean(rep(system.time(predict(cr_mod50, data = test)$predictions, 5)[3], 5))
pred_times[3] <- mean(rep(system.time(predict(cr_mod200, data = test)$predictions, 5)[3], 5))
pred_times[4] <- mean(rep(system.time(predict(cr_mod500, data = test)$predictions, 5)[3], 5))
comp <- data.frame(model = c("lasso", "ranger50", "ranger200", "ranger500"),
                   size = sizes, pred_time = pred_times, rmse = RMSEs)

comp %>% ggplot(aes(model, size)) + geom_point()
comp %>% ggplot(aes(model, pred_time)) + geom_point()
comp %>% ggplot(aes(model, rmse)) + geom_point()

cr_mod_rf <- cr_mod50
save(cr_mod_rf, file = "../base-models-rf/cr-mod-rf.rda")
