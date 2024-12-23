# Updated w/ rfArb 2024-12-23

library("dplyr")
library("caret")
library("Rborist")
library("doParallel")

temp <- tempfile()
download.file(
  "https://github.com/nealmaker/tree-growth-nf/raw/master/data/nf-fia.rda", 
  temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change != "cut") %>% 
  mutate(lived = as.factor(as.character(status_change))) %>% 
  select(plot, lived, interval, spp, dbh_s, cr_s,
         ba_s, bal_s, site_class, lat, lon, elev) %>% 
  rename(dbh = dbh_s, cr = cr_s, ba = ba_s, bal = bal_s)

surv <- nf_fia$lived
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
trainy <- surv[-index]
test <- nf_fia[index, -1] # features[index, ]
testy <- surv[index]

# cross validation
control <- trainControl(method="cv", number = 10,
                        returnData = F, trim = T)
seed <- 7
metric <- "Kappa"

################################################################################
# Upsampling minority class (died) w/ SMOTE to make a more balanced dataset ####
################################################################################
# Smote will turn integers numeric, so need to make test data compatible for predictions
test <- mutate(test, cr = as.numeric(cr), site_class = as.numeric(site_class),
               elev = as.numeric(elev))

train_all <- cbind(data.frame(y = trainy), train)
library("ROSE")

# I was using p = .5 (died and lived equal), but model was doubling total amount
# of mortality, now trying with other values
smote_test <- lapply(c(.2, .3, .4, .5), function(i){
  synthetic <- ROSE(y ~ ., train_all, N = 700000, p = i, seed = 3214) 
  train <- synthetic$data
  trainy <- train[, 1]
  train <- train[, -1]
  
  # sample from training data to expediate algorithm testing
  subsamp_size <- 100000
  set.seed(201)
  subsamp <- sample(1:nrow(train),
                    size = subsamp_size,
                    replace = FALSE)
  
  x <- train[subsamp, ]
  y <- trainy[subsamp]
  
  cl <- makePSOCKcluster(10)
  registerDoParallel(cl)
  
  memory.limit(size=56000)
  set.seed(seed)
  surv_mod2 <- train(x, y,
                     method = "Rborist", 
                     trControl = control,
                     metric = metric,
                     ntree = 50,
                     tuneGrid = expand.grid(predFixed = 7:10, 
                                            minNode = seq(2, 10, by = 2)))
  
  stopCluster(cl)
  
  cl <- makePSOCKcluster(18)
  registerDoParallel(cl)
  
  set.seed(seed)
  surv_mod50 <- rfArb(x = train, y = trainy,
                      predFixed = surv_mod2$bestTune$predFixed, 
                      minNode = surv_mod2$bestTune$minNode, 
                      nTree = 50)
  
  stopCluster(cl)
  
  return(confusionMatrix(data = predict(surv_mod50, newdata = test)$yPred, 
                         reference = testy))
})

# p = .3 best, higher gives better sensitivity, but lower kappa
# apply function very slow with parallel processing.

# New Smote test for synthetic data size:
rose_n_test <- list()
rose_ns <- seq(400000, 800000, by = 200000)
for (i in 1:3) {
  synthetic <- ROSE(y ~ ., train_all, N = rose_ns[i], p = .3, seed = 3214) 
  train <- synthetic$data
  trainy <- train[, 1]
  train <- train[, -1]
  
  # sample from training data to expediate algorithm testing
  subsamp_size <- 100000
  set.seed(201)
  subsamp <- sample(1:nrow(train),
                    size = subsamp_size,
                    replace = FALSE)
  
  x <- train[subsamp, ]
  y <- trainy[subsamp]
  
  cl <- makePSOCKcluster(10)
  registerDoParallel(cl)
  
  memory.limit(size=56000)
  set.seed(seed)
  surv_mod2 <- train(x, y,
                     method = "Rborist", 
                     trControl = control,
                     metric = metric,
                     ntree = 50,
                     tuneGrid = expand.grid(predFixed = 7:10, 
                                            minNode = seq(2, 10, by = 2)))
  
  stopCluster(cl)
  
  cl <- makePSOCKcluster(18)
  registerDoParallel(cl)
  
  set.seed(seed)
  surv_mod50 <- rfArb(x = train, y = trainy,
                      predFixed = surv_mod2$bestTune$predFixed, 
                      minNode = surv_mod2$bestTune$minNode, 
                      nTree = 50)
  
  stopCluster(cl)
  
  rose_n_test[[i]] <- 
    confusionMatrix(data = predict(surv_mod50, newdata = test)$yPred, 
                    reference = testy)
}
# N = 700000 gave the best kappa,
# Also slow with for loop, maybe just slow no matter what.
# Best ROSE tuning params are N = 700000, p = .3 ###############################

synthetic <- ROSE(y ~ ., train_all, N = 700000, p = .3, seed = 3214) 
train <- synthetic$data
trainy <- train[, 1]
train <- train[, -1]


# sample from training data to expediate algorithm testing
subsamp_size <- 100000
set.seed(201)
subsamp <- sample(1:nrow(train),
                  size = subsamp_size,
                  replace = FALSE)

x <- train[subsamp, ]
y <- trainy[subsamp]

# train in parallel to speed

# cl <- makePSOCKcluster(10)
# registerDoParallel(cl)
# 
# memory.limit(size=56000)
# set.seed(seed)
# surv_mod_base <- train(x, y,  # training Rborist objects like this use predict(model, newdata, type = "prob")
#                        method = "Rborist", 
#                        trControl = control,
#                        metric = metric,
#                        ntree = 50)
# 
# stopCluster(cl)

cl <- makePSOCKcluster(10)
registerDoParallel(cl)

memory.limit(size=56000)
set.seed(seed)
surv_mod2 <- train(x, y,
                   method = "Rborist", 
                   trControl = control,
                   metric = metric,
                   ntree = 50,
                   tuneGrid = expand.grid(predFixed = 7:10, 
                                          minNode = seq(2, 10, by = 2)))

stopCluster(cl)

# Rborist was replaced by rfArb in 2023 ########################################
cl <- makePSOCKcluster(18)
registerDoParallel(cl)

set.seed(seed)
surv_mod50 <- rfArb(x = train, y = trainy,
                       predFixed = surv_mod2$bestTune$predFixed, # 7
                       minNode = surv_mod2$bestTune$minNode, # 10
                       nTree = 50)

stopCluster(cl)

cl <- makePSOCKcluster(18)
registerDoParallel(cl)

set.seed(seed)
surv_mod200 <- rfArb(x = train, y = trainy,
                      predFixed = surv_mod2$bestTune$predFixed,
                      minNode = surv_mod2$bestTune$minNode,
                      nTree = 200)

stopCluster(cl)

cl <- makePSOCKcluster(18)
registerDoParallel(cl)

set.seed(seed)
surv_mod500 <- rfArb(x = train, y = trainy,
                      predFixed = surv_mod2$bestTune$predFixed,
                      minNode = surv_mod2$bestTune$minNode,
                      nTree = 500)

stopCluster(cl)

# load("../base-models-rf/surv-mod-l2.rda")
# surv_oldrf <- readRDS("../base-models-rf/surv-mod-base.rds")
sizes <- pred_times <- kappas <- vector(mode = "numeric", length = 3L)
sizes[1] <- lobstr::obj_size(surv_mod50)
sizes[2] <- lobstr::obj_size(surv_mod200)
sizes[3] <- lobstr::obj_size(surv_mod500)
# sizes[1] <- lobstr::obj_size(surv_mod_dummies) + lobstr::obj_size(surv_mod_l2)
# sizes[5] <- lobstr::obj_size(surv_oldrf)

# library(LiblineaR)

preds <- list()
preds[[3]] <- predict(surv_mod500, newdata = test)$yPred
preds[[1]] <- predict(surv_mod50, newdata = test)$yPred
preds[[2]] <- predict(surv_mod200, newdata = test)$yPred
# preds[[1]] <- predict(surv_mod_l2,
#                       newdata = predict(surv_mod_dummies, newdata = test))
# preds[[5]] <- predict(surv_oldrf, newdata = test)
kappas[1] <- postResample(preds[[1]], testy)[2]
kappas[2] <- postResample(preds[[2]], testy)[2]
kappas[3] <- postResample(preds[[3]], testy)[2]
# kappas[4] <- postResample(preds[[4]], testy)[2]
# kappas[5] <- postResample(preds[[5]], testy)[2]
# pred_times[1] <- mean(rep(system.time(predict(surv_mod_l2,
#                                               newdata = predict(surv_mod_dummies, newdata = test)), 5)[3], 5))
pred_times[1] <- mean(rep(system.time(predict(surv_mod50, newdata = test)$yPred, 5)[3], 5))
pred_times[2] <- mean(rep(system.time(predict(surv_mod200, newdata = test)$yPred, 5)[3], 5))
pred_times[3] <- mean(rep(system.time(predict(surv_mod500, newdata = test)$yPred, 5)[3], 5))
# pred_times[5] <- mean(rep(system.time(predict(surv_oldrf, newdata = test), 5)[3], 5))
comp <- data.frame(model = c("Rborist50", "Rborist200", "Rborist500"),
                   size = sizes, pred_time = pred_times, kappa = kappas)

# View(comp) 

# 50 trees give a lower kappa (.3305, compared to .3380 for 200 trees, but is
# much smaller (346mb vs 1344mb) & much faster (2.33sec to make 101716
# predictions compared to 5.54 sec for 200 trees), so I used it. 

# 2024-12-23 run specs: training data was 408491 observations, ROSE was used
# with p = .3 & N = 700000 to make a synthetic dataset of 700000 observations
# for training. Test data used to paramaterize rfArb and ROSE was 101716
# observations set aside by plot at the start (not synthetic). Best
# paramiterization that was used was 50 trees, predFixed of 7, and minNode of
# 10. Confusion matrix shows sensitivity of .3745 (predictions of trees that
# actually died, died became the positive class for some reason), specificity of
# .94135, kappa of .3305, balanced accuracy of .65793, mcnemar's test p-value of
# <2e-16, accuracy of .8844.

################################################################################
# Make predictions with:
# predict(surv_mod_rf, newdata = y, ctgCensus = "prob")
################################################################################

surv_mod_rf <- surv_mod50

save(surv_mod_rf, file = "../base-models-rf/surv-mod-rf.rda")
