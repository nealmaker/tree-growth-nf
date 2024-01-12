library("dplyr")
library("caret")

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

# Upsampling minority class (died) w/ SMOTE to make a more balanced dataset
train_all <- cbind(data.frame(y = trainy), train)
library("ROSE")
# I was using p = .5 (died and lived equal), but model was doubling total amount
# of mortality, now trying with p = .2
synthetic <- ROSE(y ~ ., train_all, p = .2, seed = 3214) 
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

# cross validation
control <- trainControl(method="cv", number = 10,
                        returnData = F, trim = T)
seed <- 7
metric <- "Kappa"

# train in parallel to speed
library(doParallel)
cl <- makePSOCKcluster(10)
registerDoParallel(cl)

memory.limit(size=56000)
set.seed(seed)
surv_mod_base <- train(x, y,
                       method = "Rborist", 
                       trControl = control,
                       metric = metric,
                       ntree = 50)

stopCluster(cl)

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

library(Rborist)
cl <- makePSOCKcluster(18)
registerDoParallel(cl)

set.seed(seed)
surv_mod50 <- Rborist(x = train, y = trainy,
                       predFixed = surv_mod2$bestTune$predFixed, # 8
                       minNode = surv_mod2$bestTune$minNode, # 10
                       nTree = 50)

stopCluster(cl)

cl <- makePSOCKcluster(18)
registerDoParallel(cl)

set.seed(seed)
surv_mod200 <- Rborist(x = nf_fia[,-1], y = surv,
                      predFixed = surv_mod2$bestTune$predFixed,
                      minNode = surv_mod2$bestTune$minNode,
                      nTree = 200)

stopCluster(cl)

cl <- makePSOCKcluster(18)
registerDoParallel(cl)

set.seed(seed)
surv_mod500 <- Rborist(x = nf_fia[,-1], y = surv,
                      predFixed = surv_mod2$bestTune$predFixed,
                      minNode = surv_mod2$bestTune$minNode,
                      nTree = 500)

stopCluster(cl)

load("../base-models-rf/surv-mod-l2.rda")
surv_oldrf <- readRDS("../base-models-rf/surv-mod-base.rds")
sizes <- pred_times <- kappas <- vector(mode = "numeric", length = 5L)
sizes[2] <- lobstr::obj_size(surv_mod50)
sizes[3] <- lobstr::obj_size(surv_mod200)
sizes[4] <- lobstr::obj_size(surv_mod500)
sizes[1] <- lobstr::obj_size(surv_mod_dummies) + lobstr::obj_size(surv_mod_l2)
sizes[5] <- lobstr::obj_size(surv_oldrf)
preds <- list()
preds[[4]] <- predict(surv_mod500, newdata = test)$yPred
preds[[2]] <- predict(surv_mod50, newdata = test)$yPred
preds[[3]] <- predict(surv_mod200, newdata = test)$yPred
preds[[1]] <- predict(surv_mod_l2,
                      newdata = predict(surv_mod_dummies, newdata = test))
preds[[5]] <- predict(surv_oldrf, newdata = test)
kappas[1] <- postResample(preds[[1]], testy)[2]
kappas[2] <- postResample(preds[[2]], testy)[2]
kappas[3] <- postResample(preds[[3]], testy)[2]
kappas[4] <- postResample(preds[[4]], testy)[2]
kappas[5] <- postResample(preds[[5]], testy)[2]
pred_times[1] <- mean(rep(system.time(predict(surv_mod_l2,
                                              newdata = predict(surv_mod_dummies, newdata = test)), 5)[3], 5))
pred_times[2] <- mean(rep(system.time(predict(surv_mod50, newdata = test)$yPred, 5)[3], 5))
pred_times[3] <- mean(rep(system.time(predict(surv_mod200, newdata = test)$yPred, 5)[3], 5))
pred_times[4] <- mean(rep(system.time(predict(surv_mod500, newdata = test)$yPred, 5)[3], 5))
pred_times[5] <- mean(rep(system.time(predict(surv_oldrf, newdata = test), 5)[3], 5))
comp <- data.frame(model = c("l2", "Rborist50", "Rborist200", "Rborist500", "oldRF"),
                   size = sizes, pred_time = pred_times, kappa = kappas)

comp %>% ggplot(aes(model, size)) + geom_point()
comp %>% ggplot(aes(model, pred_time)) + geom_point()
comp %>% ggplot(aes(model, kappa)) + geom_point()

surv_mod_rf <- surv_mod50
save(surv_mod_rf, file = "../base-models-rf/surv-mod-rf.rda")
