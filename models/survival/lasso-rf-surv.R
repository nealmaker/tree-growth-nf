library("tidyverse")
library("caret")

temp <- tempfile()
download.file(
  "https://github.com/nealmaker/tree-growth-nf/raw/master/data/nf-fia.rda", 
  temp)
load(temp)

# remove trees that were cut and unwanted variables
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

dummies <- dummyVars(~ ., data = nf_fia[, -1])
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
trainy <- surv[-index]
test <- features[index, ]
testy <- surv[index]

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

# train in parallel to speed
library(doParallel)
cl <- makePSOCKcluster(10)
registerDoParallel(cl)

memory.limit(size=56000)
set.seed(seed)
surv_mod_l1 <- train(x, y,
                       method = "regLogistic", 
                       preProcess = c("center", "scale"),
                       trControl = control)

stopCluster(cl)

cl <- makePSOCKcluster(10)
registerDoParallel(cl)

memory.limit(size=56000)
set.seed(seed)
surv_mod_l1_2 <- train(x, y,
                     method = "regLogistic", 
                     preProcess = c("center", "scale"),
                     trControl = control,
                     tuneGrid = expand.grid(cost = c(.1, .5, 1, 2, 3),
                                            loss = "L2_dual",
                                            epsilon = c(.001, .005, .01, .05)))

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
surv_mod_l2 <- train(features, surv, method = 'regLogistic', 
                     preProc=c("center", "scale"), 
                     trControl=control2, 
                     tuneGrid = surv_mod_l1_2$bestTune)

stopCluster(cl)

surv_mod_dummies <- dummies
save(surv_mod_l2, surv_mod_dummies, 
     file = "../base-models-rf/surv-mod-l2.rds")
