library("tidyverse")
library("caret")

temp <- tempfile()
download.file(
  "https://github.com/nealmaker/tree-growth-nf/raw/master/data/nf-fia.rda", 
  temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(!is.na(ht_s)) %>% 
  select(plot, ht_s, spp, dbh_s, cr_s,
         ba_s, bal_s, site_class, lat, lon, elev) %>% 
  rename(ht = ht_s, dbh = dbh_s, cr = cr_s, ba = ba_s, bal = bal_s)

hts <- nf_fia$ht
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
trainy <- hts[-index]
test <- nf_fia[index, -1] # features[index, ]
testy <- hts[index]

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
ht_mod <- 
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

ht_mod_rf50 <- ranger(y = trainy, x = train,
                       mtry = ht_mod$bestTune$mtry,
                       min.node.size = ht_mod$bestTune$min.node.size,
                       splitrule = ht_mod$bestTune$splitrule,
                       num.trees = 50,
                       num.threads = 18)

stopCluster(cl)

ht_mod_rf <- ht_mod_rf50

save(ht_mod_rf, file = "../base-models-rf/ht-mod-rf.rda")
