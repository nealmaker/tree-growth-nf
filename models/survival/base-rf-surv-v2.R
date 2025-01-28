################################################################################
# Updated Workflow: Tree Survival Model Using LightGBM with GPU and SMOTE-ENN
# Replaces Random Forest (rfArb) and ROSE with LightGBM and SMOTE-ENN
################################################################################

# Load necessary libraries
requireNamespace("lightgbm")
requireNamespace("smotefamily") # For SMOTE-ENN
requireNamespace("dplyr")
requireNamespace("caret")
requireNamespace("doParallel")

# Download and preprocess data
# Retains the previous data loading and filtering steps
temp <- tempfile()
download.file(
  "https://github.com/nealmaker/tree-growth-nf/raw/master/data/nf-fia.rda", 
  temp)
load(temp)

nf_fia <- nf_fia  |> 
  dplyr::filter(status_change != "cut") |> 
  dplyr::mutate(lived = as.factor(as.character(status_change))) |> 
  dplyr::select(plot, lived, interval, spp, dbh_s, cr_s, ba_s, bal_s, site_class, lat, lon, elev) |> 
  dplyr::rename(dbh = dbh_s, cr = cr_s, ba = ba_s, bal = bal_s)

surv <- nf_fia$lived
plots <- nf_fia$plot

nf_fia <- dplyr::select(nf_fia, -plot)

# Define test set based on plots to ensure independence
set.seed(10)
test_plots <- sample(unique(plots), 
                     size = round(0.2 * length(unique(plots))), 
                     replace = FALSE)

index <- which(plots %in% test_plots)
train <- nf_fia[-index, -1]
trainy <- surv[-index]
test <- nf_fia[index, -1]
testy <- surv[index]

################################################################################
# Step 1: SMOTE Upsampling
################################################################################

# SHOULD PROABABLY APPLY SMOTE TO EACH SPECIES SEPARATELY, THEN COMBINE: With
# species as a numeric factor, knn will assume that close spp #s are
# functionally close (ordered), but they're not. 

# Combine features and labels for SMOTE
train_data <- cbind(data.frame(y = as.numeric(trainy) - 1), train) # Convert factors to numeric for SMOTE

# Ensure all data columns are numeric
train_data <- train_data |> dplyr::mutate(across(everything(), as.numeric))

# Apply SMOTE
smote <- smotefamily::SMOTE(train_data[, -1], train_data$y, K = 5)
train_smote <- smote$data
colnames(train_smote)[ncol(train_smote)] <- "y"
trainy <- as.factor(train_smote$y)
train <- train_smote |> dplyr::select(-y)

################################################################################
# Step 2: LightGBM Setup and Training
################################################################################

# GET SPP FACTORS BACK SO IT'S USABLE IN FORESTMAKER!!!!!!!!!!!!!!!!!!
# Convert data to LightGBM dataset format
lgb_train <- lightgbm::lgb.Dataset(data = as.matrix(train), label = as.numeric(trainy) - 1)
lgb_test <- lightgbm::lgb.Dataset(data = as.matrix(test), label = as.numeric(testy) - 1)

# Define parameters for LightGBM
params <- list(
  boosting_type = "gbdt",  # Gradient Boosting Decision Tree
  objective = "binary",   # Binary classification
  metric = "auc",         # Area Under Curve as evaluation metric
  device = "gpu",         # Use GPU for training
  max_depth = 10,          # Max depth of trees
  learning_rate = 0.1,     # Learning rate
  num_leaves = 31,         # Maximum number of leaves in one tree
  min_data_in_leaf = 20,   # Minimum data points per leaf
  feature_fraction = 0.8,  # Fraction of features to consider at each split
  bagging_fraction = 0.8,  # Fraction of data for bagging
  bagging_freq = 5         # Frequency of bagging
)

# Train the LightGBM model
set.seed(1234)
surv_model <- lightgbm::lgb.train(
  params = params,
  data = lgb_train,
  nrounds = 1000,                 # Number of boosting rounds
  valids = list(test = lgb_test), # Validation set
  early_stopping_rounds = 50,     # Stop early if no improvement
  verbose = 1
)

################################################################################
# Step 3: Model Evaluation and Simplification
################################################################################

# Evaluate performance on the test set
predictions <- predict(surv_model, as.matrix(test))
auc_score <- pROC::auc(pROC::roc(as.numeric(testy) - 1, predictions))
cat("AUC on test set:", auc_score, "\n")

# Save a simplified version of the model for faster inference
lightgbm::lgb.save(surv_model, "survival_model.txt") # Save model to file
simplified_model <- lightgbm::lgb.load("survival_model.txt")

################################################################################
# Step 4: Optional Recalibration (Isotonic Regression)
################################################################################

# If needed, apply isotonic regression for recalibration
# Uncomment the lines below if recalibration is necessary

# library(isotone)
# calibration_model <- isotone::gpava(
#   y = predictions,
#   z = as.numeric(testy) - 1
# )
# recalibrated_preds <- predict(calibration_model, predictions)

################################################################################
# Save the final model
################################################################################
saveRDS(simplified_model, file = "survival_model_lightgbm.rds")
cat("Model saved as survival_model_lightgbm.rds\n")
