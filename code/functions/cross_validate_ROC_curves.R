cross_validate_ROC_curves <- function(df, ID_val, acc_metric) {
  # Cross validates the model, separating sample in:
  #  training dataset: used to build the model
  #  testing dataset: used to predict the model
  #
  # Args:
  #   df: a data frame containing data used to build ROC curves
  #   ID_val: subject ID value to be assigned to testing dataset
  #
  # Returns:
  #   A data frame containing testing dataset predictions
  
  require(tidyverse)
  require(pROC)
  
  df <- df %>% 
    as.data.frame() %>% 
    na.omit() %>% 
    select(
      ID, eval, speed, acc_metric, MET,
      SED_CAT_by_MET, LIG_CAT_by_MET,
      MOD_CAT_by_MET, VIG_CAT_by_MET,
      MVPA_CAT_by_MET, INTENS_CAT_by_MET
    )
  
  training <- filter(df, ID != ID_val)
  testing  <- filter(df, ID == ID_val)
  
  # Builds a ROC curve for each of the intensity categories using the training dataset
  cv_ROC_SED  <- roc(response = training[, "SED_CAT_by_MET"], predictor = training[, acc_metric])
  cv_ROC_MOD  <- roc(response = training[, "MOD_CAT_by_MET"], predictor = training[, acc_metric])
  cv_ROC_VIG  <- roc(response = training[, "VIG_CAT_by_MET"], predictor = training[, acc_metric])
  cv_ROC_MVPA <- roc(response = training[, "MVPA_CAT_by_MET"], predictor = training[, acc_metric])
  cv_cp_ROC_SED  <- coords(cv_ROC_SED, x = "best", best.method = "closest.topleft")
  cv_cp_ROC_MOD  <- coords(cv_ROC_MOD, x = "best", best.method = "closest.topleft")
  cv_cp_ROC_VIG  <- coords(cv_ROC_VIG, x = "best", best.method = "closest.topleft")
  cv_cp_ROC_MVPA <- coords(cv_ROC_MVPA, x = "best", best.method = "closest.topleft")
  
  # Creates categorical variables for each of the intensity categories 
  # based on accelerometer metric values in the testing dataset
  # Creates and names the variables
  testing$SED_CAT_by_ROC    <- rep(NA, nrow(testing))
  testing$LIG_CAT_by_ROC    <- rep(NA, nrow(testing))
  testing$MOD_CAT_by_ROC    <- rep(NA, nrow(testing))
  testing$VIG_CAT_by_ROC    <- rep(NA, nrow(testing))
  testing$MVPA_CAT_by_ROC   <- rep(NA, nrow(testing))
  testing$INTENS_CAT_by_ROC <- rep(NA, nrow(testing))
  # Fills the variables
  for (i in 1:nrow(testing)) {
    # 1 = TRUE; 0 = FALSE
    # Sedentary
    if (testing[i, acc_metric] <= cv_cp_ROC_SED[[1]]) {
      testing$SED_CAT_by_ROC[i] <- 1
    } else {testing$SED_CAT_by_ROC[i] <- 0}
    # Light
    if (testing[i, acc_metric] > cv_cp_ROC_SED[1] &
        testing[i, acc_metric] < cv_cp_ROC_MOD[1]) {
      testing$LIG_CAT_by_ROC[i] <- 1
    } else {testing$LIG_CAT_by_ROC[i] <- 0}
    # Moderate
    if (testing[i, acc_metric] >= cv_cp_ROC_MOD[1] &
        testing[i, acc_metric] <  cv_cp_ROC_VIG[1]) {
      testing$MOD_CAT_by_ROC[i] <- 1
    } else {testing$MOD_CAT_by_ROC[i] <- 0}
    # Vigorous
    if (testing[i, acc_metric] >= cv_cp_ROC_VIG[1]) {
      testing$VIG_CAT_by_ROC[i] <- 1
    } else {testing$VIG_CAT_by_ROC[i] <- 0}
    # Moderate to vigorous
    if (testing[i, acc_metric] >= cv_cp_ROC_MVPA[[1]]) {
      testing$MVPA_CAT_by_ROC[i] <- 1
    } else {testing$MVPA_CAT_by_ROC[i] <- 0}

    # 1 = Sedentary; 2 = Light; 3 = Moderate; 4 = Vigorous
    if (testing$SED_CAT_by_ROC[i] == 1) {
      testing$INTENS_CAT_by_ROC[i] <- 1
    } else {
      if (testing$LIG_CAT_by_ROC[i] == 1) {
        testing$INTENS_CAT_by_ROC[i] <- 2
      } else {
        if (testing$MOD_CAT_by_ROC[i] == 1) {
          testing$INTENS_CAT_by_ROC[i] <- 3
        } else {
          if (testing$VIG_CAT_by_ROC[i] == 1) {
            testing$INTENS_CAT_by_ROC[i] <- 4
          }
        }
      }
    }
  }
  testing <- as_tibble(testing)
  return(testing)
}