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
      ID, eval, speed, acc_metric,
      MET, MVPA_CAT_by_MET
    )
  
  training <- filter(df, ID != ID_val)
  testing  <- filter(df, ID == ID_val)
  
  # Builds a ROC curve for each of the intensity categories using the training dataset
  cv_ROC_MVPA <- roc(response = training[, "MVPA_CAT_by_MET"], predictor = training[, acc_metric])
  cv_cp_ROC_MVPA <- coords(cv_ROC_MVPA, x = "best", best.method = "closest.topleft")
  
  # Creates a categorical variables for MVPA intensity category
  # based on accelerometer metric values in the testing dataset
  
  testing$MVPA_CAT_by_ROC   <- rep(NA, nrow(testing))
  
  for (i in 1:nrow(testing)) {
    # 1 = TRUE; 0 = FALSE
    if (testing[i, acc_metric] >= cv_cp_ROC_MVPA[[1]]) {
      testing$MVPA_CAT_by_ROC[i] <- 1
    } else {testing$MVPA_CAT_by_ROC[i] <- 0}
  }
  
  testing <- as_tibble(testing)
  return(testing)
}