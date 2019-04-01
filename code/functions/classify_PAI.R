classify_PAI <- function(df, acc_metric, class_name, cp_MVPA) {
  # Creates a categorical variable for moderate to vigorous (MVPA) intensity
  # category based on the acc_metric values and selected cut-point values
  #
  # Args:
  #   df: a data frame containing data used to classify PAI
  #   acc_metric: a character string with the accelerometer metric name
  #   class_name: a character string with the name to be given to the PAI
  # categorical variables
  #   cp_MVPA: cut-points to moderate to vigorous PAI category obtained 
  #   from the coords() function of the pROC package
  #
  # Returns:
  #   A data frame containing the categorical variables for each intensity
  #   category
  
  require(tidyverse)
  
  df$MVPA_CAT   <- rep(NA, nrow(df))
  
  for (i in 1:nrow(df)) {
    if (df[i, acc_metric] >= cp_MVPA[[1]]) {
      df$MVPA_CAT[i] <- 1
    } else {df$MVPA_CAT[i] <- 0}
  }
  
  names(df)[which(names(df) == "MVPA_CAT")] <- str_c("MVPA_CAT_by_", class_name, sep = "")
  
  return(df)
}