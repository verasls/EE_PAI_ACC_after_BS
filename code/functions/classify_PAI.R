classify_PAI <- function(df, acc_metric, class_name, cp_SED, cp_MOD, cp_VIG, cp_MVPA) {
  # Creates categorical variables for each of the intensity categories based on
  # the acc_metric values and selected cut-points values
  #
  # Args:
  #   df: a data frame containing data used to classify PAI
  #   acc_metric: a character string with the accelerometer metric name
  #   class_name: a character string with the name to be given to the PAI
  # categorical variables
  #   cp_SED, cp_MOD, cp_VIG, cp_MVPA: cut-points to, respectively, sedentary,
  #   moderate and vigorous PAI categories obtained from the coords() function
  #   of the pROC package
  #
  # Returns:
  #   A data frame containing the categorical variables for each intensity
  #   category
  
  require(tidyverse)
  
  # Creates and names the variables
  df$SED_CAT    <- rep(NA, nrow(df))
  df$LIG_CAT    <- rep(NA, nrow(df))
  df$MOD_CAT    <- rep(NA, nrow(df))
  df$VIG_CAT    <- rep(NA, nrow(df))
  df$MVPA_CAT   <- rep(NA, nrow(df))
  df$INTENS_CAT <- rep(NA, nrow(df))
  # Fills the variables
  for (i in 1:nrow(df)) {
    # 1 = TRUE; 0 = FALSE
    # Sedentary
    if (df[i, acc_metric] <= cp_SED[[1]]) {
      df$SED_CAT[i] <- 1
    } else {df$SED_CAT[i] <- 0}
    # Light
    if (df[i, acc_metric] > cp_SED[[1]] &
        df[i, acc_metric] < cp_MOD[[1]]) {
      df$LIG_CAT[i] <- 1
    } else {df$LIG_CAT[i] <- 0}
    # Moderate
    if (df[i, acc_metric] >= cp_MOD[[1]] &
        df[i, acc_metric] <  cp_VIG[[1]]) {
      df$MOD_CAT[i] <- 1
    } else {df$MOD_CAT[i] <- 0}
    # Vigorous
    if (df[i, acc_metric] >= cp_VIG[[1]]) {
      df$VIG_CAT[i] <- 1
    } else {df$VIG_CAT[i] <- 0}
    # Moderate to vigorous
    if (df[i, acc_metric] >= cp_MVPA[[1]]) {
      df$MVPA_CAT[i] <- 1
    } else {df$MVPA_CAT[i] <- 0}
    
    # 1 = Sedentary; 2 = Light; 3 = Moderate; 4 = Vigorous
    if (df$SED_CAT[i] == 1) {
      df$INTENS_CAT[i] <- 1
    } else {
      if (df$LIG_CAT[i] == 1) {
        df$INTENS_CAT[i] <- 2
      } else {
        if (df$MOD_CAT[i] == 1) {
          df$INTENS_CAT[i] <- 3
        } else {
          if (df$VIG_CAT[i] == 1) {
            df$INTENS_CAT[i] <- 4
          }
        }
      }
    }
  }
  
  names(df)[which(names(df) == "SED_CAT")]    <- str_c("SED_CAT_by_", class_name, sep = "")
  names(df)[which(names(df) == "LIG_CAT")]    <- str_c("LIG_CAT_by_", class_name, sep = "")   
  names(df)[which(names(df) == "MOD_CAT")]    <- str_c("MOD_CAT_by_", class_name, sep = "")   
  names(df)[which(names(df) == "VIG_CAT")]    <- str_c("VIG_CAT_by_", class_name, sep = "")
  names(df)[which(names(df) == "MVPA_CAT")]   <- str_c("MVPA_CAT_by_", class_name, sep = "")
  names(df)[which(names(df) == "INTENS_CAT")] <- str_c("INTENS_CAT_by_", class_name, sep = "")
  
  return(df)
}