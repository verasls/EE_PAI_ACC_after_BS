get_classification_error <- function(df) {
  # Checks whether there is a classification error of the MVPA category between
  # the METs and each cut-point
  #
  # Args:
  #   df: a data frame containing data for computation
  #
  # Returns:
  #   the data frame with a variable for each cut-point classification error
  
  df$cp_1m  <- NA
  df$cp_6m  <- NA
  df$cp_12m <- NA
  
  for (i in 1:nrow(df)) {
    if (df$MVPA_CAT_by_MET[i] == df$MVPA_CAT_by_1m_cp[i]) {
      df$cp_1m[i] <- "yes"
    } else {df$cp_1m[i] <- "no"}
    
    if (df$MVPA_CAT_by_MET[i] == df$MVPA_CAT_by_6m_cp[i]) {
      df$cp_6m[i] <- "yes"
    } else {df$cp_6m[i] <- "no"}
    
    if (df$MVPA_CAT_by_MET[i] == df$MVPA_CAT_by_12m_cp[i]) {
      df$cp_12m[i] <- "yes"
    } else {df$cp_12m[i] <- "no"}
  }
  
  return(df)
}