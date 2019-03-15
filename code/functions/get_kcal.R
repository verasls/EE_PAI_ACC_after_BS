get_kcal <- function(df, ID_val) {
  # Computes kilocalory values for a single ID based on VO2 and VCO2
  #
  # Args:
  #   df: a data frame containing data for computation   
  #   ID_val: subject ID value
  #
  # Returns:
  #   A data frame adding a colunm with computed kilocalory 
  #   values to the imput data frame
  
  require(tidyverse)
  
  ID_df <- filter(df, ID == ID_val)
  
  ID_df$kcal <- NA
  for (i in 1:nrow(ID_df)) {
    ID_df$kcal[i] <- (3.941 * (ID_df$V_O2[i] / 1000)) + (1.106 * (ID_df$V_CO2[i] / 1000))
  }
  return(ID_df)
}