# Load packages -----------------------------------------------------------

library(readxl)
library(tidyverse)

# Read file ---------------------------------------------------------------

database <- read_excel("data/Database.xlsx", sheet = "Database")
database <- database[-1, ]

# Recoding ID to match "data/acc_metrics.csv" file
names(database)[1] <- "database_ID"
database$database_ID <- as.character(database$database_ID)

database$ID <- str_c(
  database$Evaluation, database$database_ID,
  sep = "0"
)

# Make cardio_data data frame ---------------------------------------------

cardio_data <- select(
  database,
  ID,
  eval = Evaluation,
  speed = Step,
  elevation = Elev.,
  HR,
  BF,
  V_E = "V'E",
  V_O2 = "V'O2",
  V_CO2 = "V'CO2",
  RER,
  VO2_kg = "VO2/kg"
)

cardio_data$elevation <- as.double(cardio_data$elevation)
cardio_data$HR        <- as.double(cardio_data$HR)
cardio_data$BF        <- as.double(cardio_data$BF)
cardio_data$V_E       <- as.double(cardio_data$V_E)
cardio_data$V_O2      <- as.double(cardio_data$V_O2)
cardio_data$V_CO2     <- as.double(cardio_data$V_CO2)
cardio_data$VO2_kg    <- as.double(cardio_data$VO2_kg)

if (file.exists("data/cardio_data.csv") == FALSE) {
  write_csv(cardio_data, "data/cardio_data.csv")
}

# Make sample_descriptives data frame -------------------------------------

sample_descriptives <- select(
  database,
  ID,
  eval = Evaluation,
  height = height_cm,
  weight = weight_kg,
  body_fat,
  sex,
  age,
  surgery_type = Surgery
)

if (file.exists("data/sample_descriptives.csv") == FALSE) {
  write_csv(sample_descriptives, "data/sample_descriptives.csv")
}