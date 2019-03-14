# Load packages -----------------------------------------------------------

library(here)
library(readxl)
library(tidyverse)

# Read acc_metrics.csv file -----------------------------------------------

hip_acc_data <- read_csv(here("data", "acc_data.csv"))

# Read Database.xlsx file -------------------------------------------------

database <- read_excel(here("data", "Database.xlsx"), sheet = "Database")
database <- database[-1, ]

# Recoding ID to match "data/acc_metrics.csv" file
names(database)[1] <- "database_ID"
database$database_ID <- as.character(database$database_ID)
# Certifies that str_length(database_ID) == 2 in all rows
for (i in 1:nrow(database)) {
  if (str_length(database$database_ID[i]) != 2) {
    database$database_ID[i] <- str_c(
      "0", database$database_ID[i], sep = ""
    )
  }
}
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

# Filter due to cardio_data having a greater number of subjects
cardio_data <- filter(
  cardio_data, ID %in% hip_acc_data$ID
) 

if (file.exists(here("data", "cardio_data.csv")) == FALSE) {
  write_csv(cardio_data, here("data", "cardio_data.csv"))
}

# Make sample descriptives data frame -------------------------------------

sample_desc <- select(
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

# Filter due to sample_desc having a greater number of subjects
sample_desc <- filter(
  sample_desc, ID %in% hip_acc_data$ID
) 

# Get BMI
for (i in 1:nrow(sample_desc)) {
  sample_desc$BMI[i] <- 
    sample_desc$weight[i] / ((sample_desc$height[i] / 100)^2)
}

sample_desc <- select(sample_desc,
                      ID, eval, height, weight, BMI, body_fat,
                      sex, age, surgery_type) %>% 
  distinct(ID, .keep_all = TRUE)

if (file.exists(here("data", "sample_desc.csv")) == FALSE) {
  write_csv(sample_desc, here("data", "sample_desc.csv"))
}