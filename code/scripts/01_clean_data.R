# Load packages and functions ---------------------------------------------

library(here)
library(readxl)
library(tidyverse)
source(here("code", "functions", "get_kcal.R"))
source(here("code", "functions", "get_MET.R"))
source(here("code", "functions", "get_PAI_categories.R"))

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

# Define possible samples:
# 1 = participants who have been evaluated on at least 2 of the 3 evaluations
# (2nd, 3rd, 4th)
# 2 = participants who have been evaluated on the 3 eval (2nd, 3rd and 4th),
# with training sessions attendance <= 50%
# 3 = participants who have been evaluated on the 3 eval (2nd, 3rd and 4th),
# no matter training sessions attendance
sample <- 1
if (sample == 1) {
  database <- database
} else {
  if (sample == 2) {
    database <- database %>% 
      filter(
        database_ID %in% c(20, 21, 28, 30, 34, 36, 43, 61, 72, 80)
      )
  } else {
    if (sample == 3) {
      database <- database %>% 
        filter(
          database_ID %in% c(
            20, 21, 22, 28, 29, 30, 34, 36, 38, 39, 43, 
            46, 49, 54, 55, 61, 62, 63, 68, 72, 73, 80
            )
        )
    }
  }
}

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
) %>% 
  mutate(speed = round(speed, 0)) %>% 
  filter(speed != 8)

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
  body_mass = weight_kg,
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
sample_desc$BMI <- NA
for (i in 1:nrow(sample_desc)) {
  sample_desc$BMI[i] <- 
    sample_desc$body_mass[i] / ((sample_desc$height[i] / 100)^2)
}

sample_desc <- select(sample_desc,
                      ID, eval, height, body_mass, BMI, body_fat,
                      sex, age, surgery_type) %>% 
  distinct(ID, .keep_all = TRUE)

if (file.exists(here("data", "sample_desc.csv")) == FALSE) {
  write_csv(sample_desc, here("data", "sample_desc.csv"))
}

# Merge data frames -------------------------------------------------------

hip <- inner_join(
  hip_acc_data, cardio_data, by = c("ID", "eval", "speed")
) %>% 
  inner_join(sample_desc, by = c("ID", "eval")) %>% 
  select(
   ID, eval, speed, AC = counts, ENMO, MAD, 
   V_O2, V_CO2, VO2_kg, body_mass, BMI 
  )

# Compute other variables -------------------------------------------------

# Compute kcal values
hip <- do.call(rbind, (lapply(unique(hip$ID), get_kcal, df = hip)))
# Compute MET values
hip <- do.call(rbind, (lapply(unique(hip$ID), get_MET, df = hip)))
# Get PAI categories by MET
hip <- get_PAI_categories(hip)

hip$eval <- as.factor(hip$eval)