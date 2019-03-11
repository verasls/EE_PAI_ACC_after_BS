# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)

# 1. Prepare files --------------------------------------------------------

acc_data    <- read_csv(here("data", "acc_data.csv"))
cardio_data <- read_csv(here("data", "cardio_data.csv")) %>% 
  filter(ID %in% acc_data$ID) # filter due to cardio_data having a greater
                              # number of subjects
sample_desc <- read_csv(here("data", "sample_descriptives.csv"))