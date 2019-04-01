# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(pROC)

source(here("code", "scripts", "01_clean_data.R")) # Loads cleaned data

# Subset data frame -------------------------------------------------------

# 1 month after surgery
hip_1m <- hip %>% 
  filter(eval == "2nd")

# 6 months after surgery
hip_6m <- hip %>% 
  filter(eval == "3rd")

# 12 months after surgery
hip_12m <- hip %>% 
  filter(eval == "4th")

# ROC curves --------------------------------------------------------------

# 1 month after surgery

## AC
AC_ROC_MVPA_1m <- roc(MVPA_CAT_by_MET ~ AC, ci = TRUE, data = hip_1m, na.rm = TRUE)
cp_AC_ROC_MVPA_1m <- coords(AC_ROC_MVPA_1m, x = "best", best.method = "closest.topleft")

## ENMO
ENMO_ROC_MVPA_1m <- roc(MVPA_CAT_by_MET ~ ENMO, ci = TRUE, data = hip_1m, na.rm = TRUE)
cp_ENMO_ROC_MVPA_1m <- coords(ENMO_ROC_MVPA_1m, x = "best", best.method = "closest.topleft")

## MAD
MAD_ROC_MVPA_1m <- roc(MVPA_CAT_by_MET ~ MAD, ci = TRUE, data = hip_1m, na.rm = TRUE)
cp_MAD_ROC_MVPA_1m <- coords(MAD_ROC_MVPA_1m, x = "best", best.method = "closest.topleft")

# 6 months after surgery

## AC
AC_ROC_MVPA_6m <- roc(MVPA_CAT_by_MET ~ AC, ci = TRUE, data = hip_6m, na.rm = TRUE)
cp_AC_ROC_MVPA_6m <- coords(AC_ROC_MVPA_6m, x = "best", best.method = "closest.topleft")

## ENMO
ENMO_ROC_MVPA_6m <- roc(MVPA_CAT_by_MET ~ ENMO, ci = TRUE, data = hip_6m, na.rm = TRUE)
cp_ENMO_ROC_MVPA_6m <- coords(ENMO_ROC_MVPA_6m, x = "best", best.method = "closest.topleft")

## MAD
MAD_ROC_MVPA_6m <- roc(MVPA_CAT_by_MET ~ MAD, ci = TRUE, data = hip_6m, na.rm = TRUE)
cp_MAD_ROC_MVPA_6m <- coords(MAD_ROC_MVPA_6m, x = "best", best.method = "closest.topleft")

# 12 months after surgery

## AC
AC_ROC_MVPA_12m <- roc(MVPA_CAT_by_MET ~ AC, ci = TRUE, data = hip_12m, na.rm = TRUE)
cp_AC_ROC_MVPA_12m <- coords(AC_ROC_MVPA_12m, x = "best", best.method = "closest.topleft")

## ENMO
ENMO_ROC_MVPA_12m <- roc(MVPA_CAT_by_MET ~ ENMO, ci = TRUE, data = hip_12m, na.rm = TRUE)
cp_ENMO_ROC_MVPA_12m <- coords(ENMO_ROC_MVPA_12m, x = "best", best.method = "closest.topleft")

## MAD
MAD_ROC_MVPA_12m <- roc(MVPA_CAT_by_MET ~ MAD, ci = TRUE, data = hip_12m, na.rm = TRUE)
cp_MAD_ROC_MVPA_12m <- coords(MAD_ROC_MVPA_12m, x = "best", best.method = "closest.topleft")