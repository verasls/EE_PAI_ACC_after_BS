# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)

source(here("code", "scripts", "04_analyse_ROC_validity.R")) # Loads ROC curves
source(here("code", "functions", "classify_PAI.R"))


# In this script, PAI at one month after surgery were classified with a
# cut-point set created for each one, six and twelve months after surgery. 
# The same procedure was done for six and twelve months after BS.

# 1 month after surgery analysis ------------------------------------------

# AC
cutpoints_AC_1m <- LOOCV_AC_ROC_1m %>% 
  select(
    everything(), MVPA_CAT_by_MET,
    MVPA_CAT_by_1m_cp = MVPA_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "AC", class_name = "6m_cp",
    cp_MVPA = cp_AC_ROC_MVPA_6m
  ) %>% 
  classify_PAI(
    acc_metric = "AC", class_name = "12m_cp",
    cp_MVPA = cp_AC_ROC_MVPA_12m
  )

# ENMO
cutpoints_ENMO_1m <- LOOCV_ENMO_ROC_1m %>% 
  select(
    everything(), MVPA_CAT_by_MET, 
    MVPA_CAT_by_1m_cp   = MVPA_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "ENMO", class_name = "6m_cp",
    cp_MVPA = cp_ENMO_ROC_MVPA_6m
  ) %>% 
  classify_PAI(
    acc_metric = "ENMO", class_name = "12m_cp",
    cp_MVPA = cp_ENMO_ROC_MVPA_12m
  )

# MAD
cutpoints_MAD_1m <- LOOCV_MAD_ROC_1m %>% 
  select(
    everything(), MVPA_CAT_by_MET,
    MVPA_CAT_by_1m_cp   = MVPA_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "MAD", class_name = "6m_cp",
    cp_MVPA = cp_MAD_ROC_MVPA_6m
  ) %>% 
  classify_PAI(
    acc_metric = "MAD", class_name = "12m_cp",
    cp_MVPA = cp_MAD_ROC_MVPA_12m
  )

# 6 months after surgery analysis -----------------------------------------

# AC
cutpoints_AC_6m <- LOOCV_AC_ROC_6m %>% 
  select(
    everything(), MVPA_CAT_by_MET,
    MVPA_CAT_by_6m_cp   = MVPA_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "AC", class_name = "1m_cp",
    cp_MVPA = cp_AC_ROC_MVPA_1m
  ) %>% 
  classify_PAI(
    acc_metric = "AC", class_name = "12m_cp",
    cp_MVPA = cp_AC_ROC_MVPA_12m
  )

# ENMO
cutpoints_ENMO_6m <- LOOCV_ENMO_ROC_6m %>% 
  select(
    everything(), MVPA_CAT_by_MET,
    MVPA_CAT_by_6m_cp   = MVPA_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "ENMO", class_name = "1m_cp",
    cp_MVPA = cp_ENMO_ROC_MVPA_1m
  ) %>% 
  classify_PAI(
    acc_metric = "ENMO", class_name = "12m_cp",
    cp_MVPA = cp_ENMO_ROC_MVPA_12m
  )

# MAD
cutpoints_MAD_6m <- LOOCV_MAD_ROC_6m %>% 
  select(
    everything(), MVPA_CAT_by_MET,
    MVPA_CAT_by_6m_cp   = MVPA_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "MAD", class_name = "1m_cp",
    cp_MVPA = cp_MAD_ROC_MVPA_1m
  ) %>% 
  classify_PAI(
    acc_metric = "MAD", class_name = "12m_cp",
    cp_MVPA = cp_MAD_ROC_MVPA_12m
  )

# 12 months after surgery analysis ----------------------------------------

# AC
cutpoints_AC_12m <- LOOCV_AC_ROC_12m %>% 
  select(
    everything(), MVPA_CAT_by_MET,
    MVPA_CAT_by_12m_cp   = MVPA_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "AC", class_name = "1m_cp",
    cp_MVPA = cp_AC_ROC_MVPA_1m
  ) %>% 
  classify_PAI(
    acc_metric = "AC", class_name = "6m_cp",
    cp_MVPA = cp_AC_ROC_MVPA_6m
  )

# ENMO
cutpoints_ENMO_12m <- LOOCV_ENMO_ROC_12m %>% 
  select(
    everything(), MVPA_CAT_by_MET,
    MVPA_CAT_by_12m_cp   = MVPA_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "ENMO", class_name = "1m_cp",
    cp_MVPA = cp_ENMO_ROC_MVPA_1m
  ) %>% 
  classify_PAI(
    acc_metric = "ENMO", class_name = "6m_cp",
    cp_MVPA = cp_ENMO_ROC_MVPA_6m
  )

# MAD
cutpoints_MAD_12m <- LOOCV_MAD_ROC_12m %>% 
  select(
    everything(), MVPA_CAT_by_MET,
    MVPA_CAT_by_12m_cp   = MVPA_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "MAD", class_name = "1m_cp",
    cp_MVPA = cp_MAD_ROC_MVPA_1m
  ) %>% 
  classify_PAI(
    acc_metric = "MAD", class_name = "6m_cp",
    cp_MVPA = cp_MAD_ROC_MVPA_6m
  )