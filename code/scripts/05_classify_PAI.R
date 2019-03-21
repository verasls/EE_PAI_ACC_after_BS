# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)

source(here("code", "scripts", "04_ROC_validity_analysis.R")) # Loads ROC curves
source(here("code", "functions", "classify_PAI.R"))

# 1 month after surgery analysis ------------------------------------------

# AC
cutpoints_AC_1m <- LOOCV_AC_ROC_1m %>% 
  select(
    ID, eval, speed, AC, MET,
    SED_CAT_by_MET, LIG_CAT_by_MET, 
    MOD_CAT_by_MET, VIG_CAT_by_MET, 
    INTENS_CAT_by_MET,
    SED_CAT_by_1m_cp    = SED_CAT_by_ROC,
    LIG_CAT_by_1m_cp    = LIG_CAT_by_ROC,
    MOD_CAT_by_1m_cp    = MOD_CAT_by_ROC,
    VIG_CAT_by_1m_cp    = VIG_CAT_by_ROC,
    INTENS_CAT_by_1m_cp = INTENS_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "AC", class_name = "6m_cp",
    cp_SED = cp_AC_ROC_SED_6m,
    cp_MOD = cp_AC_ROC_MOD_6m,
    cp_VIG = cp_AC_ROC_VIG_6m
  ) %>% 
  classify_PAI(
    acc_metric = "AC", class_name = "12m_cp",
    cp_SED = cp_AC_ROC_SED_12m,
    cp_MOD = cp_AC_ROC_MOD_12m,
    cp_VIG = cp_AC_ROC_VIG_12m
  )

# ENMO
cutpoints_ENMO_1m <- LOOCV_ENMO_ROC_1m %>% 
  select(
    ID, eval, speed, ENMO, MET,
    SED_CAT_by_MET, LIG_CAT_by_MET, 
    MOD_CAT_by_MET, VIG_CAT_by_MET, 
    INTENS_CAT_by_MET,
    SED_CAT_by_1m_cp    = SED_CAT_by_ROC,
    LIG_CAT_by_1m_cp    = LIG_CAT_by_ROC,
    MOD_CAT_by_1m_cp    = MOD_CAT_by_ROC,
    VIG_CAT_by_1m_cp    = VIG_CAT_by_ROC,
    INTENS_CAT_by_1m_cp = INTENS_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "ENMO", class_name = "6m_cp",
    cp_SED = cp_ENMO_ROC_SED_6m,
    cp_MOD = cp_ENMO_ROC_MOD_6m,
    cp_VIG = cp_ENMO_ROC_VIG_6m
  ) %>% 
  classify_PAI(
    acc_metric = "ENMO", class_name = "12m_cp",
    cp_SED = cp_ENMO_ROC_SED_12m,
    cp_MOD = cp_ENMO_ROC_MOD_12m,
    cp_VIG = cp_ENMO_ROC_VIG_12m
  )

# MAD
cutpoints_MAD_1m <- LOOCV_MAD_ROC_1m %>% 
  select(
    ID, eval, speed, MAD, MET,
    SED_CAT_by_MET, LIG_CAT_by_MET, 
    MOD_CAT_by_MET, VIG_CAT_by_MET, 
    INTENS_CAT_by_MET,
    SED_CAT_by_1m_cp    = SED_CAT_by_ROC,
    LIG_CAT_by_1m_cp    = LIG_CAT_by_ROC,
    MOD_CAT_by_1m_cp    = MOD_CAT_by_ROC,
    VIG_CAT_by_1m_cp    = VIG_CAT_by_ROC,
    INTENS_CAT_by_1m_cp = INTENS_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "MAD", class_name = "6m_cp",
    cp_SED = cp_MAD_ROC_SED_6m,
    cp_MOD = cp_MAD_ROC_MOD_6m,
    cp_VIG = cp_MAD_ROC_VIG_6m
  ) %>% 
  classify_PAI(
    acc_metric = "MAD", class_name = "12m_cp",
    cp_SED = cp_MAD_ROC_SED_12m,
    cp_MOD = cp_MAD_ROC_MOD_12m,
    cp_VIG = cp_MAD_ROC_VIG_12m
  )

# 6 months after surgery analysis -----------------------------------------

# AC
cutpoints_AC_6m <- LOOCV_AC_ROC_6m %>% 
  select(
    ID, eval, speed, AC, MET,
    SED_CAT_by_MET, LIG_CAT_by_MET, 
    MOD_CAT_by_MET, VIG_CAT_by_MET, 
    INTENS_CAT_by_MET,
    SED_CAT_by_6m_cp    = SED_CAT_by_ROC,
    LIG_CAT_by_6m_cp    = LIG_CAT_by_ROC,
    MOD_CAT_by_6m_cp    = MOD_CAT_by_ROC,
    VIG_CAT_by_6m_cp    = VIG_CAT_by_ROC,
    INTENS_CAT_by_6m_cp = INTENS_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "AC", class_name = "1m_cp",
    cp_SED = cp_AC_ROC_SED_1m,
    cp_MOD = cp_AC_ROC_MOD_1m,
    cp_VIG = cp_AC_ROC_VIG_1m
  ) %>% 
  classify_PAI(
    acc_metric = "AC", class_name = "12m_cp",
    cp_SED = cp_AC_ROC_SED_12m,
    cp_MOD = cp_AC_ROC_MOD_12m,
    cp_VIG = cp_AC_ROC_VIG_12m
  )

# ENMO
cutpoints_ENMO_6m <- LOOCV_ENMO_ROC_6m %>% 
  select(
    ID, eval, speed, ENMO, MET,
    SED_CAT_by_MET, LIG_CAT_by_MET, 
    MOD_CAT_by_MET, VIG_CAT_by_MET, 
    INTENS_CAT_by_MET,
    SED_CAT_by_6m_cp    = SED_CAT_by_ROC,
    LIG_CAT_by_6m_cp    = LIG_CAT_by_ROC,
    MOD_CAT_by_6m_cp    = MOD_CAT_by_ROC,
    VIG_CAT_by_6m_cp    = VIG_CAT_by_ROC,
    INTENS_CAT_by_6m_cp = INTENS_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "ENMO", class_name = "1m_cp",
    cp_SED = cp_ENMO_ROC_SED_1m,
    cp_MOD = cp_ENMO_ROC_MOD_1m,
    cp_VIG = cp_ENMO_ROC_VIG_1m
  ) %>% 
  classify_PAI(
    acc_metric = "ENMO", class_name = "12m_cp",
    cp_SED = cp_ENMO_ROC_SED_12m,
    cp_MOD = cp_ENMO_ROC_MOD_12m,
    cp_VIG = cp_ENMO_ROC_VIG_12m
  )

# MAD
cutpoints_MAD_6m <- LOOCV_MAD_ROC_6m %>% 
  select(
    ID, eval, speed, MAD, MET,
    SED_CAT_by_MET, LIG_CAT_by_MET, 
    MOD_CAT_by_MET, VIG_CAT_by_MET, 
    INTENS_CAT_by_MET,
    SED_CAT_by_6m_cp    = SED_CAT_by_ROC,
    LIG_CAT_by_6m_cp    = LIG_CAT_by_ROC,
    MOD_CAT_by_6m_cp    = MOD_CAT_by_ROC,
    VIG_CAT_by_6m_cp    = VIG_CAT_by_ROC,
    INTENS_CAT_by_6m_cp = INTENS_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "MAD", class_name = "1m_cp",
    cp_SED = cp_MAD_ROC_SED_1m,
    cp_MOD = cp_MAD_ROC_MOD_1m,
    cp_VIG = cp_MAD_ROC_VIG_1m
  ) %>% 
  classify_PAI(
    acc_metric = "MAD", class_name = "12m_cp",
    cp_SED = cp_MAD_ROC_SED_12m,
    cp_MOD = cp_MAD_ROC_MOD_12m,
    cp_VIG = cp_MAD_ROC_VIG_12m
  )

# 12 months after surgery analysis ----------------------------------------

# AC
cutpoints_AC_12m <- LOOCV_AC_ROC_12m %>% 
  select(
    ID, eval, speed, AC, MET,
    SED_CAT_by_MET, LIG_CAT_by_MET, 
    MOD_CAT_by_MET, VIG_CAT_by_MET, 
    INTENS_CAT_by_MET,
    SED_CAT_by_12m_cp    = SED_CAT_by_ROC,
    LIG_CAT_by_12m_cp    = LIG_CAT_by_ROC,
    MOD_CAT_by_12m_cp    = MOD_CAT_by_ROC,
    VIG_CAT_by_12m_cp    = VIG_CAT_by_ROC,
    INTENS_CAT_by_12m_cp = INTENS_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "AC", class_name = "1m_cp",
    cp_SED = cp_AC_ROC_SED_1m,
    cp_MOD = cp_AC_ROC_MOD_1m,
    cp_VIG = cp_AC_ROC_VIG_1m
  ) %>% 
  classify_PAI(
    acc_metric = "AC", class_name = "6m_cp",
    cp_SED = cp_AC_ROC_SED_6m,
    cp_MOD = cp_AC_ROC_MOD_6m,
    cp_VIG = cp_AC_ROC_VIG_6m
  )

# ENMO
cutpoints_ENMO_12m <- LOOCV_ENMO_ROC_12m %>% 
  select(
    ID, eval, speed, ENMO, MET,
    SED_CAT_by_MET, LIG_CAT_by_MET, 
    MOD_CAT_by_MET, VIG_CAT_by_MET, 
    INTENS_CAT_by_MET,
    SED_CAT_by_12m_cp    = SED_CAT_by_ROC,
    LIG_CAT_by_12m_cp    = LIG_CAT_by_ROC,
    MOD_CAT_by_12m_cp    = MOD_CAT_by_ROC,
    VIG_CAT_by_12m_cp    = VIG_CAT_by_ROC,
    INTENS_CAT_by_12m_cp = INTENS_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "ENMO", class_name = "1m_cp",
    cp_SED = cp_ENMO_ROC_SED_1m,
    cp_MOD = cp_ENMO_ROC_MOD_1m,
    cp_VIG = cp_ENMO_ROC_VIG_1m
  ) %>% 
  classify_PAI(
    acc_metric = "ENMO", class_name = "6m_cp",
    cp_SED = cp_ENMO_ROC_SED_6m,
    cp_MOD = cp_ENMO_ROC_MOD_6m,
    cp_VIG = cp_ENMO_ROC_VIG_6m
  )

# MAD
cutpoints_MAD_12m <- LOOCV_MAD_ROC_12m %>% 
  select(
    ID, eval, speed, MAD, MET,
    SED_CAT_by_MET, LIG_CAT_by_MET, 
    MOD_CAT_by_MET, VIG_CAT_by_MET, 
    INTENS_CAT_by_MET,
    SED_CAT_by_12m_cp    = SED_CAT_by_ROC,
    LIG_CAT_by_12m_cp    = LIG_CAT_by_ROC,
    MOD_CAT_by_12m_cp    = MOD_CAT_by_ROC,
    VIG_CAT_by_12m_cp    = VIG_CAT_by_ROC,
    INTENS_CAT_by_12m_cp = INTENS_CAT_by_ROC
  ) %>% 
  classify_PAI(
    acc_metric = "MAD", class_name = "1m_cp",
    cp_SED = cp_MAD_ROC_SED_1m,
    cp_MOD = cp_MAD_ROC_MOD_1m,
    cp_VIG = cp_MAD_ROC_VIG_1m
  ) %>% 
  classify_PAI(
    acc_metric = "MAD", class_name = "6m_cp",
    cp_SED = cp_MAD_ROC_SED_6m,
    cp_MOD = cp_MAD_ROC_MOD_6m,
    cp_VIG = cp_MAD_ROC_VIG_6m
  )