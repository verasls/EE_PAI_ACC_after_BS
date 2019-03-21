# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(irr)

source(here("code", "scripts", "03_ROC_curves.R")) # Loads ROC curves
source(here("code", "functions", "cross_validate_ROC_curves.R"))
source(here("code", "functions", "percent_agreement.R"))

# Leave-one-out cross-validation ------------------------------------------

# 1 month after surgery

## AC
LOOCV_AC_ROC_1m <- do.call(
  rbind, (lapply(
    unique(hip_1m$ID),
    cross_validate_ROC_curves,
    df = hip_1m, acc_metric = "AC"
  ))
)

## ENMO
LOOCV_ENMO_ROC_1m <- do.call(
  rbind, (lapply(
    unique(hip_1m$ID),
    cross_validate_ROC_curves,
    df = hip_1m, acc_metric = "ENMO"
  ))
)

## MAD
LOOCV_MAD_ROC_1m <- do.call(
  rbind, (lapply(
    unique(hip_1m$ID),
    cross_validate_ROC_curves,
    df = hip_1m, acc_metric = "MAD"
  ))
)

# 6 months after surgery

## AC
LOOCV_AC_ROC_6m <- do.call(
  rbind, (lapply(
    unique(hip_6m$ID),
    cross_validate_ROC_curves,
    df = hip_6m, acc_metric = "AC"
  ))
)

## ENMO
LOOCV_ENMO_ROC_6m <- do.call(
  rbind, (lapply(
    unique(hip_6m$ID),
    cross_validate_ROC_curves,
    df = hip_6m, acc_metric = "ENMO"
  ))
)

## MAD
LOOCV_MAD_ROC_6m <- do.call(
  rbind, (lapply(
    unique(hip_6m$ID),
    cross_validate_ROC_curves,
    df = hip_6m, acc_metric = "MAD"
  ))
)

# 12 months after surgery

## AC
LOOCV_AC_ROC_12m <- do.call(
  rbind, (lapply(
    unique(hip_12m$ID),
    cross_validate_ROC_curves,
    df = hip_12m, acc_metric = "AC"
  ))
)

## ENMO
LOOCV_ENMO_ROC_12m <- do.call(
  rbind, (lapply(
    unique(hip_12m$ID),
    cross_validate_ROC_curves,
    df = hip_12m, acc_metric = "ENMO"
  ))
)

## MAD
LOOCV_MAD_ROC_12m <- do.call(
  rbind, (lapply(
    unique(hip_12m$ID),
    cross_validate_ROC_curves,
    df = hip_12m, acc_metric = "MAD"
  ))
)

# Kappa statistic ---------------------------------------------------------

# 1 month after surgery

## AC
kappa_AC_SED_1m  <- kappa2(select(LOOCV_AC_ROC_1m, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_AC_MOD_1m  <- kappa2(select(LOOCV_AC_ROC_1m, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_AC_VIG_1m  <- kappa2(select(LOOCV_AC_ROC_1m, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_AC_glob_1m <- kappa2(select(LOOCV_AC_ROC_1m, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                           weight = "squared")

## ENMO
kappa_ENMO_SED_1m  <- kappa2(select(LOOCV_ENMO_ROC_1m, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_ENMO_MOD_1m  <- kappa2(select(LOOCV_ENMO_ROC_1m, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_ENMO_VIG_1m  <- kappa2(select(LOOCV_ENMO_ROC_1m, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_ENMO_glob_1m <- kappa2(select(LOOCV_ENMO_ROC_1m, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                           weight = "squared")

## MAD
kappa_MAD_SED_1m  <- kappa2(select(LOOCV_MAD_ROC_1m, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_MAD_MOD_1m  <- kappa2(select(LOOCV_MAD_ROC_1m, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_MAD_VIG_1m  <- kappa2(select(LOOCV_MAD_ROC_1m, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_MAD_glob_1m <- kappa2(select(LOOCV_MAD_ROC_1m, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                             weight = "squared")

# 6 months after surgery

## AC
kappa_AC_SED_6m  <- kappa2(select(LOOCV_AC_ROC_6m, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_AC_MOD_6m  <- kappa2(select(LOOCV_AC_ROC_6m, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_AC_VIG_6m  <- kappa2(select(LOOCV_AC_ROC_6m, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_AC_glob_6m <- kappa2(select(LOOCV_AC_ROC_6m, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                           weight = "squared")

## ENMO
kappa_ENMO_SED_6m  <- kappa2(select(LOOCV_ENMO_ROC_6m, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_ENMO_MOD_6m  <- kappa2(select(LOOCV_ENMO_ROC_6m, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_ENMO_VIG_6m  <- kappa2(select(LOOCV_ENMO_ROC_6m, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_ENMO_glob_6m <- kappa2(select(LOOCV_ENMO_ROC_6m, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                             weight = "squared")

## MAD
kappa_MAD_SED_6m  <- kappa2(select(LOOCV_MAD_ROC_6m, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_MAD_MOD_6m  <- kappa2(select(LOOCV_MAD_ROC_6m, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_MAD_VIG_6m  <- kappa2(select(LOOCV_MAD_ROC_6m, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_MAD_glob_6m <- kappa2(select(LOOCV_MAD_ROC_6m, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                            weight = "squared")

# 12 months after surgery

## AC
kappa_AC_SED_6m  <- kappa2(select(LOOCV_AC_ROC_6m, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_AC_MOD_6m  <- kappa2(select(LOOCV_AC_ROC_6m, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_AC_VIG_6m  <- kappa2(select(LOOCV_AC_ROC_6m, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_AC_glob_6m <- kappa2(select(LOOCV_AC_ROC_6m, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                           weight = "squared")

## ENMO
kappa_ENMO_SED_6m  <- kappa2(select(LOOCV_ENMO_ROC_6m, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_ENMO_MOD_6m  <- kappa2(select(LOOCV_ENMO_ROC_6m, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_ENMO_VIG_6m  <- kappa2(select(LOOCV_ENMO_ROC_6m, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_ENMO_glob_6m <- kappa2(select(LOOCV_ENMO_ROC_6m, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                             weight = "squared")

## MAD
kappa_MAD_SED_6m  <- kappa2(select(LOOCV_MAD_ROC_6m, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_MAD_MOD_6m  <- kappa2(select(LOOCV_MAD_ROC_6m, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_MAD_VIG_6m  <- kappa2(select(LOOCV_MAD_ROC_6m, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_MAD_glob_6m <- kappa2(select(LOOCV_MAD_ROC_6m, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                            weight = "squared")

# Percent agreement -------------------------------------------------------

# 1 month after surgery

## AC
perc_agree_AC_ROC_1m <- percent_agreement(LOOCV_AC_ROC_1m, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

## ENMO
perc_agree_ENMO_ROC_1m <- percent_agreement(LOOCV_ENMO_ROC_1m, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

## MAD
perc_agree_MAD_ROC_1m <- percent_agreement(LOOCV_MAD_ROC_1m, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

# 6 months after surgery

## AC
perc_agree_AC_ROC_6m <- percent_agreement(LOOCV_AC_ROC_6m, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

## ENMO
perc_agree_ENMO_ROC_6m <- percent_agreement(LOOCV_ENMO_ROC_6m, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

## MAD
perc_agree_MAD_ROC_6m <- percent_agreement(LOOCV_MAD_ROC_6m, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

# 12 months after surgery

## AC
perc_agree_AC_ROC_12m <- percent_agreement(LOOCV_AC_ROC_12m, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

## ENMO
perc_agree_ENMO_ROC_12m <- percent_agreement(LOOCV_ENMO_ROC_12m, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

## MAD
perc_agree_MAD_ROC_12m <- percent_agreement(LOOCV_MAD_ROC_12m, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")