# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(gmodels)

source(here("code", "scripts", "05_classify_PAI.R")) # Loads ROC curves
source(here("code", "functions", "get_classification_error.R"))

# 1 month after surgery analysis ------------------------------------------

## AC
# Build data frame
class_error_AC_1m <- cutpoints_AC_1m %>% 
  get_classification_error() %>% 
  select(ID, speed, cp_1m, cp_6m, cp_12m) %>% 
  gather(
    cp_1m, cp_6m, cp_12m,
    key = cut_point,
    value = correct_classification
  )
class_error_AC_1m$cut_point <- as_factor(class_error_AC_1m$cut_point)

# Run analysis
# 1m vs 6m
AC_cp_1m_vs_6m <- class_error_AC_1m %>% 
  filter(cut_point != "cp_12m")

CrossTable(
  AC_cp_1m_vs_6m$cut_point, AC_cp_1m_vs_6m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

# 1m vs 12m
AC_cp_1m_vs_12m <- class_error_AC_1m %>% 
  filter(cut_point != "cp_6m")

CrossTable(
  AC_cp_1m_vs_12m$cut_point, AC_cp_1m_vs_12m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

## ENMO
# Build data frame
class_error_ENMO_1m <- cutpoints_ENMO_1m %>% 
  get_classification_error() %>% 
  select(ID, speed, cp_1m, cp_6m, cp_12m) %>% 
  gather(
    cp_1m, cp_6m, cp_12m,
    key = cut_point,
    value = correct_classification
  )
class_error_ENMO_1m$cut_point <- as_factor(class_error_ENMO_1m$cut_point)

# Run analysis
# 1m vs 6m
ENMO_cp_1m_vs_6m <- class_error_ENMO_1m %>% 
  filter(cut_point != "cp_12m")

CrossTable(
  ENMO_cp_1m_vs_6m$cut_point, ENMO_cp_1m_vs_6m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

# 1m vs 12m
ENMO_cp_1m_vs_12m <- class_error_AC_1m %>% 
  filter(cut_point != "cp_6m")

CrossTable(
  ENMO_cp_1m_vs_12m$cut_point, ENMO_cp_1m_vs_12m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

## MAD
# Build data frame
class_error_MAD_1m <- cutpoints_MAD_1m %>% 
  get_classification_error() %>% 
  select(ID, speed, cp_1m, cp_6m, cp_12m) %>% 
  gather(
    cp_1m, cp_6m, cp_12m,
    key = cut_point,
    value = correct_classification
  )
class_error_MAD_1m$cut_point <- as_factor(class_error_MAD_1m$cut_point)

# Run analysis
# 1m vs 6m
MAD_cp_1m_vs_6m <- class_error_MAD_1m %>% 
  filter(cut_point != "cp_12m")

CrossTable(
  MAD_cp_1m_vs_6m$cut_point, MAD_cp_1m_vs_6m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

# 1m vs 12m
MAD_cp_1m_vs_12m <- class_error_AC_1m %>% 
  filter(cut_point != "cp_6m")

CrossTable(
  MAD_cp_1m_vs_12m$cut_point, MAD_cp_1m_vs_12m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)


# 6 months after surgery analysis -----------------------------------------

# 12 months after surgery analysis ----------------------------------------