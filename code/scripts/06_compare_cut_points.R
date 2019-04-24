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

## AC
# Build data frame
class_error_AC_6m <- cutpoints_AC_6m %>% 
  get_classification_error() %>% 
  select(ID, speed, cp_6m, cp_1m, cp_12m) %>% 
  gather(
    cp_6m, cp_1m, cp_12m,
    key = cut_point,
    value = correct_classification
  )
class_error_AC_6m$cut_point <- as_factor(class_error_AC_6m$cut_point)

# Run analysis
# 6m vs 1m
AC_cp_6m_vs_1m <- class_error_AC_6m %>% 
  filter(cut_point != "cp_12m")

CrossTable(
  AC_cp_6m_vs_1m$cut_point, AC_cp_6m_vs_1m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

# 6m vs 12m
AC_cp_6m_vs_12m <- class_error_AC_6m %>% 
  filter(cut_point != "cp_1m")

CrossTable(
  AC_cp_6m_vs_12m$cut_point, AC_cp_6m_vs_12m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

## ENMO
# Build data frame
class_error_ENMO_6m <- cutpoints_ENMO_6m %>% 
  get_classification_error() %>% 
  select(ID, speed, cp_6m, cp_1m, cp_12m) %>% 
  gather(
    cp_6m, cp_1m, cp_12m,
    key = cut_point,
    value = correct_classification
  )
class_error_ENMO_6m$cut_point <- as_factor(class_error_ENMO_6m$cut_point)

# Run analysis
# 6m vs 1m
ENMO_cp_6m_vs_1m <- class_error_ENMO_6m %>% 
  filter(cut_point != "cp_12m")

CrossTable(
  ENMO_cp_6m_vs_1m$cut_point, ENMO_cp_6m_vs_1m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

# 6m vs 12m
ENMO_cp_6m_vs_12m <- class_error_ENMO_6m %>% 
  filter(cut_point != "cp_1m")

CrossTable(
  ENMO_cp_6m_vs_12m$cut_point, ENMO_cp_6m_vs_12m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

## MAD
# Build data frame
class_error_MAD_6m <- cutpoints_MAD_6m %>% 
  get_classification_error() %>% 
  select(ID, speed, cp_6m, cp_1m, cp_12m) %>% 
  gather(
    cp_6m, cp_1m, cp_12m,
    key = cut_point,
    value = correct_classification
  )
class_error_MAD_6m$cut_point <- as_factor(class_error_MAD_6m$cut_point)

# Run analysis
# 6m vs 1m
MAD_cp_6m_vs_1m <- class_error_MAD_6m %>% 
  filter(cut_point != "cp_12m")

CrossTable(
  MAD_cp_6m_vs_1m$cut_point, MAD_cp_6m_vs_1m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

# 6m vs 12m
MAD_cp_6m_vs_12m <- class_error_MAD_6m %>% 
  filter(cut_point != "cp_1m")

CrossTable(
  MAD_cp_6m_vs_12m$cut_point, MAD_cp_6m_vs_12m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

# 12 months after surgery analysis ----------------------------------------

## AC
# Build data frame
class_error_AC_12m <- cutpoints_AC_12m %>% 
  get_classification_error() %>% 
  select(ID, speed, cp_12m, cp_1m, cp_6m) %>% 
  gather(
    cp_12m, cp_1m, cp_6m,
    key = cut_point,
    value = correct_classification
  )
class_error_AC_12m$cut_point <- as_factor(class_error_AC_12m$cut_point)

# Run analysis
# 12m vs 1m
AC_cp_12m_vs_1m <- class_error_AC_12m %>% 
  filter(cut_point != "cp_6m")

CrossTable(
  AC_cp_12m_vs_1m$cut_point, AC_cp_12m_vs_1m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

# 12m vs 6m
AC_cp_12m_vs_6m <- class_error_AC_12m %>% 
  filter(cut_point != "cp_1m")

CrossTable(
  AC_cp_12m_vs_6m$cut_point, AC_cp_12m_vs_6m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

## ENMO
# Build data frame
class_error_ENMO_12m <- cutpoints_ENMO_12m %>% 
  get_classification_error() %>% 
  select(ID, speed, cp_12m, cp_1m, cp_6m) %>% 
  gather(
    cp_12m, cp_1m, cp_6m,
    key = cut_point,
    value = correct_classification
  )
class_error_ENMO_12m$cut_point <- as_factor(class_error_ENMO_12m$cut_point)

# Run analysis
# 12m vs 1m
ENMO_cp_12m_vs_1m <- class_error_ENMO_12m %>% 
  filter(cut_point != "cp_6m")

CrossTable(
  ENMO_cp_12m_vs_1m$cut_point, ENMO_cp_12m_vs_1m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

# 12m vs 6m
ENMO_cp_12m_vs_6m <- class_error_ENMO_12m %>% 
  filter(cut_point != "cp_1m")

CrossTable(
  ENMO_cp_12m_vs_6m$cut_point, ENMO_cp_12m_vs_6m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

## MAD
# Build data frame
class_error_MAD_12m <- cutpoints_MAD_12m %>% 
  get_classification_error() %>% 
  select(ID, speed, cp_12m, cp_1m, cp_6m) %>% 
  gather(
    cp_12m, cp_1m, cp_6m,
    key = cut_point,
    value = correct_classification
  )
class_error_MAD_12m$cut_point <- as_factor(class_error_MAD_12m$cut_point)

# Run analysis
# 12m vs 1m
MAD_cp_12m_vs_1m <- class_error_MAD_12m %>% 
  filter(cut_point != "cp_6m")

CrossTable(
  MAD_cp_12m_vs_1m$cut_point, MAD_cp_12m_vs_1m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)

# 12m vs 6m
MAD_cp_12m_vs_6m <- class_error_MAD_12m %>% 
  filter(cut_point != "cp_1m")

CrossTable(
  MAD_cp_12m_vs_6m$cut_point, MAD_cp_12m_vs_6m$correct_classification,
  fisher = TRUE, chisq = TRUE, mcnemar = TRUE, 
  expected = TRUE, sresid = TRUE, format = "SPSS"
)