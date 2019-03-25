# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(pgirmess)

source(here("code", "scripts", "05_classify_PAI.R")) # Loads ROC curves

# 1 month after surgery analysis ------------------------------------------

## AC
# Build data frame
class_error_AC_1m <- cutpoints_AC_1m %>% 
  select(
    ID, speed, INTENS_CAT_by_MET, 
    INTENS_CAT_by_1m_cp, INTENS_CAT_by_6m_cp, INTENS_CAT_by_12m_cp
    ) %>% 
  # classification error between values from indirect calorimetry and 
  # from cut-points developed for 1, 6 and 12 months after surgery
  mutate(
    cp_1m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_1m_cp),   
    cp_6m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_6m_cp),
    cp_12m = abs(INTENS_CAT_by_MET - INTENS_CAT_by_12m_cp),
  ) %>% 
  select(ID, speed, cp_1m, cp_6m, cp_12m) %>% 
  gather(
    cp_1m, cp_6m, cp_12m,
    key = cut_point,
    value = absolute_error
  )
class_error_AC_1m$cut_point <- as_factor(class_error_AC_1m$cut_point)

# Run analysis
KW_AC_1m <- kruskal.test(absolute_error ~ cut_point, data = class_error_AC_1m)

# Post hoc
if (KW_AC_1m[[3]] < 0.05) {
  AC_1m_posthoc <- kruskalmc(absolute_error ~ cut_point, data = class_error_AC_1m)
}

## ENMO
# Build data frame
class_error_ENMO_1m <- cutpoints_ENMO_1m %>% 
  select(
    ID, speed, INTENS_CAT_by_MET, 
    INTENS_CAT_by_1m_cp, INTENS_CAT_by_6m_cp, INTENS_CAT_by_12m_cp
  ) %>% 
  # classification error between values from indirect calorimetry and 
  # from cut-points developed for 1, 6 and 12 months after surgery
  mutate(
    cp_1m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_1m_cp),   
    cp_6m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_6m_cp),
    cp_12m = abs(INTENS_CAT_by_MET - INTENS_CAT_by_12m_cp),
  ) %>% 
  select(ID, speed, cp_1m, cp_6m, cp_12m) %>% 
  gather(
    cp_1m, cp_6m, cp_12m,
    key = cut_point,
    value = absolute_error
  )
class_error_ENMO_1m$cut_point <- as_factor(class_error_ENMO_1m$cut_point)

# Run analysis
KW_ENMO_1m <- kruskal.test(absolute_error ~ cut_point, data = class_error_ENMO_1m)

# Post hoc
if (KW_ENMO_1m[[3]] < 0.05) {
  ENMO_1m_posthoc <- kruskalmc(absolute_error ~ cut_point, data = class_error_ENMO_1m)
}

## MAD
# Build data frame
class_error_MAD_1m <- cutpoints_MAD_1m %>% 
  select(
    ID, speed, INTENS_CAT_by_MET, 
    INTENS_CAT_by_1m_cp, INTENS_CAT_by_6m_cp, INTENS_CAT_by_12m_cp
  ) %>% 
  # classification error between values from indirect calorimetry and 
  # from cut-points developed for 1, 6 and 12 months after surgery
  mutate(
    cp_1m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_1m_cp),   
    cp_6m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_6m_cp),
    cp_12m = abs(INTENS_CAT_by_MET - INTENS_CAT_by_12m_cp),
  ) %>% 
  select(ID, speed, cp_1m, cp_6m, cp_12m) %>% 
  gather(
    cp_1m, cp_6m, cp_12m,
    key = cut_point,
    value = absolute_error
  )
class_error_MAD_1m$cut_point <- as_factor(class_error_MAD_1m$cut_point)

# Run analysis
KW_MAD_1m <- kruskal.test(absolute_error ~ cut_point, data = class_error_MAD_1m)

# Post hoc
if (KW_MAD_1m[[3]] < 0.05) {
  MAD_1m_posthoc <- kruskalmc(absolute_error ~ cut_point, data = class_error_MAD_1m)
}

# 6 months after surgery analysis -----------------------------------------

## AC
# Build data frame
class_error_AC_6m <- cutpoints_AC_6m %>% 
  select(
    ID, speed, INTENS_CAT_by_MET, 
    INTENS_CAT_by_6m_cp, INTENS_CAT_by_1m_cp, INTENS_CAT_by_12m_cp
  ) %>% 
  # classification error between values from indirect calorimetry and 
  # from cut-points developed for 1, 6 and 12 months after surgery
  mutate(
    cp_6m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_6m_cp),   
    cp_1m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_1m_cp),
    cp_12m = abs(INTENS_CAT_by_MET - INTENS_CAT_by_12m_cp),
  ) %>% 
  select(ID, speed, cp_6m, cp_1m, cp_12m) %>% 
  gather(
    cp_6m, cp_1m, cp_12m,
    key = cut_point,
    value = absolute_error
  )
class_error_AC_6m$cut_point <- as_factor(class_error_AC_6m$cut_point)

# Run analysis
KW_AC_6m <- kruskal.test(absolute_error ~ cut_point, data = class_error_AC_6m)

# Post hoc
if (KW_AC_6m[[3]] < 0.05) {
  AC_6m_posthoc <- kruskalmc(absolute_error ~ cut_point, data = class_error_AC_6m)
}

## ENMO
# Build data frame
class_error_ENMO_6m <- cutpoints_ENMO_6m %>% 
  select(
    ID, speed, INTENS_CAT_by_MET, 
    INTENS_CAT_by_6m_cp, INTENS_CAT_by_1m_cp, INTENS_CAT_by_12m_cp
  ) %>% 
  # classification error between values from indirect calorimetry and 
  # from cut-points developed for 1, 6 and 12 months after surgery
  mutate(
    cp_6m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_6m_cp),   
    cp_1m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_1m_cp),
    cp_12m = abs(INTENS_CAT_by_MET - INTENS_CAT_by_12m_cp),
  ) %>% 
  select(ID, speed, cp_6m, cp_1m, cp_12m) %>% 
  gather(
    cp_6m, cp_1m, cp_12m,
    key = cut_point,
    value = absolute_error
  )
class_error_ENMO_6m$cut_point <- as_factor(class_error_ENMO_6m$cut_point)

# Run analysis
KW_ENMO_6m <- kruskal.test(absolute_error ~ cut_point, data = class_error_ENMO_6m)

# Post hoc
if (KW_ENMO_6m[[3]] < 0.05) {
  ENMO_6m_posthoc <- kruskalmc(absolute_error ~ cut_point, data = class_error_ENMO_6m)
}

## MAD
# Build data frame
class_error_MAD_6m <- cutpoints_MAD_6m %>% 
  select(
    ID, speed, INTENS_CAT_by_MET, 
    INTENS_CAT_by_6m_cp, INTENS_CAT_by_1m_cp, INTENS_CAT_by_12m_cp
  ) %>% 
  # classification error between values from indirect calorimetry and 
  # from cut-points developed for 1, 6 and 12 months after surgery
  mutate(
    cp_6m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_6m_cp),   
    cp_1m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_1m_cp),
    cp_12m = abs(INTENS_CAT_by_MET - INTENS_CAT_by_12m_cp),
  ) %>% 
  select(ID, speed, cp_6m, cp_1m, cp_12m) %>% 
  gather(
    cp_6m, cp_1m, cp_12m,
    key = cut_point,
    value = absolute_error
  )
class_error_MAD_6m$cut_point <- as_factor(class_error_MAD_6m$cut_point)

# Run analysis
KW_MAD_6m <- kruskal.test(absolute_error ~ cut_point, data = class_error_MAD_6m)

# Post hoc
if (KW_MAD_6m[[3]] < 0.05) {
  MAD_6m_posthoc <- kruskalmc(absolute_error ~ cut_point, data = class_error_MAD_6m)
}

# 12 months analysis ------------------------------------------------------

## AC
# Build data frame
class_error_AC_12m <- cutpoints_AC_12m %>% 
  select(
    ID, speed, INTENS_CAT_by_MET, 
    INTENS_CAT_by_12m_cp, INTENS_CAT_by_1m_cp, INTENS_CAT_by_6m_cp
  ) %>% 
  # classification error between values from indirect calorimetry and 
  # from cut-points developed for 1, 6 and 12 months after surgery
  mutate(
    cp_12m = abs(INTENS_CAT_by_MET - INTENS_CAT_by_12m_cp),
    cp_1m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_1m_cp),
    cp_6m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_6m_cp),   
  ) %>% 
  select(ID, speed, cp_12m, cp_1m, cp_6m) %>% 
  gather(
    cp_12m, cp_1m, cp_6m,
    key = cut_point,
    value = absolute_error
  )
class_error_AC_12m$cut_point <- as_factor(class_error_AC_12m$cut_point)

# Run analysis
KW_AC_12m <- kruskal.test(absolute_error ~ cut_point, data = class_error_AC_12m)

# Post hoc
if (KW_AC_12m[[3]] < 0.05) {
  AC_12m_posthoc <- kruskalmc(absolute_error ~ cut_point, data = class_error_AC_12m)
}

## ENMO
# Build data frame
class_error_ENMO_12m <- cutpoints_ENMO_12m %>% 
  select(
    ID, speed, INTENS_CAT_by_MET, 
    INTENS_CAT_by_12m_cp, INTENS_CAT_by_1m_cp, INTENS_CAT_by_6m_cp
  ) %>% 
  # classification error between values from indirect calorimetry and 
  # from cut-points developed for 1, 6 and 12 months after surgery
  mutate(
    cp_12m = abs(INTENS_CAT_by_MET - INTENS_CAT_by_12m_cp),
    cp_1m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_1m_cp),
    cp_6m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_6m_cp),   
  ) %>% 
  select(ID, speed, cp_12m, cp_1m, cp_6m) %>% 
  gather(
    cp_12m, cp_1m, cp_6m,
    key = cut_point,
    value = absolute_error
  )
class_error_ENMO_12m$cut_point <- as_factor(class_error_ENMO_12m$cut_point)

# Run analysis
KW_ENMO_12m <- kruskal.test(absolute_error ~ cut_point, data = class_error_ENMO_12m)

# Post hoc
if (KW_ENMO_12m[[3]] < 0.05) {
  ENMO_12m_posthoc <- kruskalmc(absolute_error ~ cut_point, data = class_error_ENMO_12m)
}

## MAD
# Build data frame
class_error_MAD_12m <- cutpoints_MAD_12m %>% 
  select(
    ID, speed, INTENS_CAT_by_MET, 
    INTENS_CAT_by_12m_cp, INTENS_CAT_by_1m_cp, INTENS_CAT_by_6m_cp
  ) %>% 
  # classification error between values from indirect calorimetry and 
  # from cut-points developed for 1, 6 and 12 months after surgery
  mutate(
    cp_12m = abs(INTENS_CAT_by_MET - INTENS_CAT_by_12m_cp),
    cp_1m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_1m_cp),
    cp_6m  = abs(INTENS_CAT_by_MET - INTENS_CAT_by_6m_cp),   
  ) %>% 
  select(ID, speed, cp_12m, cp_1m, cp_6m) %>% 
  gather(
    cp_12m, cp_1m, cp_6m,
    key = cut_point,
    value = absolute_error
  )
class_error_MAD_12m$cut_point <- as_factor(class_error_MAD_12m$cut_point)

# Run analysis
KW_MAD_12m <- kruskal.test(absolute_error ~ cut_point, data = class_error_MAD_12m)

# Post hoc
if (KW_MAD_12m[[3]] < 0.05) {
  MAD_12m_posthoc <- kruskalmc(absolute_error ~ cut_point, data = class_error_MAD_12m)
}