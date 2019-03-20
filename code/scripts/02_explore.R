# Load packages and functions ---------------------------------------------

library(here)
source(here("code", "scripts", "01_clean_data.R")) # Loads cleaned data

library(tidyverse)
library(broman)

# Sample descriptives -----------------------------------------------------

# Demographics
demographics <- summarise(
  .data = sample_desc,
  age_mean       = round(mean(age), digits = 1),
  age_sd         = round(sd(age), digits = 1),
  body_mass_mean = round(mean(body_mass), digits = 1),
  body_mass_sd   = round(sd(body_mass), digits = 1),
  height_mean    = round(mean(height), digits = 1),
  height_sd      = round(sd(height), digits = 1),
  BMI_mean       = round(mean(BMI), digits = 1),
  BMI_sd         = round(sd(BMI), digits = 1),
  body_fat_mean  = round(mean(body_fat), digits = 1),
  body_fat_sd    = round(sd(body_fat), digits = 1)
)
sex         <- table(sample_desc$sex)
n_surg_type <- table(sample_desc$surgery_type)

# Cardiorespiratory and accelerometry
cardio_acc_descriptives <- hip %>% 
  group_by(speed) %>% 
  summarise(
    n         = n(),
    kcal_mean = myround(mean(kcal), 2),
    kcal_sd   = myround(sd(kcal), 2),
    VO2_mean  = myround(mean(VO2_kg), 2),
    VO2_sd    = myround(sd(VO2_kg), 2),
    MET_mean  = myround(mean(MET), 2),
    MET_sd    = myround(sd(MET), 2),
    AC_mean   = round(mean(AC, na.rm = TRUE), 0),
    AC_sd     = round(sd(AC, na.rm = TRUE), 0),
    MAD_mean  = round(mean(MAD, na.rm = TRUE), 0),
    MAD_sd    = round(sd(MAD, na.rm = TRUE), 0),
    ENMO_mean = round(mean(ENMO, na.rm = TRUE), 0), 
    ENMO_sd   = round(sd(ENMO, na.rm = TRUE), 0)
  )

# Plots -------------------------------------------------------------------

# Scatter plots kcal X acc metric
kcal_AC_scatter <- ggplot(hip) +
  geom_point(aes(x = kcal, y = AC, colour = eval))

kcal_ENMO_scatter <- ggplot(hip) +
  geom_point(aes(x = kcal, y = ENMO, colour = eval))

kcal_MAD_scatter <- ggplot(hip) +
  geom_point(aes(x = kcal, y = MAD, colour = eval))

# Smooth line plots kcal X acc metric
kcal_AC_smooth <- ggplot(hip) +
  geom_smooth(aes(x = kcal, y = AC))

kcal_ENMO_smooth <- ggplot(hip) +
  geom_smooth(aes(x = kcal, y = ENMO))

kcal_MAD_smooth <- ggplot(hip) +
  geom_smooth(aes(x = kcal, y = MAD))

# Scatter plots MET X acc metric
MET_AC_scatter <- ggplot(hip) +
  geom_point(aes(x = MET, y = AC, colour = eval)) +
  scale_x_continuous(breaks = c(1.5, 3, 6)) +
  theme(panel.grid.major.x = element_line(color = "red"))

MET_ENMO_scatter <- ggplot(hip) +
  geom_point(aes(x = MET, y = ENMO, colour = eval)) +
  scale_x_continuous(breaks = c(1.5, 3, 6)) +
  theme(panel.grid.major.x = element_line(color = "red"))

MET_MAD_scatter <- ggplot(hip) +
  geom_point(aes(x = MET, y = MAD, colour = eval)) +
  scale_x_continuous(breaks = c(1.5, 3, 6)) +
  theme(panel.grid.major.x = element_line(color = "red"))