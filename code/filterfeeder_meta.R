#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# KBAY Filter Feeder Metaanalysis                                             ##
# Script created y2024-06-03                                                  ##
# Data source: Various                                                        ##
# R code prepared by Ross Whippo                                              ##
# Last updated 2024-06-03                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# lit_filtration.csv

# Associated Scripts:
# FILE.R

# TO DO 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                         ####
#                                                                              +
# LOAD PACKAGES                                                                +
# READ IN AND PREPARE DATA                                                     +
# MANIPULATE DATA                                                              +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                             ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(ggpubr)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lit_filtration <- read_csv("data/lit_filtration.csv", 
                           col_types = cols(rate_formula = col_double()))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

str(lit_filtration)
unique(lit_filtration$unit)

lit_combined <- lit_filtration %>% 
  mutate(rate = coalesce(rate_formula, `calculated g rate`))

# mean per individuals
indiv <- lit_combined %>%
  filter(unit %in% c("l_hr_ind", "ml_min_ind", "ml_hr_ind")) %>%
  filter(min_max_mean == "mean")

indv_stand <- indiv %>%
  mutate(standard_filtration = case_when(unit == "ml_min_ind" ~ rate_formula * 0.001 * 60,
                                         unit == "ml_hr_ind" ~ rate_formula * 0.001,
                                         .default = rate_formula)) %>%
  filter(!is.na(standard_filtration))

indv_stand %>%
  ggplot(aes(x = species, y = standard_filtration, fill = min_max_mean)) +
  geom_boxplot(alpha = 0.3, width = 0.2, linewidth = 1.05) +
  geom_jitter(shape = 21, size = 5, width = 0.1, alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE, begin = 0.2) +
  ylab(expression(paste(L, ~h^{-1} , ~ind^{-1}))) +
  theme_bw()

# mean per unit weight
dryweight <- lit_combined %>%
  filter(unit %in% c("l_hr_g", "l_hr", "l_g_hr", "ml_hr_g")) %>%
  filter(min_max_mean == "mean")

weight_stand <- dryweight %>%
  mutate(standard_filtration = case_when(unit == "ml_hr_g" ~ rate * 0.001,
                                         .default = rate))

weight_stand %>%
  ggplot(aes(x = species, y = standard_filtration, fill = min_max_mean)) +
  geom_boxplot(alpha = 0.3, width = 0.2, linewidth = 1.05) +
  geom_jitter(shape = 21, size = 5, width = 0.1, alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE, begin = 0.5) +
  ylab(expression(paste(L, ~g^{-1} , ~h^{-1}))) +
  theme_bw()

# z-score comparison
zscore <- lit_combined %>%
  filter(min_max_mean == "mean") %>%
  mutate(standard_filtration = case_when(unit == "ml_min_ind" ~ rate_formula * 0.001 * 60,
                                         unit == "ml_hr_ind" ~ rate_formula * 0.001,
                                         unit == "ml_hr_g" ~ rate * 0.001,
                                         .default = rate_formula)) %>%
  mutate(coal_rate = coalesce(`calculated g rate`, standard_filtration)) %>%
  mutate(`z score` = ((coal_rate - mean(coal_rate, na.rm = TRUE))/sd(coal_rate, na.rm = TRUE))) %>%
  filter(!is.na(`z score`))

zscore %>%
  ggplot(aes(x = species, y = `z score`, fill = min_max_mean)) +
  geom_boxplot(alpha = 0.3, width = 0.2, linewidth = 1.05) +
  geom_jitter(shape = 21, size = 5, width = 0.1, alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE, begin = 0.7) +
  theme_bw()


############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####