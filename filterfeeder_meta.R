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
  mutate(rate = coalesce(rate_formula, `calculated g rate`)) %>%
  mutate(species = factor(species, levels = c("Crassostrea virginica",
                                "Magallana gigas",
                                "Mytilus edulis",
                                "Mytilus trossulus",
                                "Mytilus californianus",
                                "Mytilus galloprovinciallis")))

# mean per individuals
indiv <- lit_combined %>%
  filter(unit %in% c("l_hr_ind", "ml_min_ind", "ml_hr_ind", "l_h_ind")) %>%
  filter(min_max_mean == "mean")

indv_stand <- indiv %>%
  mutate(standard_filtration = case_when(unit == "ml_min_ind" ~ rate_formula * 0.001 * 60,
                                         unit == "ml_hr_ind" ~ rate_formula * 0.001,
                                         .default = rate_formula)) %>%
  filter(!is.na(standard_filtration))

# number of studies
unique(indv_stand$source)

indv_gg <- indv_stand %>%
  ggplot(aes(x = species, y = standard_filtration, fill = species)) +
  geom_boxplot(alpha = 0.3, width = 0.2, linewidth = 1.05) +
  geom_jitter(shape = 21, size = 5, width = 0.1, alpha = 0.7) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_viridis(discrete = TRUE, begin = 0.2, drop = FALSE) +
  ylab(expression(paste(L, ~h^{-1} , ~ind^{-1}))) +
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) +
  annotate("text", x = 5.5, y = 3.75, label = "studies (n) = 3")

# mean per unit weight
dryweight <- lit_combined %>%
  filter(unit %in% c("l_hr_g", "l_hr", "l_g_hr", "ml_hr_g")) %>%
  filter(min_max_mean == "mean")

weight_stand <- dryweight %>%
  mutate(standard_filtration = case_when(unit == "ml_hr_g" ~ rate * 0.001,
                                         .default = rate))

# number of studies
unique(weight_stand$source)

weight_gg <- weight_stand %>%
  ggplot(aes(x = species, y = standard_filtration, fill = species)) +
  geom_boxplot(alpha = 0.3, width = 0.2, linewidth = 1.05) +
  geom_jitter(shape = 21, size = 5, width = 0.1, alpha = 0.7) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_viridis(discrete = TRUE, begin = 0.2, drop = FALSE) +
  ylab(expression(paste(L, ~g^{-1} , ~h^{-1}))) +
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0, 0.5, 0, 0.23), "cm")) +
  annotate("text", x = 5.5, y = 16, label = "studies (n) = 17")

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

# number of studies
unique(zscore$source)

zscore_gg <- zscore %>%
  ggplot(aes(x = species, y = `z score`, fill = species)) +
  geom_boxplot(alpha = 0.3, width = 0.2, linewidth = 1.05) +
  geom_jitter(shape = 21, size = 5, width = 0.1, alpha = 0.7) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_viridis(discrete = TRUE, begin = 0.2, drop = FALSE) +
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0, 0.5, 0, 0.6), "cm")) +
  annotate("text", x = 5.5, y = 4, label = "studies (n) = 19")

ggarrange(indv_gg, weight_gg, zscore_gg, ncol = 1, common.legend = TRUE)


############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####