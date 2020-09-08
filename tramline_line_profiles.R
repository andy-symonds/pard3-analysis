# Line Profile Analysis for tramlines
# Author: Andy Symonds
# Date: July 2020

# Load libraries
library(tidyverse)
library(openxlsx)
library(stringr)
library(here)

# 1. Load in data
raw_tramline_profiles <- read.xlsx("data/pard3tramlines.xlsx")

saveRDS(raw_tramline_profiles, "data/tramline_profiles.rds")

# 2. Calculate statistics ####

tramline_profiles <- raw_tramline_profiles %>%
  mutate(id = row_number()) # Add id column to allow for tidying the data

# Convert into microns, 250 unit/rows (x-axis) = 30 microns
tramline_profiles_distance <- tramline_profiles %>%
  mutate(distance = id * (30/250))

# # Convert into micorns. 600 units/rows = 70 microns
# all_line_profile_trim_distance <- all_line_profile_trim %>%
#   slice(14:613) %>%
#   mutate(distance = (id-13) * (70/600))

tramline_profiles_distance <- tramline_profiles_distance %>%
  select(-id)

tidy_tramline_profiles <- gather(tramline_profiles_distance, embryo, intensity, -distance)

tidy_analysis_trameline_profiles <- tidy_tramline_profiles %>%
  group_by(distance) %>%
  mutate(mean_intensity = mean(intensity),
         sd = sd(intensity),
         ci_low = mean_intensity - sd,
         ci_upper = mean_intensity + sd) %>%
  select(distance, mean_intensity, ci_low, ci_upper)

# 3. Plot data

df <- tidy_analysis_trameline_profiles
# Visualization
# Basic plot with no SD
ggplot(df, aes(x = distance, y = mean_intensity, ymin = ci_low, ymax = ci_upper)) + 
  geom_line(color = "red") +
  geom_ribbon(alpha = 0.5, fill = "grey80") + 
  theme(#panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  #theme_classic() + # Add this line back in to get second option
  xlab(expression(paste("Distance (", mu, "m)"))) +
  ylab("Mean intensity (A.U.)") +
  coord_fixed(ratio = 1.2)
  

ggsave("data/tramlines.tiff", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = NA, height = NA,
       dpi = 300, limitsize = TRUE)





