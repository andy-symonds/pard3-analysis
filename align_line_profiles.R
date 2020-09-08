# Line Profile Analysis
# Author: Andy Symonds
# Date: June 2020

# Load libraries
library(tidyverse)
library(openxlsx)
library(stringr)
library(here)

# 1. Code to align line profiles on max intensity value ####

raw_line_profiles <- read.xlsx("data/pard3fluorintensities.xlsx")

columns_to_use <- seq(2, 48, 2)
line_profiles_to_use <- raw_line_profiles %>%
  select(columns_to_use)

# Get row index of max value for each column
rowMax <- function(data) sapply(data, which.max)
row_index_max_of_each_column <- rowMax(line_profiles_to_use)
max_index <- as.numeric(max(row_index_max_of_each_column))

# How much padding is required for each column to align max values
padding_required_for_each_col <- max_index - row_index_max_of_each_column
n <- unname(padding_required_for_each_col)

df_len <- as.numeric(nrow(line_profiles_to_use))
output <- list()
for (i in 1:length(n)) {
  profile <- line_profiles_to_use[i]
  output[[i]] <- lag(profile, n[i])
}

aligned_line_profile <- do.call(cbind, output)

saveRDS(aligned_line_profile, "data/aligned_line_profiles.rds")

write.xlsx(aligned_line_profile, "data/aligned_line_profiles.xlsx")

# Trim data so no NAs in data frame 
# I inspected small dataset manually and I need to slice between row 108 to row 737 inclusive
aligned_line_profile_trimmed <- slice(aligned_line_profile, 108:737)

saveRDS(aligned_line_profile_trimmed, "data/aligned_line_profiles_trimmed.rds")

write.xlsx(aligned_line_profile_trimmed, "data/aligned_line_profiles_trimmed.xlsx")


# 2. Calculate statistics & plot data ####

all_line_profile_trim <- readRDS("data/aligned_line_profiles_trimmed.rds")

# Add ID column to allow tidying of data
all_line_profile_trim <- all_line_profile_trim %>% mutate(id = row_number())

# Max value for each embryo are aligned at row 313. Make this centre
# Convert into microns. 600 units/rows = 70 microns
all_line_profile_trim_distance <- all_line_profile_trim %>%
  slice(14:613) %>%
  mutate(distance = (id-13) * (70/600))

tp_no_letter <- all_line_profile_trim_distance %>%
  select(distance, "31", "32", "33", "34", "35", "36")

tp_a <- all_line_profile_trim_distance %>%
  select(distance, "31a", "32a", "33a", "34a", "35a", "36a")

tp_b <- all_line_profile_trim_distance %>%
  select(distance, "31b", "32b", "33b", "34b", "35b", "36b")

tp_c <- all_line_profile_trim_distance %>%
  select(distance, "31c", "32c", "33c", "34c", "35c", "36c")

# Tidy data
tidy_tp_no_letter <- gather(tp_no_letter, embryo, intensity, -distance)
tidy_tp_a <- gather(tp_a, embryo, intensity, -distance)
tidy_tp_b <- gather(tp_b, embryo, intensity, -distance)
tidy_tp_c <- gather(tp_c, embryo, intensity, -distance)

# Calculate mean and sd
tidy_tp_no_letter <- tidy_tp_no_letter %>% group_by(distance) %>%
  mutate(mean_intensity = mean(intensity),
         sd = sd(intensity),
         ci_low = mean_intensity - sd,
         ci_upper = mean_intensity + sd,
         timepoint = "0h") %>%
  select(distance, timepoint, mean_intensity, ci_low, ci_upper)

tidy_tp_a <- tidy_tp_a %>% group_by(distance) %>%
  mutate(mean_intensity = mean(intensity),
         sd = sd(intensity),
         ci_low = mean_intensity - sd,
         ci_upper = mean_intensity + sd,
         timepoint = "1h") %>%
  select(distance, timepoint, mean_intensity, ci_low, ci_upper)

tidy_tp_b <- tidy_tp_b %>% group_by(distance) %>%
  mutate(mean_intensity = mean(intensity),
         sd = sd(intensity),
         ci_low = mean_intensity - sd,
         ci_upper = mean_intensity + sd,
         timepoint = "2h") %>%
  select(distance, timepoint, mean_intensity, ci_low, ci_upper)

tidy_tp_c <- tidy_tp_c %>% group_by(distance) %>%
  mutate(mean_intensity = mean(intensity),
         sd = sd(intensity),
         ci_low = mean_intensity - sd,
         ci_upper = mean_intensity + sd,
         timepoint = "3h") %>%
  select(distance, timepoint, mean_intensity, ci_low, ci_upper)

# Data Preparation for ploting
# Data preparation
df <- rbind(tidy_tp_no_letter, tidy_tp_a, tidy_tp_b, tidy_tp_c)

# Visualization
# Basic plot with no SD
ggplot(df, aes(x = distance, y = mean_intensity)) + 
  geom_line(aes(color = timepoint, linetype = timepoint)) #+ 
  #scale_color_manual(values = c("darkred", "steelblue")) # Add manual colour to lines

ggplot(df, aes(x = distance, y = mean_intensity)) + 
  geom_line(aes(color = timepoint, linetype = timepoint)) +
  theme(#panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  theme_classic() +
  xlab(expression(paste("Distance (", mu, "m)"))) +
  ylab("Mean intensity (A.U.)")

ggsave("data/all_lines.tiff", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = NA, height = NA,
       dpi = 300, limitsize = TRUE)

# With SD... overlapping
# ggplot(df, aes(x = distance, y = mean_intensity, ymin = ci_low, ymax = ci_upper)) + 
#   geom_line(aes(color = timepoint, linetype = timepoint)) +
#   geom_ribbon(alpha = 0.5)

# Separate charts as subplots
ggplot(df, aes(x = distance, y = mean_intensity, ymin = ci_low, ymax = ci_upper)) + 
  geom_line() +
  geom_ribbon(alpha = 0.5, fill = "grey80") + 
  theme(#panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  #theme_classic() + # Add this line back in to get second option
  facet_wrap(~ timepoint, nrow = 2) +
  xlab(expression(paste("Distance (", mu, "m)"))) +
  ylab("Mean intensity (A.U.)")

ggsave("data/subplots2.tiff", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = NA, height = NA,
       dpi = 300, limitsize = TRUE)

