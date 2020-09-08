# Generate heatmaps for ring development, rings per area

# Author: Andy Symonds
# Date: July 2020

# Load libraries
library(tidyverse)


# Load data
dat <- read.csv("data/ring_development_data2.csv", stringsAsFactors = FALSE)

# When I have greater than 1 embryo, I will make summary stats here, taking mean
# Generate confidence intervals if required
tp_15ss <- dat %>%
  select(X, "a14ss", "b15ss")

tp_17ss <- dat %>%
  select(X, "a16ss", "b17ss")

tp_19ss <- dat %>%
  select(X, "a19ss", "b20ss")

tp_21ss <- dat %>%
  select(X, "a21ss", "b21ss")

# Tidy data
tidy_tp_15ss <- gather(tp_15ss, embryo, density, -X)
tidy_tp_17ss <- gather(tp_17ss, embryo, density, -X)
tidy_tp_19ss <- gather(tp_19ss, embryo, density, -X)
tidy_tp_21ss <- gather(tp_21ss, embryo, density, -X)

tidy_tp_15ss_stats <- tidy_tp_15ss %>% group_by(X) %>%
  mutate(mean_density = mean(density),
         sd = sd(density),
         ci_low = mean_density - sd,
         ci_upper = mean_density + sd,
         timepoint = "15ss") %>%
  select(X, timepoint, mean_density, ci_low, ci_upper)

tidy_tp_17ss_stats <- tidy_tp_17ss %>% group_by(X) %>%
  mutate(mean_density = mean(density),
         sd = sd(density),
         ci_low = mean_density - sd,
         ci_upper = mean_density + sd,
         timepoint = "17ss") %>%
  select(X, timepoint, mean_density, ci_low, ci_upper)

tidy_tp_19ss_stats <- tidy_tp_19ss %>% group_by(X) %>%
  mutate(mean_density = mean(density),
         sd = sd(density),
         ci_low = mean_density - sd,
         ci_upper = mean_density + sd,
         timepoint = "19ss") %>%
  select(X, timepoint, mean_density, ci_low, ci_upper)

tidy_tp_21ss_stats <- tidy_tp_21ss %>% group_by(X) %>%
  mutate(mean_density = mean(density),
         sd = sd(density),
         ci_low = mean_density - sd,
         ci_upper = mean_density + sd,
         timepoint = "21ss") %>%
  select(X, timepoint, mean_density, ci_low, ci_upper)

# Plot data
df <- rbind(tidy_tp_15ss_stats, tidy_tp_17ss_stats, tidy_tp_19ss_stats, tidy_tp_21ss_stats)
df <- df %>%
  select(-ci_low, -ci_upper)


# Plot data
# Use factors to order DV levels
df$X <- factor(df$X, levels = c('ventral', 'mid-ventral', 'mid-dorsal', 'dorsal'))

ggplot(df, aes(x = timepoint, y = X, fill = mean_density, frame = timepoint)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient(name = expression(paste("Rings/10", mu, "m"^2)),
                      low = "blue", high = "red") +
  #coord_equal() +
  labs(x = "Timepoint", y = "Dorsoventral level", title = "Average ring density along dorso-ventral axis") +
  theme(plot.title = element_text(hjust = 0.5))
  

ggsave("data/ring_density_3.tiff", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = NA, height = NA,
       dpi = 300, limitsize = TRUE)
