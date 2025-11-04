# Load required libraries
library(tidyverse)
library(ggpubr)
library(patchwork)

# Assuming your dataset is named df
df <- Full_dataset_aggregates2  # Replace with your actual data object

# Create individual plots
p1 <- ggplot(df, aes(x = CO2, y = SOC1)) +
  geom_point(color = "#1b9e77", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#1b9e77", fill = "#1b9e77", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$CO2), label.y = max(df$SOC1), size = 4) +
  labs(title = "A", x = "CO2", y = "SOC in >2 mm") +
  theme_minimal()

p2 <- ggplot(df, aes(x = CO2, y = SOC2)) +
  geom_point(color = "#d95f02", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#d95f02", fill = "#d95f02", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$CO2), label.y = max(df$SOC2), size = 4) +
  labs(title = "B", x = "CO2", y = "SOC in 0.25–2 mm") +
  theme_minimal()

p3 <- ggplot(df, aes(x = CO2, y = SOC3)) +
  geom_point(color = "#7570b3", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#7570b3", fill = "#7570b3", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$CO2), label.y = max(df$SOC3), size = 4) +
  labs(title = "C", x = "CO2", y = "SOC in 0.053–0.25 mm") +
  theme_minimal()

p4 <- ggplot(df, aes(x = CO2, y = SOC4)) +
  geom_point(color = "#e7298a", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#e7298a", fill = "#e7298a", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$CO2), label.y = max(df$SOC4), size = 4) +
  labs(title = "D", x = "CO2", y = "SOC in <0.053 mm") +
  theme_minimal()

# Combine plots with patchwork
(p1 | p2) / (p3 | p4) +
  plot_annotation(title = "",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))
