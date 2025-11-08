library(tidyverse)
library(ggpubr)
library(patchwork)

df <- Full_dataset

# create individual plots

p1 <- ggplot(df, aes(x = Csol, y = SOC1)) +
  geom_point(color = "#1b9e77", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#1b9e77", fill = "#1b9e77", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$Csol), label.y = max(df$SOC1), size = 4) +
  labs(title = "A", x = "Soluble C (mg/kg)", y = "SOC in >2 mm") +
  theme_minimal()

p2 <- ggplot(df, aes(x = Csol, y = SOC2)) +
  geom_point(color = "#d95f02", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#d95f02", fill = "#d95f02", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$Csol), label.y = max(df$SOC2), size = 4) +
  labs(title = "B", x = "Soluble C (mg/kg)", y = "SOC in 0.25–2 mm") +
  theme_minimal()

p3 <- ggplot(df, aes(x = Csol, y = SOC3)) +
  geom_point(color = "#7570b3", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#7570b3", fill = "#7570b3", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$Csol), label.y = max(df$SOC3), size = 4) +
  labs(title = "C", x = "Soluble C (mg/kg)", y = "SOC in 0.053–0.25 mm") +
  theme_minimal()

p4 <- ggplot(df, aes(x = Csol, y = SOC4)) +
  geom_point(color = "#e7298a", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#e7298a", fill = "#e7298a", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$Csol), label.y = max(df$SOC4), size = 4) +
  labs(title = "D", x = "Soluble C (mg/kg)", y = "SOC in <0.053 mm") +
  theme_minimal()

# combine plots with patchwork
(p1 | p2) / (p3 | p4) +
  plot_annotation(title = "",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))
