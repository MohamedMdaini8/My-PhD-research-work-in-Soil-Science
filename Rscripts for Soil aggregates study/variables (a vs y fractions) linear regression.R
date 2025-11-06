library(tidyverse)
library(ggpubr)
library(patchwork)

df <- dataset  # Replace with your actual data object

# Create individual plots
p1 <- ggplot(df, aes(x = a, y = b)) +
  geom_point(color = "#1b9e77", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#1b9e77", fill = "#1b9e77", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$a), label.y = max(df$b), size = 4) +
  labs(title = "A", x = "a", y = "b in >2 mm") +
  theme_minimal()

p2 <- ggplot(df, aes(x = a, y = c)) +
  geom_point(color = "#d95f02", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#d95f02", fill = "#d95f02", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$a), label.y = max(df$c), size = 4) +
  labs(title = "B", x = "a", y = "c in 0.25–2 mm") +
  theme_minimal()

p3 <- ggplot(df, aes(x = a, y = d)) +
  geom_point(color = "#7570b3", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#7570b3", fill = "#7570b3", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$a), label.y = max(df$d), size = 4) +
  labs(title = "C", x = "a", y = "d in 0.053–0.25 mm") +
  theme_minimal()

p4 <- ggplot(df, aes(x = a, y = e)) +
  geom_point(color = "#e7298a", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#e7298a", fill = "#e7298a", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$a), label.y = max(df$e), size = 4) +
  labs(title = "D", x = "a", y = "e in <0.053 mm") +
  theme_minimal()

# Combine plots with patchwork
(p1 | p2) / (p3 | p4) +
  plot_annotation(title = "",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))
