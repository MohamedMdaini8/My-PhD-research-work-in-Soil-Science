library(tidyverse)
library(ggpubr)
library(patchwork)

df <- Full_dataset

p <- ggplot(df, aes(x = MWD, y = GMD)) +
  geom_point(color = "#1b9e77", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#1b9e77", fill = "#1b9e77", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$MWD), label.y = max(df$GMD), size = 4) +
  labs(title = "A", x = "MWD (mm)", y = "GMD (mm)") +
  theme_minimal()

p
