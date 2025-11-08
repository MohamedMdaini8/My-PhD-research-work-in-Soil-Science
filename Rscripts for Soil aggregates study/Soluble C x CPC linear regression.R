# Load required libraries

library(tidyverse)
library(ggpubr)
library(patchwork)

df <- Full_dataset

p <- ggplot(df, aes(x = Full_dataset$`Soluble C`, y = CPC)) +
  geom_point(color = "#1b9e77", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#1b9e77", fill = "#1b9e77", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$Csol), label.y = max(df$CPC), size = 4) +
  labs(title = "A", x = "Soluble C (mg/kg)", y = "CPC (g/kg)") +
  theme_minimal()

p
