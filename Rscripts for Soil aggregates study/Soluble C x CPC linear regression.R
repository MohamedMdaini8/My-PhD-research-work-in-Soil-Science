# Load required libraries
library(tidyverse)
library(ggpubr)
library(patchwork)

# Assuming your dataset is named df
df <- Full_dataset_aggregates_MM  # Replace with your actual data object

# Create individual plots
p1 <- ggplot(df, aes(x = Full_dataset_aggregates_MM$`Soluble C`, y = CPC)) +
  geom_point(color = "#1b9e77", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#1b9e77", fill = "#1b9e77", alpha = 0.2) +
  stat_cor(method = "spearman", label.x = min(df$Csol), label.y = max(df$CPC), size = 4) +
  labs(title = "A", x = "Soluble C (mg/kg)", y = "CPC (g/kg)") +
  theme_minimal()




# Combine plots with patchwork
p1 +
  plot_annotation(title = "",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))
