install.packages(c("ggplot2", "dplyr", "tidyr", "ggpubr", "ggsignif", "reshape2", "corrplot"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggsignif)
library(reshape2)
library(corrplot)

# Convert factors
df$temperature <- factor(df$temperature)
df$salinity <- factor(df$salinity)

# List of response variables (adjust this list)
response_vars <- c("SOC1", "SOC2",....)# x variables


# Convert to long format
df_long <- df %>%
  tidyr::pivot_longer(cols = all_of(response_vars), names_to = "variable", values_to = "value")

# Loop through variables to make individual boxplots
plots <- lapply(unique(df_long$variable), function(var_name) {
  df_sub <- df_long %>% filter(variable == var_name)
  
  ggplot(df_sub, aes(x = Salinity, y = value, fill = Temperature)) +
    geom_boxplot(position = position_dodge(0.8)) +
    labs(title = var_name, y = NULL, x = "Salinity") +
    theme_minimal(base_size = 13) +
    scale_fill_brewer(palette = "Set1") +
    stat_compare_means(aes(group = Salinity), 
                       method = "kruskal.test", label = "p.format", 
                       label.y.npc = "top", size = 3.5) +
    stat_compare_means(aes(group = Temperature), 
                       method = "kruskal.test", label = "p.format", 
                       label.y.npc = "bottom", size = 3.5)
})

# Combine into one figure
ggarrange(plotlist = plots, ncol = 3, nrow = ceiling(length(plots)/3),
          common.legend = TRUE, legend = "bottom")

