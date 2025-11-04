library(tidyverse)
library(FactoMineR)
library(factoextra)
library(vegan)

# Run PCA
iris.pca <- PCA(Full_dataset_aggregates_MM[,5:44], graph = FALSE)

# Identify significant variables with envfit
response_vars <- Full_dataset_aggregates_MM[, 5:44]
fit <- envfit(iris.pca$ind$coord, response_vars, permutations = 999)
sig_vars <- names(which(fit$vectors$pvals < 0.05))

# Prepare point data (ensure factors)
points_data <- iris.pca$ind$coord %>%
  as_tibble() %>%
  mutate(
    Salinity = factor(Full_dataset_aggregates_MM$Salinity,
                      levels = c("S1","S5","S10")),
    Temperature = factor(Full_dataset_aggregates_MM$Temperature)
  )

# Arrows for significant variables
m <- 6
arrow_data <- iris.pca$var$coord %>%
  as_tibble(rownames = "par") %>%
  filter(par %in% sig_vars)

# Final plot
p <- ggplot() +
  # Points
  geom_point(data = points_data,
             aes(x = Dim.1, y = Dim.2,
                 colour = Salinity, shape = Temperature),
             size = 3.8, alpha = 0.9) +
  # Arrows
  geom_segment(data = arrow_data,
               aes(x = 0, xend = Dim.1 * m, y = 0, yend = Dim.2 * m),
               arrow = arrow(length = unit(0.22, "inches"), type = "closed"),
               colour = "grey30") +
  # Arrow labels (larger text)
  geom_text(data = arrow_data,
            aes(x = Dim.1 * m + 0.3, y = Dim.2 * m + 0.3, label = par),
            size = 4.5, fontface = "bold", colour = "black") +
  # Axes lines
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
  labs(
    x = paste0("PCA1 (", round(iris.pca$eig[1, 2], 1), "%)"),
    y = paste0("PCA2 (", round(iris.pca$eig[2, 2], 1), "%)"),
    colour = "Salinity", shape = "Temperature"
  ) +
  theme_bw(base_size = 14) +
  scale_shape_manual(values = c(19, 17)) +
  scale_colour_manual(values = c("S1" = "#D55E00",  # burnt orange
                                 "S5" = "#0072B2",  # blue
                                 "S10" = "#009E73")) # green

p

