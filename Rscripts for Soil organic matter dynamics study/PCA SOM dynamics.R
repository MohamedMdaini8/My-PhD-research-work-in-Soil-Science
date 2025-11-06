library(ggfortify)
library(plotly)
library(stats)
library(FactoMineR)
library(factoextra)

data <- inc_data[, x:y]
iris.pca <- PCA(data, graph = FALSE)
## Making the plot
plot(iris.pca)
library(tidyverse)

# The points in the graph
points_part <- iris.pca$ind$coord %>%  # Get the coordinates
  as_tibble() %>%  #
  mutate(Salinity = inc_data$Salinity,  # Add the salinity and temperature. Be careful that rstudio_results6 is the right source
         Temperature = inc_data$Temperature
         ) %>%
  geom_point(aes(x = Dim.1, y = Dim.2, colour = Salinity, shape = Temperature),
             data = ., inherit.aes = FALSE,
             size = 3)  # Here you can change the size of the points

m <- 8  # This is just a scaling factor, so the arrows look bigger (otherwise they are normalized to 1)

p <- iris.pca$var$coord %>%  # Get the data for the arrows
  as_tibble(rownames = "par") %>%
  ggplot() +
  points_part +  # Adding the points. Better to do it here otherwise they cannot be seen very well
  geom_segment(aes(x = 0, xend = Dim.1*m, y = 0, yend = Dim.2*m
                   ),
               arrow = arrow(length = unit(.2, "inches"), # the size of the arrow tip
                             type = "open") 
               ) +
  geom_text(aes(x = Dim.1*m+.2, y = Dim.2*m+.2, label = par  # The label on top of the arrow. The +.2 is to have a bit of gap
                )
                )
p +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey") +  # the horizontal line
  geom_vline(xintercept = 0, linetype = 2, colour = "grey") +  # the vertical line
  labs(x = paste0("PCA1 (", round(iris.pca$eig[1,2], 1), "%)"),  # Adding the % of variance explained to the axes. Be careful that the iris.pca is hard-coded.
       y = paste0("PCA2 (", round(iris.pca$eig[2,2], 1), "%)")
       ) +
  theme_minimal() +  # the overall theme
  scale_shape_manual(values = c(1, 2)) +  # the shapes
  scale_colour_manual(values = c("red", "green", "blue"))  # the colors for the points

summary(iris.pca)

#######
# the significance of variables in relation to principal components
pca_test <- dimdesc(iris.pca, axes = c(1,2), proba = 0.05)  # Significance at 5%
pca_test$Dim.1
pca_test$Dim.2
fviz_pca_var(iris.pca, col.var = "contrib", gradient.cols = c("blue", "red"))

# test if groups differ significantly in PCA space
pca_scores <- as.data.frame(iris.pca$ind$coord)
permanova <- adonis2(pca_scores ~ ., data = inc_data4[, 5:26], method = "euclidean", permutations = 999)

# Print results
print(permanova)

install.packages("remotes")
remotes::install_github("pmartinezarbizu/pairwiseAdonis")

library(pairwiseAdonis)

pairwise_results <- pairwise.adonis2(pca_scores ~., data = iris, permutations = 999)

print(pairwise_results)
