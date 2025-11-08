library(tidyverse)

# make a function to compute Spearman correlation and p-values

correlation_matrix <- function(data, method = "spearman") {
  cor_res <- matrix(NA, ncol(data), ncol(data))
  p_res <- matrix(NA, ncol(data), ncol(data))
  colnames(cor_res) <- colnames(data)
  rownames(cor_res) <- colnames(data)
  colnames(p_res) <- colnames(data)
  rownames(p_res) <- colnames(data)
  
  for (i in 1:ncol(data)) {
    for (j in 1:ncol(data)) {
      test <- cor.test(data[[i]], data[[j]], method = method)
      cor_res[i, j] <- test$estimate
      p_res[i, j] <- test$p.value
    }
  }
  list(cor = cor_res, p = p_res)
}

a.env1 <- Full_dataset[, x:y]

# compute correlations and p-values

res <- correlation_matrix(a.env1)

# convert matrices to long format
cor_df <- as.data.frame(as.table(res$cor))
p_df <- as.data.frame(as.table(res$p))
colnames(cor_df) <- c("Var1", "Var2", "correlation")
colnames(p_df) <- c("Var1", "Var2", "pvalue")

# merge and classify

df <- left_join(cor_df, p_df, by = c("Var1", "Var2")) %>%
  mutate(sig = case_when(
    pvalue <= 0.001 ~ "***",
    pvalue <= 0.01 ~ "**",
    pvalue <= 0.05 ~ "*",
    TRUE ~ ""
  ),
  corr_label = sprintf("%.2f", correlation),
  is_upper = as.numeric(Var1) < as.numeric(Var2),
  is_diag = Var1 == Var2)

# make the plot with different text in upper and lower triangles

ggplot(df, aes(x = Var1, y = Var2, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(data = filter(df, is_upper), aes(label = corr_label), size = 2.7) +
  geom_text(data = filter(df, !is_upper & !is_diag), aes(label = sig), size = 3) +
  scale_fill_gradient2(low = "#6D9EC1", mid = "white", high = "#E46726",
                       midpoint = 0, limit = c(-1, 1), name = "Spearman") +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  coord_fixed() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid = element_blank()
  )
rstudioapi::versionInfo()$version
