# Install and load the vegan package
install.packages("vegan")  # Run once if not installed
library(vegan)
df_response <- Full_dataset_aggregates_MM[, 5:26]
  
# Step 1: Standardize your data
df_scaled <- scale(df_response)  # df_response = your 15 response variables

# Step 2: Run PCA
pca_res <- rda(df_scaled)

# Step 3: Test significance of all variables using envfit
fit <- envfit(pca_res, df_scaled, permutations = 999)

# Step 4: Show results (r² and p-values for all 15 variables)
fit_table <- data.frame(
  Variable = rownames(fit$vectors$arrows),
  R2 = fit$vectors$r,
  P_value = fit$vectors$pvals
)

print(fit_table)

# Optional: Save to CSV
write.csv(fit_table, "PCA_variable_significance.csv", row.names = FALSE)
