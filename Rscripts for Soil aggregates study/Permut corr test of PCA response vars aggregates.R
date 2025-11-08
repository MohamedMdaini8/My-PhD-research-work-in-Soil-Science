library(vegan)

df_response <- Full_dataset[, x:y]
  
# standardize data
df_scaled <- scale(df_response)  # df_response = your 15 response variables

# run pca
pca_res <- rda(df_scaled)

# test the significance of all variables using envfit
fit <- envfit(pca_res, df_scaled, permutations = 999)

# show results
fit_table <- data.frame(
  Variable = rownames(fit$vectors$arrows),
  R2 = fit$vectors$r,
  P_value = fit$vectors$pvals
)

print(fit_table)

# save to csv
write.csv(fit_table, "PCA_variable_significance.csv", row.names = FALSE)
