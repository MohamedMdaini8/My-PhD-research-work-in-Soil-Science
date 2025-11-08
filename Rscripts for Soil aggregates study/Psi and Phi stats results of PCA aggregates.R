library(PCAtest)

# perform scaling and centering to your data
data_scaled <- scale(df_response)

# pca and significance test
pca_results <- PCAtest(data_scaled)
# Example: Create a scree plot
plot(pca_results$eigenvalues, type="b", xlab="Principal Component", ylab="Eigenvalue", main="Scree Plot")

library(PCAtest)
# generate some random data (replace with your actual data)
set.seed(123)
data <- matrix(rnorm(100 * 5), ncol = 5)
colnames(data) <- paste0("Var", 1:5)

# pca and significance test
pca_results <- PCAtest(data)

# print the results
print(pca_results)

# example of making the scree plot
plot(pca_results$eigenvalues, type="b", xlab="Principal Component", ylab="Eigenvalue", main="Scree Plot")

# example of examining the variables in PC1
print(pca_results$variable_contributions[1,])


