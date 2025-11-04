# Install the package if you don't have it already
# install.packages("PCAtest")

# Load the package
library(PCAtest)

# Example: Assume your data is in a data frame called 'data'
# Perform scaling and centering
data_scaled <- scale(df_response)

# Perform PCA and significance tests
pca_results <- PCAtest(data_scaled)
# Example: Create a scree plot
plot(pca_results$eigenvalues, type="b", xlab="Principal Component", ylab="Eigenvalue", main="Scree Plot")

library(PCAtest)
# Generate some random data (replace with your actual data)
set.seed(123)
data <- matrix(rnorm(100 * 5), ncol = 5)
colnames(data) <- paste0("Var", 1:5)

# Perform PCA and significance tests
pca_results <- PCAtest(data)

# Print the results
print(pca_results)

# Example of plotting the scree plot
plot(pca_results$eigenvalues, type="b", xlab="Principal Component", ylab="Eigenvalue", main="Scree Plot")

# Example of examining variable contributions to PC1
print(pca_results$variable_contributions[1,])


