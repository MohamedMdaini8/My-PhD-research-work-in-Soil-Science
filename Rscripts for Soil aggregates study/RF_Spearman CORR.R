# COMPLETE INTEGRATED ANALYSIS: Random Forest + Spearman Correlations
# For Soil Salinity-Temperature Incubation Experiment

# Load required libraries
library(randomForest)
library(tidyverse)
library(corrplot)
library(viridis)
library(caret)
library(ggpubr)
library(reshape2)

# Set seed for reproducibility
set.seed(123)

# Clear workspace
rm(list = ls())

# =============================================================================
# STEP 1: DATA LOADING AND PREPARATION
# =============================================================================

cat("=== STEP 1: DATA LOADING ===\n")

# Read your data (replace with your actual file path)
df <- Full_dataset_aggregates_MM_2_

# Define variables (ADJUST THESE TO YOUR ACTUAL VARIABLE NAMES)
independent_vars <- c('Salinity', 'Temperature', 'Salinity vs. Temperature', 'Incubation_day')
dependent_vars <- c('MWD', 'GMD', 'CPC','SOC1', 'SOC2', 'SOC3', 'SOC4',
                    'TN1', 'TN2', 'TN3', 'TN4',
                    'CN1', 'CN2', 'CN3', 'CN4', 'frac1', 'frac2', 'frac3', 'frac4')

# Check data structure
cat("Data dimensions:", dim(df), "\n")
cat("Variables in dataset:", names(df), "\n")

# Select complete cases for analysis
analysis_data <- df %>% 
  select(all_of(c(independent_vars, dependent_vars))) %>%
  na.omit()

cat("Complete cases after removing NAs:", nrow(analysis_data), "\n")

# =============================================================================
# STEP 2: SPEARMAN CORRELATION ANALYSIS
# =============================================================================

cat("\n=== STEP 2: SPEARMAN CORRELATION ANALYSIS ===\n")

# Function to analyze correlations for each dependent variable
analyze_correlations <- function(dep_var) {
  cat("\nAnalyzing:", dep_var, "\n")
  
  y <- analysis_data[[dep_var]]
  
  # Initialize results
  variables <- character()
  rhos <- numeric()
  p_values <- numeric()
  
  # Calculate correlations for each independent variable
  for (var in independent_vars) {
    # Calculate Spearman correlation and p-value
    cor_test <- cor.test(analysis_data[[var]], y, method = "spearman", exact = FALSE)
    
    variables <- c(variables, var)
    rhos <- c(rhos, cor_test$estimate)
    p_values <- c(p_values, cor_test$p.value)
  }
  
  # Create results dataframe
  results <- data.frame(
    Dependent_variable = dep_var,
    Variable = variables,
    Spearman_rho = round(rhos, 3),
    P_value = round(p_values, 4),
    Significance = ifelse(p_values < 0.001, "***",
                          ifelse(p_values < 0.01, "**",
                                 ifelse(p_values < 0.05, "*", "ns")))
  )
  
  return(results)
}

# Run correlation analysis for all dependent variables
all_cor_results <- map_dfr(dependent_vars, analyze_correlations)

# Print correlation results
print(all_cor_results)

# =============================================================================
# STEP 3: RANDOM FOREST ANALYSIS
# =============================================================================

cat("\n=== STEP 3: RANDOM FOREST ANALYSIS ===\n")

# We'll analyze one response variable at a time
# Example: CPC (change as needed)
target_variable <- "CPC"

cat("Target variable for RF analysis:", target_variable, "\n")

# Prepare data for Random Forest
X <- analysis_data %>% select(all_of(independent_vars))
y <- analysis_data[[target_variable]]

# Train Random Forest model
rf_model <- randomForest(x = X, y = y, 
                         ntree = 500,
                         importance = TRUE,
                         keep.forest = TRUE)

# Print model summary
print(rf_model)
cat("R-squared:", round(1 - rf_model$mse[length(rf_model$mse)] / var(y), 3), "\n")

# Extract variable importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

# Create variable importance plot
vip_plot <- ggplot(importance_df, aes(x = reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(title = paste("Random Forest Variable Importance for", target_variable),
       x = "Predictor Variables", 
       y = "% Increase in MSE (Importance)") +
  theme_minimal()

print(vip_plot)

# =============================================================================
# STEP 4: INTEGRATED COMPARISON
# =============================================================================

cat("\n=== STEP 4: INTEGRATED COMPARISON ===\n")

# Filter correlation results for our target variable
target_cor_results <- all_cor_results %>% 
  filter(Dependent_variable == target_variable)

# Create comparison dataframe
comparison_df <- data.frame(
  Variable = independent_vars,
  RF_Importance = importance_df$`%IncMSE`[match(independent_vars, importance_df$Variable)],
  Spearman_rho = target_cor_results$Spearman_rho[match(independent_vars, target_cor_results$Variable)],
  Spearman_p = target_cor_results$P_value[match(independent_vars, target_cor_results$Variable)]
)

# Calculate ranks
comparison_df$RF_Rank <- rank(-comparison_df$RF_Importance)
comparison_df$Spearman_Rank <- rank(-abs(comparison_df$Spearman_rho))

# Determine agreement patterns
comparison_df$Agreement <- ifelse(
  abs(comparison_df$Spearman_rho) > 0.5 & comparison_df$RF_Importance > mean(comparison_df$RF_Importance),
  "High Agreement",
  ifelse(abs(comparison_df$Spearman_rho) < 0.3 & comparison_df$RF_Importance > mean(comparison_df$RF_Importance),
         "Nonlinear Relationship",
         "Moderate Agreement")
)

print(comparison_df)

# =============================================================================
# STEP 5: VISUALIZATIONS
# =============================================================================

cat("\n=== STEP 5: CREATING VISUALIZATIONS ===\n")

# 5.1 Side-by-side comparison plot
comparison_long <- comparison_df %>%
  pivot_longer(cols = c(RF_Importance, Spearman_rho),
               names_to = "Method", 
               values_to = "Value")

comp_plot <- ggplot(comparison_long, aes(x = Variable, y = Value, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("RF_Importance" = "blue", "Spearman_rho" = "red"),
                    labels = c("RF Importance", "Spearman Correlation")) +
  labs(title = paste("RF vs Spearman:", target_variable),
       subtitle = "Comparison of Variable Importance Measures",
       y = "Importance / Correlation Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(comp_plot)

# 5.2 Correlation matrix heatmap - OPTIMIZED FOR MANY VARIABLES

# Calculate correlation matrix with all variables
corr_matrix <- cor(analysis_data %>% select(all_of(c(independent_vars, dependent_vars))), 
                   method = "spearman")

# Create a larger plot window for better visibility
par(mfrow = c(1, 1), mar = c(2, 2, 4, 2))

# Option A: Heatmap without coefficients (cleaner for many variables)
corrplot(corr_matrix, 
         method = "color",
         type = "full",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.6,    # Even smaller text
         title = "Spearman Correlation Matrix: All Variables",
         mar = c(0, 0, 3, 0),
         addgrid.col = "gray80",  # Light grid lines
         cl.cex = 0.7)

# Option B: With hierarchical clustering to group related variables
corrplot(corr_matrix, 
         method = "color",
         type = "full",
         order = "hclust",  # Cluster similar variables together
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.6,
         title = "Spearman Correlation Matrix (Clustered)",
         mar = c(0, 0, 3, 0))

# Option C: Split view - Independent vs Dependent variables
# Create separate correlation plots if the matrix is too large

# Just show correlations between independent variables and ALL dependent variables
indep_dep_corr <- corr_matrix[independent_vars, dependent_vars]

corrplot(indep_dep_corr,
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.7,
         number.cex = 0.6,
         title = "Correlations: Treatments vs Soil Properties",
         mar = c(0, 0, 3, 0))
for (var in independent_vars) {
  p <- ggplot(analysis_data, aes_string(x = var, y = target_variable)) +
    geom_point(alpha = 0.6, color = "darkgreen") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    labs(title = paste(var, "vs", target_variable),
         subtitle = paste("Spearman ρ =", 
                          round(comparison_df$Spearman_rho[comparison_df$Variable == var], 3),
                          "RF Importance =", 
                          round(comparison_df$RF_Importance[comparison_df$Variable == var], 2))) +
    theme_minimal()
  print(p)
}

# =============================================================================
# STEP 6: SCIENTIFIC INTERPRETATION SUMMARY
# =============================================================================

cat("\n=== STEP 6: SCIENTIFIC INTERPRETATION SUMMARY ===\n")

# Create interpretation table
interpretation_table <- comparison_df %>%
  mutate(
    RF_Importance = round(RF_Importance, 2),
    Spearman_rho = round(Spearman_rho, 3),
    Scientific_Interpretation = case_when(
      Agreement == "High Agreement" & Spearman_rho > 0 ~ paste("Strong positive linear relationship with", target_variable),
      Agreement == "High Agreement" & Spearman_rho < 0 ~ paste("Strong negative linear relationship with", target_variable),
      Agreement == "Nonlinear Relationship" ~ paste("Complex nonlinear relationship with", target_variable, "- important but not captured by correlation"),
      TRUE ~ paste("Moderate influence on", target_variable, "with potential interactions")
    )
  ) %>%
  select(Variable, RF_Importance, Spearman_rho, Scientific_Interpretation)

print(interpretation_table)

# =============================================================================
# STEP 7: EXPORT RESULTS
# =============================================================================

cat("\n=== STEP 7: EXPORTING RESULTS ===\n")

# Create timestamp for output files
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Export results
write.csv(comparison_df, paste0("RF_Spearman_Comparison_", timestamp, ".csv"), row.names = FALSE)
write.csv(interpretation_table, paste0("Scientific_Interpretation_", timestamp, ".csv"), row.names = FALSE)
write.csv(all_cor_results, paste0("All_Correlations_", timestamp, ".csv"), row.names = FALSE)

# Save plots
ggsave(paste0("variable_importance_", timestamp, ".png"), vip_plot, width = 8, height = 6, dpi = 300)
ggsave(paste0("comparison_plot_", timestamp, ".png"), comp_plot, width = 10, height = 6, dpi = 300)

# Save correlation matrix
png(paste0("correlation_matrix_", timestamp, ".png"), width = 800, height = 800)
corrplot(corr_matrix, 
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.8,
         title = paste("Spearman Correlation Matrix\nTarget:", target_variable),
         mar = c(0, 0, 2, 0))
dev.off()

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Target variable analyzed:", target_variable, "\n")
cat("Random Forest R-squared:", round(1 - rf_model$mse[length(rf_model$mse)] / var(y), 3), "\n")
cat("Most important variable (RF):", comparison_df$Variable[which.max(comparison_df$RF_Importance)], "\n")
cat("Strongest correlation:", comparison_df$Variable[which.max(abs(comparison_df$Spearman_rho))], "\n")
cat("\nFiles saved with timestamp:", timestamp, "\n")

# Print key findings
cat("\n=== KEY SCIENTIFIC FINDINGS ===\n")
for(i in 1:nrow(interpretation_table)) {
  cat(interpretation_table$Variable[i], ":", interpretation_table$Scientific_Interpretation[i], "\n")
}

