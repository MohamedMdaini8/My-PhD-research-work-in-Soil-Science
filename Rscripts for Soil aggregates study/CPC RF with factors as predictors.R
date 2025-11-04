library(randomForest)
library(tidyverse)

# ---- STEP 1: Load your data ----
df <- Full_dataset_aggregates_MM[1:26]
your_data <- df  # replace with your actual dataset name

# Set the response variable and remove response variables from predictors
response <- your_data$CPC
predictors <- your_data %>% select(-GMD, -CPC)

# ---- STEP 2: Fit Random Forest model ----
set.seed(123)
rf_model <- randomForest(x = predictors, y = response, importance = TRUE, ntree = 500)

# ---- STEP 3: Extract importance ----
imp_df <- as.data.frame(importance(rf_model, type = 1))
imp_df$Variable <- rownames(imp_df)
colnames(imp_df)[1] <- "IncMSE"

# ---- STEP 4: Assign variable groups with distinct colors ----
imp_df <- imp_df %>%
  mutate(
    Group = case_when(
      Variable %in% c("CN1", "CN2", "CN3", "CN4") ~ "SOC/TN ratio",
      Variable %in% c("frac1", "frac2", "frac3", "frac4") ~ "Aggregate fraction %",
      Variable %in% c("SOC1", "SOC2", "SOC3", "SOC4") ~ "SOC content",
      Variable %in% c("TN1", "TN2", "TN3", "TN4") ~ "TN content",
      Variable == "Salinity" ~ "Salinity",
      Variable == "Temperature" ~ "Temperature",
      Variable == "Incubation_day" ~ "Incubation time",
      Variable == "Salinity vs. Temperature" ~ "Salinity × Temperature",
      Variable %in% c("CPC 1", "CPC 2", "CPC 3", "CPC 4") ~ "CPC fractions",
      TRUE ~ "Other"
    ),
    quantile = ntile(IncMSE, 4),
    sig = case_when(
      quantile == 4 ~ "***",
      quantile == 3 ~ "**",
      quantile == 2 ~ "*",
      TRUE ~ ""
    )
  )

# ---- STEP 5: Plot with color-coded groups and significance stars ----
ggplot(imp_df, aes(x = reorder(Variable, IncMSE), y = IncMSE, fill = Group)) +
  geom_col() +
  geom_text(aes(label = sig), hjust = -0.2, size = 5, color = "black") +
  coord_flip() +
  scale_fill_manual(values = c(
    "SOC/TN ratio" = "gray",
    "Aggregate fraction %" = "#1f77b4",              # blue
    "SOC content" = "#2ca02c",                      # green
    "TN content" = "#ff7f0e",                       # orange
    "Incubation time" = "#17becf",                      # cyan
    "Salinity" = "#bcbd22",                # olive
    "Temperature" = "#8c564b",              # brown
    "Salinity × Temperature" = "#e377c2",# pink
    "CPC fractions" = "red",
    other = "black"
  )) +
  labs(
    title = "Random Forest Variable Importance for CPC",
    x = "Predictor",
    y = "%IncMSE (Importance)",
    fill = "Variable Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 12, face = "bold")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

