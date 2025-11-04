library(randomForest)
library(tidyverse)

# ---- STEP 1: Load your data ----
df <- Full_dataset_aggregates_MM[1:26]

# ---- FUNCTION to run RF and return importance data ----
rf_importance_df <- function(response, response_name, predictors) {
  set.seed(123)
  rf_model <- randomForest(
    x = predictors,
    y = response,
    importance = TRUE,
    ntree = 500
  )
  
  imp_df <- as.data.frame(importance(rf_model, type = 1))
  imp_df$Variable <- rownames(imp_df)
  colnames(imp_df)[1] <- "IncMSE"
  
  imp_df <- imp_df %>%
    mutate(
      Group = case_when(
        Variable %in% c("CN1","CN2","CN3","CN4") ~ "SOC/TN ratio",
        Variable %in% c("frac1","frac2","frac3","frac4") ~ "Aggregate fraction %",
        Variable %in% c("SOC1","SOC2","SOC3","SOC4") ~ "SOC content",
        Variable %in% c("TN1","TN2","TN3","TN4") ~ "TN content",
        Variable == "Salinity" ~ "Salinity",
        Variable == "Temperature" ~ "Temperature",
        Variable == "Incubation_day" ~ "Incubation time",
        Variable == "Salinity vs. Temperature" ~ "Salinity × Temperature",
        Variable %in% c("CPC 1","CPC 2","CPC 3","CPC 4") ~ "CPC fractions",
        TRUE ~ "Other"
      ),
      quantile = ntile(IncMSE, 4),
      sig = case_when(
        quantile == 4 ~ "***",
        quantile == 3 ~ "**",
        quantile == 2 ~ "*",
        TRUE ~ ""
      ),
      Response = response_name
    )
  
  return(imp_df)
}

# ---- STEP 2: Run RF for MWD, GMD, CPC ----
predictors <- df %>% select(-MWD, -GMD, -CPC)

imp_MWD <- rf_importance_df(df$MWD, "MWD", predictors)
imp_GMD <- rf_importance_df(df$GMD, "GMD", predictors)
imp_CPC <- rf_importance_df(df$CPC, "CPC", predictors)

# Combine all
all_imp <- bind_rows(imp_MWD, imp_GMD, imp_CPC)

# ---- STEP 3: Plot facetted importance plots ----
ggplot(all_imp, aes(x = reorder(Variable, IncMSE), y = IncMSE, fill = Group)) +
  geom_col() +
  geom_text(aes(label = sig), hjust = -0.2, size = 4, color = "black") +
  coord_flip() +
  facet_wrap(~Response, scales = "free_y") +
  scale_fill_manual(values = c(
    "SOC/TN ratio" = "gray",
    "Aggregate fraction %" = "#1f77b4",
    "SOC content" = "#2ca02c",
    "TN content" = "#ff7f0e",
    "Incubation time" = "#17becf",
    "Salinity" = "#bcbd22",
    "Temperature" = "#8c564b",
    "Salinity × Temperature" = "#e377c2",
    "CPC fractions" = "red",
    "Other" = "black"
  )) +
  labs(
    title = "Random Forest Variable Importance",
    x = "Predictor",
    y = "%IncMSE (Importance)",
    fill = "Variable Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
