library(tidyverse)

# Prepare data for one CPC fraction (example: cpc1 = >2 mm)
df_cpc1 <- Full_dataset_aggregates_MM %>%
  select(Temperature, Incubation_day, Salinity, CPC) %>%
  mutate(
    Salinity = factor(Salinity, levels = c("S1", "S5", "S10")),
    Temperature = factor(Temperature, levels = c("19°C", "21°C")),
    Incubation_day = as.numeric(as.character(Incubation_day))
  )

# Summarize mean and standard error
df_cpc1_summary <- df_cpc1 %>%
  group_by(Incubation_day, Salinity, Temperature) %>%
  summarise(
    mean_CPC = mean(CPC, na.rm = TRUE),
    se_CPC = sd(CPC, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot single CPC fraction
ggplot(df_cpc1_summary, aes(x = Incubation_day, y = mean_CPC,
                            color = Salinity,
                            linetype = Temperature,
                            group = interaction(Salinity, Temperature))) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_CPC - se_CPC, ymax = mean_CPC + se_CPC),
                width = 2, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  scale_color_manual(
    values = c("S1" = "#56B4E9", "S5" = "#FF7F0E", "S10" = "#A8E6A3")
  ) +
  scale_linetype_manual(
    values = c("19°C" = "dashed", "21°C" = "solid")
  ) +
  labs(
    title = "",   # change if using cpc2, cpc3, or cpc4
    x = "Incubation Day",
    y = "Carbon preservation capacity (g/kg)",
    color = "Salinity",
    linetype = "Temperature"
  ) +
  theme_minimal(base_size = 15)
