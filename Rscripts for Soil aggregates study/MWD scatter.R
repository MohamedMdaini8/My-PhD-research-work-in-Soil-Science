library(tidyverse)

# Prepare data for one CPC fraction (example: cpc1 = >2 mm)
df_mwd <- Full_dataset_aggregates_MM %>%
  select(Temperature, Incubation_day, Salinity, MWD) %>%
  mutate(
    Salinity = factor(Salinity, levels = c("S1", "S5", "S10")),
    Temperature = factor(Temperature, levels = c("19°C", "21°C")),
    Incubation_day = as.numeric(as.character(Incubation_day))
  )

# Summarize mean and standard error
df_mwd_summary <- df_mwd %>%
  group_by(Incubation_day, Salinity, Temperature) %>%
  summarise(
    mean_MWD = mean(MWD, na.rm = TRUE),
    se_MWD = sd(MWD, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot single CPC fraction
ggplot(df_mwd_summary, aes(x = Incubation_day, y = mean_MWD,
                            color = Salinity,
                            linetype = Temperature,
                            group = interaction(Salinity, Temperature))) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_MWD - se_MWD, ymax = mean_MWD + se_MWD),
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
    y = "Mean weight diameter (mm)",
    color = "Salinity",
    linetype = "Temperature"
  ) +
  theme_minimal(base_size = 15)
