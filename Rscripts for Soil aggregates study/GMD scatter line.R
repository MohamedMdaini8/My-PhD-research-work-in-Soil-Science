library(tidyverse)

# if GMD is a single column per sample and not per fraction:
# summarize mean and SE for each group

df_gmd_summary <- Full_dataset %>%
  group_by(Incubation_day, Salinity, Temperature) %>%
  summarise(
    mean_GMD = mean(GMD, na.rm = TRUE),
    se_GMD = sd(GMD, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    Salinity = factor(Salinity, levels = c("S1", "S5", "S10")),
    Temperature = factor(Temperature, levels = c("19°C", "21°C")),
    Incubation_day = as.numeric(as.character(Incubation_day))
  )

# make scatter with smooth lines plot
ggplot(df_gmd_summary, aes(x = Incubation_day, y = mean_GMD,
                           color = Salinity,
                           linetype = Temperature,
                           group = interaction(Salinity, Temperature))) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_GMD - se_GMD, ymax = mean_GMD + se_GMD), width = 2, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  scale_color_manual(
    values = c("S1" = "#56B4E9", "S5" = "#FF7F0E", "S10" = "#A8E6A3")
  ) +
  scale_linetype_manual(
    values = c("19°C" = "dashed", "21°C" = "solid")
  ) +
  labs(
    title = "",
    x = "Incubation Day",
    y = "Geometric Mean Diameter (mm)",
    color = "Salinity",
    linetype = "Temperature"
  ) +
  theme_minimal(base_size = 15)
