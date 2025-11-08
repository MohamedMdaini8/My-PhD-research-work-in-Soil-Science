library(tidyverse)

# reshape to long format for SOC fractions

df_long_soc <- Full_dataset %>%
  select(Temperature, Incubation_day, Salinity, SOC1, SOC2, SOC3, SOC4) %>%
  pivot_longer(
    cols = starts_with("SOC"),
    names_to = "fraction",
    values_to = "SOC"
  ) %>%
  mutate(
    fraction = recode(fraction,
                      SOC1 = ">2 mm",
                      SOC2 = "0.25–2 mm",
                      SOC3 = "0.053–0.25 mm",
                      SOC4 = "<0.053 mm"),
    Salinity = factor(Salinity, levels = c("S1", "S5", "S10")),
    Temperature = factor(Temperature, levels = c("19°C", "21°C")),
    Incubation_day = as.numeric(as.character(Incubation_day)),
    fraction = factor(fraction, 
                      levels = c(">2 mm", "0.25–2 mm", "0.053–0.25 mm", "<0.053 mm"))
  )

# summarize mean and standard error

df_soc_summary <- df_long_soc %>%
  group_by(fraction, Incubation_day, Salinity, Temperature) %>%
  summarise(
    mean_SOC = mean(SOC, na.rm = TRUE),
    se_SOC = sd(SOC, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  distinct(fraction, Incubation_day, Salinity, Temperature, .keep_all = TRUE)

# make the plot SOC concentrations across fractions

ggplot(df_soc_summary, aes(x = Incubation_day, y = mean_SOC,
                           color = Salinity,
                           linetype = Temperature,
                           group = interaction(Salinity, Temperature))) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_SOC - se_SOC, ymax = mean_SOC + se_SOC),
                width = 2, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  facet_wrap(~ fraction) +
  scale_color_manual(
    values = c("S1" = "#56B4E9", "S5" = "#FF7F0E", "S10" = "#A8E6A3")
  ) +
  scale_linetype_manual(
    values = c("19°C" = "dashed", "21°C" = "solid")
  ) +
  labs(
    title = "",
    x = "Incubation Day",
    y = "SOC concentration (g/kg)",   # you can adjust units if needed
    color = "Salinity",
    linetype = "Temperature"
  ) +
  theme_minimal(base_size = 15)
