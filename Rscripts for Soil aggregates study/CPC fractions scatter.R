library(tidyverse)

# reshape to long format for CPC fractions
df_long_cpc <- Full_dataset %>%
  select(Temperature, Incubation_day, Salinity, cpc1, cpc2, cpc3, cpc4) %>%
  pivot_longer(
    cols = starts_with("cpc"),
    names_to = "fraction",
    values_to = "CPC"
  ) %>%
  mutate(
    fraction = recode(fraction,
                      cpc1 = ">2 mm",
                      cpc2 = "0.25–2 mm",
                      cpc3 = "0.053–0.25 mm",
                      cpc4 = "<0.053 mm"),
    Salinity = factor(Salinity, levels = c("S1", "S5", "S10")),
    Temperature = factor(Temperature, levels = c("19°C", "21°C")),
    Incubation_day = as.numeric(as.character(Incubation_day)),
    fraction = factor(fraction, 
                      levels = c(">2 mm", "0.25–2 mm", "0.053–0.25 mm", "<0.053 mm"))
  )

# summarize mean and standard error
df_cpc_summary <- df_long_cpc %>%
  group_by(fraction, Incubation_day, Salinity, Temperature) %>%
  summarise(
    mean_CPC = mean(CPC, na.rm = TRUE),
    se_CPC = sd(CPC, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  distinct(fraction, Incubation_day, Salinity, Temperature, .keep_all = TRUE)

# make the plot of CPC dynamics across fractions
ggplot(df_cpc_summary, aes(x = Incubation_day, y = mean_CPC,
                           color = Salinity,
                           linetype = Temperature,
                           group = interaction(Salinity, Temperature))) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_CPC - se_CPC, ymax = mean_CPC + se_CPC), 
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
    y = "Carbon preservation capacity (g/kg)",
    color = "Salinity",
    linetype = "Temperature"
  ) +
  theme_minimal(base_size = 15)
