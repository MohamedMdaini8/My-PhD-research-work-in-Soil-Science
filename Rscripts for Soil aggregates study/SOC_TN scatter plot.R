library(tidyverse)

# reshape to long format for CN fractions

df_long_cn <- df_cn %>%
  select(Temperature, Incubation_day, Salinity, CN1, CN2, CN3, CN4) %>%
  pivot_longer(
    cols = starts_with("CN"),
    names_to = "fraction",
    values_to = "CN_ratio"
  ) %>%
  mutate(
    fraction = recode(fraction,
                      CN1 = ">2 mm",
                      CN2 = "0.25–2 mm",
                      CN3 = "0.053–0.25 mm",
                      CN4 = "<0.053 mm"),
    Salinity = factor(Salinity, levels = c("S1", "S5", "S10")),
    Temperature = factor(Temperature, levels = c("19°C", "21°C")),
    Incubation_day = as.numeric(as.character(Incubation_day)),
    fraction = factor(fraction, levels = c(">2 mm", "0.25–2 mm", "0.053–0.25 mm", "<0.053 mm"))
  )

# summarize mean and standard error, then remove duplicates

df_cn_summary <- df_long_cn %>%
  group_by(fraction, Incubation_day, Salinity, Temperature) %>%
  summarise(
    mean_CN = mean(CN_ratio, na.rm = TRUE),
    se_CN = sd(CN_ratio, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  distinct(fraction, Incubation_day, Salinity, Temperature, .keep_all = TRUE)  # Remove any duplicates

# make the plot with correct grouping and no duplicated lines

ggplot(df_cn_summary, aes(x = Incubation_day, y = mean_CN,
                          color = Salinity,
                          linetype = Temperature,
                          group = interaction(Salinity, Temperature))) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_CN - se_CN, ymax = mean_CN + se_CN), width = 2, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +   # Only smooth line
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
    y = "SOC/TN Ratio",
    color = "Salinity",
    linetype = "Temperature"
  ) +
  theme_minimal(base_size = 15)
