library(tidyverse)
# reshape the data
df_long_frac <- df1 %>%
  select(Temperature, Incubation_day, Salinity, frac1, frac2, frac3, frac4) %>%
  pivot_longer(cols = starts_with("frac"), names_to = "fraction", values_to = "percentage") %>%
  mutate(
    fraction = recode(fraction,
                      frac1 = ">2 mm",
                      frac2 = "0.25–2 mm",
                      frac3 = "0.053–0.25 mm",
                      frac4 = "<0.053 mm"),
    Salinity = factor(Salinity, levels = c("S1", "S5", "S10")),
    Incubation_day = factor(Incubation_day, levels = c("0", "40", "90")),
    Temperature = factor(Temperature, levels = c("19°C", "21°C")),
    fraction = factor(fraction, levels = c(">2 mm", "0.25–2 mm", "0.053–0.25 mm", "<0.053 mm"))
  )

# calculate mean and standard error for each group
df_summary_frac <- df_long_frac %>%
  group_by(fraction, Incubation_day, Salinity, Temperature) %>%
  summarise(
    mean_percentage = mean(percentage, na.rm = TRUE),
    se = sd(percentage, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# making the plot
ggplot(df_summary_frac, aes(x = fraction, y = mean_percentage, fill = Salinity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  geom_errorbar(
    aes(ymin = mean_percentage - se, ymax = mean_percentage + se),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ Incubation_day, ncol = 3) +
  labs(
    title = "Aggregate Size Distribution (%) across Incubation Days and Salinity Levels",
    x = "Aggregate size fraction",
    y = "Aggregate fraction (%)",
    fill = "Salinity"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2")
