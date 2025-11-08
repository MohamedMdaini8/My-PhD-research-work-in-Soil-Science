library(tidyverse)

# calculate C/N ratio for each fraction

df_cn <- Full_dataset %>%
  mutate(
    CN1 = SOC1 / TN1,
    CN2 = SOC2 / TN2,
    CN3 = SOC3 / TN3,
    CN4 = SOC4 / TN4
  )

# reshape to long format for plotting

df_long_cn <- df_cn %>%
  select(Temperature, Incubation_day, Salinity, CN1, CN2, CN3, CN4) %>%
  pivot_longer(cols = starts_with("CN"), names_to = "fraction", values_to = "CN_ratio") %>%
  mutate(
    fraction = recode(fraction,
                      CN1 = ">2 mm",
                      CN2 = "0.25–2 mm",
                      CN3 = "0.053–0.25 mm",
                      CN4 = "<0.053 mm"),
    Salinity = factor(Salinity, levels = c("S1", "S5", "S10")),
    Incubation_day = factor(Incubation_day, levels = c("0", "40", "90")),
    Temperature = factor(Temperature, levels = c("19°C", "21°C")),
    fraction = factor(fraction, levels = c(">2 mm", "0.25–2 mm", "0.053–0.25 mm", "<0.053 mm"))
  )

# summarize mean and SE for each group

df_summary_cn <- df_long_cn %>%
  group_by(fraction, Incubation_day, Salinity, Temperature) %>%
  summarise(
    mean_CN = mean(CN_ratio, na.rm = TRUE),
    se = sd(CN_ratio, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# make the plot
ggplot(df_summary_cn, aes(x = fraction, y = mean_CN, fill = Salinity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  geom_errorbar(
    aes(ymin = mean_CN - se, ymax = mean_CN + se),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_grid(Temperature ~ Incubation_day) +
  labs(
    title = "C/N Ratio in Aggregate Fractions",
    x = "Aggregate size fraction",
    y = "C/N Ratio",
    fill = "Salinity"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_manual(values = c("S1" = "#56B4E9", "S5" = "#FF7F0E", "S10" = "#A8E6A3"))
library(writexl)

# extract to excel file

write_xlsx(df_cn, "Aggregates data5.xlsx")
