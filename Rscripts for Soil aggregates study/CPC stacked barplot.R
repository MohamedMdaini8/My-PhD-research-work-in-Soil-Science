library(tidyverse)

df1 <- Full_dataset_aggregates_MM

# Reshape CPC fractions into long format
df_long <- df1 %>%
  select(Salinity, Temperature, cpc1, cpc2, cpc3, cpc4) %>%
  pivot_longer(
    cols = starts_with("cpc"),
    names_to = "fraction",
    values_to = "CPC"
  ) %>%
  mutate(
    fraction = recode(fraction,
                      cpc1 = ">2",
                      cpc2 = "0.25–2",
                      cpc3 = "0.053–0.25",
                      cpc4 = "<0.053"),
    Salinity = factor(Salinity, levels = c("S1", "S5", "S10")),
    fraction = factor(fraction, 
                      levels = c(">2", "0.25–2", "0.053–0.25", "<0.053"))
  )

# Stacked barplot (means only) with 6 y-axis ticks
ggplot(df_long, aes(x = Salinity, y = CPC, fill = fraction)) +
  stat_summary(fun = mean, geom = "bar", position = "stack") +
  facet_wrap(~ Temperature, ncol = 2) +
  labs(
    title = "",
    x = "Salinity",
    y = "Carbon preservation capacity (g/kg)",
    fill = "Aggregate fraction (mm)"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c(
    ">2" = "orange",
    "0.25–2" = "#dfc27d",
    "0.053–0.25" = "green",
    "<0.053" = "skyblue"
  )) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 8)  # <-- ensures ~6 ticks
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )
