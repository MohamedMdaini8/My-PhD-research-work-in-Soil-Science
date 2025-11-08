library(tidyverse)

df1 <- Full_dataset

# reshape CPC fractions into long format
df_long <- df1 %>%
  select(Salinity, Temperature, cpc1, cpc2, cpc3, cpc4) %>%
  pivot_longer(
    cols = starts_with("cpc"),   # <-- fixed here
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
    fraction = factor(fraction, 
                      levels = c(">2 mm", "0.25–2 mm", "0.053–0.25 mm", "<0.053 mm"))
  )

# make barplot with errorbars
ggplot(df_long, aes(x = Salinity, y = CPC, fill = fraction)) +
  stat_summary(fun = mean, geom = "bar", 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.9), width = 0.2) +
  facet_wrap(~ Temperature, ncol = 2) +
  labs(
    title = "",
    x = "Salinity",
    y = "CPC (g/kg)",
    fill = "Aggregate fraction"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c(
    ">2 mm" = "orange",
    "0.25–2 mm" = "#dfc27d",
    "0.053–0.25 mm" = "green",
    "<0.053 mm" = "skyblue"
  )) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )
