library(ggplot2)
library(dplyr)

# extract and mutate CPC from the dataset
df_cpc <- df1 %>%
  mutate(
    CPC
  )

# make sure variables are factors with correct levels
df_cpc <- df_cpc %>%
  mutate(
    Incubation_day = as.numeric(as.character(Incubation_day)),
    Salinity = factor(Salinity, levels = c("S1", "S5", "S10")),
    Temperature = factor(Temperature, levels = c("19°C", "21°C"))
  )

ggplot(df_cpc, aes(x = Incubation_day, y = CPC, 
                   color = Salinity, 
                   linetype = Temperature, 
                   group = interaction(Salinity, Temperature))) +
  geom_point(aes(shape = Temperature), size = 2.2, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  scale_color_manual(
    values = c("S1" = "#56B4E9",   # light blue
               "S5" = "#FF7F0E",   # intense orange
               "S10" = "#A8E6A3")  # light green
  ) +
  scale_linetype_manual(
    values = c("19" = "dashed", "21" = "solid"),
    labels = c("19°C (dashed)", "21°C (solid)")
  ) +
  scale_shape_manual(
    values = c("19" = 17, "21" = 16),  # triangle for 19°C, circle for 21°C
    labels = c("19°C", "21°C")
  ) +
  labs(
    title = "Evolution of Carbon Preservation Capacity (CPC)",
    x = "Incubation Day",
    y = "CPC (g/kg soil)",
    color = "Salinity",
    linetype = "Temperature",
    shape = "Temperature"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 13)
  )




library(dplyr)
library(ggplot2)

ggplot(df_cpc_summary, aes(x = Incubation_day, y = CPC)) +
  geom_errorbar(aes(ymin = CPC - se_CPC, ymax = CPC + se_CPC, 
                    color = Salinity, group = interaction(Salinity, Temperature)), 
                width = 2, size = 1) +
  geom_point(aes(color = Salinity, group = interaction(Salinity, Temperature)), size = 2.5) +
  geom_smooth(aes(color = Salinity, linetype = Temperature, 
                  group = interaction(Salinity, Temperature)),
              method = "loess", se = FALSE, size = 1.2) +
  scale_color_manual(
    values = c("S1" = "#56B4E9", "S5" = "#FF7F0E", "S10" = "#A8E6A3")
  ) +
  scale_linetype_manual(
    values = c("19°C" = "dashed", "21°C" = "solid"),
    labels = c("19°C (dashed)", "21°C (solid)")
  ) +
  labs(
    title = "",
    x = "Incubation Day",
    y = "CPC (g/kg soil)",
    color = "Salinity",
    linetype = "Temperature"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 13)
  )
