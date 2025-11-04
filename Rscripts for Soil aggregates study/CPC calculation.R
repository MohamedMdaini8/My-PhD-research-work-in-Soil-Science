library(dplyr)

df_cpc <- df1 %>%
  mutate(
    CPC_f1 = SOC1 * frac1 / 100,
    CPC_f2 = SOC2 * frac2 / 100,
    CPC_f3 = SOC3 * frac3 / 100,
    CPC_f4 = SOC4 * frac4 / 100,
    CPC_total = CPC_f1 + CPC_f2 + CPC_f3 + CPC_f4
  )


