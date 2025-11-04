df1 <- Aggregates_Dataset_3
df1 <- df1 %>%
  mutate(
    # Calculate the carbon contribution from each fraction (SOC × %mass)
    C1 = SOC1 * frac1,
    C2 = SOC2 * frac2,
    C3 = SOC3 * frac3,
    C4 = SOC4 * frac4,
    
    # Total carbon contribution (unnormalized)
    C_total = C1 + C2 + C3 + C4,
    
    # Fraction of carbon in each class
    w1 = C1 / C_total,
    w2 = C2 / C_total,
    w3 = C3 / C_total,
    w4 = C4 / C_total,
    
    # Estimate CPC per fraction
    CPC_f1 = CPC * w1,
    CPC_f2 = CPC * w2,
    CPC_f3 = CPC * w3,
    CPC_f4 = CPC * w4
  )

# Save the full dataframe with CPC per fraction
write_xlsx(df1, "CPC_fractions_output.xlsx")


