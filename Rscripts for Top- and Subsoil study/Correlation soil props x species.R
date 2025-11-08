library(standardize)
library(vegan)
library(tidyverse)
library(plotly)
library(corrplot)
library(autoplotly)
library(ggplot2)
library(ggplot)
library(corrr)
library(openxlsx)
library(WriteXLS)
library(metan)

genera <- decostand(bacteria_genus, "hellinger") # ASVs data need to be Hellinger transformed
a.env <- data

cor2 <- cor(a.env, genera, method = c("pearson"))

library(ggcorrplot)
ggcorrplot(cor2,
           hc.order = F,
           method = "square",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           legend.title = "Spearman correlation",
           lab = TRUE,
           lab_size = 3.5,
           tl.cex = 14) +
  theme(plot.title = element_text(hjust = 1, size=8), legend.title = element_text(size = 10)) +
  ggplot2::labs(x = '', y = '') +
  ggplot2::theme(
    axis.title.x = element_text(angle = 0, color = 'black'),
    axis.title.y = element_text(angle = 90, color = 'black')
  )

# extract excel file

df1 <- data.frame(cor2)
writexl::write_xlsx(df1,"M:/PC_correlation.xlsx")
