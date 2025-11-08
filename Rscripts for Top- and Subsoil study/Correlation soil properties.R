library(vegan)
library(tidyverse)
library(plotly)
library(corrplot)
library(ggcorrplot)
library(ggplot2)
library(corrr)
library(openxlsx)
library(WriteXLS)

a.env <- inc_data

cor1 <- cor(a.env, a.env, method = c("pearson"))

library(ggcorrplot)
ggcorrplot(cor1,
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

df1 <- data.frame(cor1)
writexl::write_xlsx(df1,"M:/PC_correlation_incubation.xlsx")

