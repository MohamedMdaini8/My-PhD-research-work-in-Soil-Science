library(rda)
library(correlation)
library(corrplot)
library(plotly)
library(vegan)
library(standardize)
library(ggplot2)
library(tidyverse)
library(dplyr)


a.env <- PC
a.comm <- Bacteria_genera
pp <- disttransform(a.comm, method = 'hellinger')
a.rda <- rda(pp~., data = a.env)
plot(a.rda)
ordiplot(a.rda, scaling = 2, type = "text")
################
anova.cca(a.rda, permutations = 999)
anova.cca(a.rda, permutations = 999, by = "terms")
