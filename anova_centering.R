# ANCOVA

library(tidyverse)
library(afex)

# loading the data

df = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# to calculate the false alarm rate

df$n_fa_rate = df$n_fa / (df$n_fa + df$n_cr)

# to calculate the z-score of false alarm rate
df$n_fa_rate = scale(df$n_fa_rate)[, 1]

# centering the fw, de and du

df$fw = as.numeric(scale(df$fw, scale = F))
df$de = as.numeric(scale(df$de, scale = F))
df$du = as.numeric(scale(df$du, scale = F))

attach(df)

# perform ANOVA using afex
aov.out = aov_ez(id = "id",
                 dv = "c", covariate = c('fw', 'de', 'du'),
                 within = c("mask"), factorize = FALSE, 
                 data = df, anova_table = list(es = "pes", correction = "none"))

aov.out$Anova
summary(aov.out)
knitr::kable(nice(aov.out))

