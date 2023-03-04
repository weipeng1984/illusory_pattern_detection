library(tidyverse)
library(knitr)
library(lavaan)
library(psych)
library(MBESS)

df = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# to calculate the false alarm rate

df$n_fa_rate = df$n_fa / (df$n_fa + df$n_cr)

# to calculate the z-score of false alarm rate

df$n_fa_rate = scale(df$n_fa_rate)[, 1]

mod = "# a path
         rpbs ~ a * du

         # b path
         dprime ~ b * rpbs

         # c prime path 
         dprime ~ cp * du

         # indirect and total effects
         ab := a * b
         total := cp + ab"

set.seed(1234)

fsem = sem(mod, data = df, se = "bootstrap", bootstrap = 1000)

summary(fsem, standardized = TRUE)





mediate(consume ~ room_temp + thirst, data = thirst_dat, n.iter = 10000) %>% print(short = FALSE)
