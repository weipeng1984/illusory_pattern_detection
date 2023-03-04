# ANCOVA
library(tidyverse)
library(ggplot2)
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
                  dv = "dprime", covariate = c('fw','de', 'du'),
                  within = c("mask", "exp"), factorize = FALSE, 
                  data = df, anova_table = list(es = "pes", correction = "none"))

aov.out$Anova
summary(aov.out)
knitr::kable(nice(aov.out))

#######################################################################

library(ggplot2)

# Change color by groups
p = ggplot(df, aes(x = exp, y = n_fa_rate, fill = exp)) + 
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = 0.1, fill = "white")+
  labs(title = "Plot of False alarm rate", x = "Task", y = "False alarm rate")

p + scale_fill_brewer(palette="Dark2") + theme_minimal()

# Plotting the interaction between two variables 


interaction.plot(x.factor = df$du, #x-axis variable
                 trace.factor = df$exp, #variable for lines
                 response = df$c, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Response bias",
                 xlab = "Belief in dualism",
                 col = c("orange", "green", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Task")

interaction.plot(x.factor = df$mask, #x-axis variable
                 trace.factor = df$exp, #variable for lines
                 response = df$n_fa_rate, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "False alarm rate",
                 xlab = "Mask",
                 col = c("orange", "green", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Task")

######################################################################

interaction.plot(x.factor = df$de, #x-axis variable
                 trace.factor = df$exp, #variable for lines
                 response = df$dprime, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Sensitivity",
                 xlab = "Determinism",
                 col = c("orange", "green", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Task")





