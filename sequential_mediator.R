
# This is to perform multiple level mediation analysis
# Reference link: https://stats.stackexchange.com/questions/19599/multiple-mediation-analysis-in-r

# --> https://towardsdatascience.com/doing-and-reporting-a-serial-mediation-model-with-two-mediators-in-r-with-lavaan-739ed3155814

library('lavaan')

df = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# to calculate the false alarm rate

df$n_fa_rate = df$n_fa / (df$n_fa + df$n_cr)

# to calculate the z-score of false alarm rate
df$n_fa_rate = scale(df$n_fa_rate)[, 1]

# sequential mediation

model = '
  # mediator models
  rpbs ~ a*du
  n_fa_rate ~ b*rpbs + du
  
  # outcome model 
  dprime ~ c*n_fa_rate + rpbs + d*du
  
  # indirect effects (IDE)
  ie := a*b*c
  de := d
  
  # total effect
  total := ie+de
'
set.seed(1234)
fit = sem(model, data = df, se = "bootstrap")

summary(fit, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE)

boot.fit = parameterEstimates(fit, boot.ci.type = "bca.simple", standardize = TRUE)

boot.fit

##############################################################################
# one mediator

model = "# a path
         rpbs ~ a * du

         # b path
         n_fa_rate ~ b * rpbs

         # c prime path 
         n_fa_rate ~ cp * du

         # indirect and total effects
         ab := a * b
         total := cp + ab"

set.seed(1234)
fit = sem(model, data = df, se = "bootstrap")

summary(fit, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE)
