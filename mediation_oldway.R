
library('mediation')
library('latticeExtra')
library('htmlTable')


df = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# to calculate the false alarm rate

df$n_fa_rate = df$n_fa / (df$n_fa + df$n_cr)

# to calculate the z-score of false alarm rate

df$n_fa_rate = scale(df$n_fa_rate)[, 1]

# step 1

model.1 = lm(n_fa_rate ~ du, data = df)

# step 2

model.2 = lm(rpbs ~ du, data = df)

# step 3
model.3 = lm(n_fa_rate ~ du + rpbs, data = df)

# step 4

set.seed(1234)

results = mediate(model.2, model.3, treat = 'du', mediator = 'rpbs',
                   boot = TRUE, sims = 1000)

summary(results)


model.1$coefficients[2] - model.2$coefficients[2] * model.3$coefficients[3]
