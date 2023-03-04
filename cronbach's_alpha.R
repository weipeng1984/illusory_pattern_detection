# for calculating Cronbach's alpha value

library(psych)

df = read.csv(file.choose() ,header = T, sep = ",", stringsAsFactors = FALSE)

alpha(du)

fw = df[, c(9, 12, 15, 18, 21)]
de = df[, c(10, 13, 16, 19, 22)]
du = df[, c(11, 14, 17, 20, 23)]

rpbs = df[, c(24:49)]
