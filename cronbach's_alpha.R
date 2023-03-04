# for calculating Cronbach's alpha value

library(psych)

df = read.csv(file.choose() ,header = T, sep = ",", stringsAsFactors = FALSE)


alpha(rpbs)

fw = df[, c(7, 10, 13, 16, 19)]
de = df[, c(8, 11, 14, 17, 20)]
du = df[, c(9, 12, 15, 18, 21)]

rpbs = df[, c(22:47)]
