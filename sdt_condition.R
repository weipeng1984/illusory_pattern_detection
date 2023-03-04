# To sort the data and make it easier to process

library(tidyverse)
library(dplyr)
library(psycho)

# batch reading the data

filenames = list.files(path = getwd(), pattern = 'RDK_+.*csv')

# creating prolific id

prolific_id = substr(filenames, 5, 28)

# batch delete the first 3 trials (rows) & shift the answer position

for (i in 1:length(prolific_id)) {
  df = read.csv(paste0('RDK_', prolific_id[i], '.csv'), header = TRUE, sep = ',', stringsAsFactors = FALSE)
  
  df = df[-c(1:4), ]
  
  for (j in 1:dim(df)[1]) {
    
    if(df[j, 3] == 'rdk' && df[j + 1, 3] == 'html-button-response') {
      df[j, 48] = df[j + 1, 48]
    }
  }
  write.table(df, file = paste0('RDK_', prolific_id[i], '.csv'), sep = ",", col.names = TRUE,
              row.names = FALSE, qmethod = "double")
}

# delete the irrelevant columns and rows

for (i in 1:length(prolific_id)) {
  df = read.csv(paste0('RDK_', prolific_id[i], '.csv'), header = TRUE, sep = ',', stringsAsFactors = FALSE)
  for (j in 1:dim(df)[1]) {
    df = df[df$trial_type != 'html-button-response', ]
  }
  
  df = df[, c(3, 4, 17, 19, 20, 48)]
  
  write.table(df, file = paste0('RDK_', prolific_id[i], '.csv'), sep = ",", col.names = TRUE,
              row.names = FALSE, qmethod = "double")
}

# Writing the prolific id into the file

for (i in 1:length(prolific_id)) {
  df = read.csv(paste0('RDK_', prolific_id[i], '.csv'), header = TRUE, sep = ',', stringsAsFactors = FALSE)
  df[1, 1] = prolific_id[i]
  write.table(df, file = paste0('RDK_', prolific_id[i], '.csv'), sep = ",", col.names = TRUE,
              row.names = FALSE, qmethod = "double")
}

##########################################################################################

# calculating the indicators of Signal Detection Theory (SDT) separately base on different levels of number of dots


sdt = data.frame(prolific_id = character(),
                 n_hit = double(), n_fa = double(), n_miss = double(), n_cr = double(),
                 acc_229 = double(), sum = double(), stringsAsFactors = FALSE)

for (i in 1:length(prolific_id)) {
  
  df = read.csv(paste0('RDK_', prolific_id[i], '.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  # counter the number of hit 
  h = 0
  for (j in 1:dim(df)[1]) {
    if ((df[j, 5] != '0' && df[j, 6] == '0') && df[j, 3] == 229) {
      h = h + 1
    }
  }
  # counter the number of false alarm
  f = 0
  for (j in 1:dim(df)[1]) {
    if ((df[j, 5] == '0' && df[j, 6] == '0') && df[j, 3] == 229) {
      f = f + 1
    }
  }
  # counter the number of miss
  m = 0
  for (j in 1:dim(df)[1]) {
    if ((df[j, 5] != '0' && df[j, 6] == '1') && df[j, 3] == 229) {
      m = m + 1
    }
  }
  # counter the number of correct rejection
  c = 0
  for (j in 1:dim(df)[1]) {
    if ((df[j, 5] == '0' && df[j, 6] == '1') && df[j, 3] == 229) {
      c = c + 1
    }
  }
  
  sum = h + f + m + c
  sdt[i, 1] = prolific_id[i]
  sdt[i, 2] = h
  sdt[i, 3] = f
  sdt[i, 4] = m
  sdt[i, 5] = c
  sdt[i, 6] = scales::percent((h + c) / sum)
  sdt[i, 7] = sum
}

write.table(sdt,
            file = paste0('sdt_229', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")

#######################################################################

# calculate different index

df = data.frame(prolific_id = c(sdt$prolific_id),
                 n_hit = c(sdt$n_hit),
                 n_fa = c(sdt$n_fa), 
                 n_miss = c(sdt$n_miss),
                 n_cr = c(sdt$n_cr),
                 acc_229 = c(sdt$acc_229),
                 sum = c(sdt$sum))
indices = psycho::dprime(df$n_hit, df$n_fa, df$n_miss, df$n_cr)
df = cbind(df, indices)

write.table(df, file = "sdt_229_stats.csv", sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")


