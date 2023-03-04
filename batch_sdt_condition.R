
# reading detection results

df_detection_res = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# reading id match results to get the unique and valid session id

df = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

attach(df_detection_res)

df_detection_res = subset(df_detection_res, df_detection_res$Session.id 
                          %in% df$session_id)

for (i in 1:dim(df)[1]) {
  subset(df_detection_res, df_detection_res$Session.id == df$session_id[i])
  write.table(subset(df_detection_res, df_detection_res$Session.id
                     == df$session_id[i])[, c(1, 3, 12, 19, 31)],
              file = paste0(df$session_id[i], '.csv'), sep = ",", col.names = TRUE,
              row.names = FALSE, qmethod = "double")
}

# calculate the number of trials

library(dplyr)

less_trial = data.frame(session_id = double(), 
                        trial_number = double(), stringsAsFactors = FALSE)
for (i in 1:dim(df)[1]) {
  df_detection = read.csv(paste0(df$session_id[i], ".csv"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  less_trial[i, 1] = df_detection[1, 1]
  less_trial[i, 2] = dim(df_detection)[1]
}

less_trial = na.omit(less_trial)

write.table(less_trial, file = "less_trial.csv", sep = ",", 
            col.names = TRUE, row.names = FALSE, qmethod = "double")

#############################################################################

# This is to calculate index of SDT
# Create data frame

sdt = data.frame(session_id = double(),
                 n_hit = double(), n_fa = double(), n_miss = double(), n_cr = double(),
                acc_61 = double(), sum = double(), stringsAsFactors = FALSE)

for (i in 1:dim(df)[1]) {
  
  df_index = read.csv(paste0(df$session_id[i], '.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)

  # counter the number of hit 
  h = 0
  for (j in 1:dim(df_index)[1]) {
    if ((df_index[j, 3] == 0 & df_index[j, 5] == 'Walker') & df_index[j, 4] == 61) {
      h = h + 1
    }
  }
  # counter the number of false alarm
  f = 0
  for (j in 1:dim(df_index)[1]) {
    if ((df_index[j, 3] == 1 & df_index[j, 5] == 'Walker') & df_index[j, 4] == 61) {
      f = f + 1
    }
  }
  # counter the number of miss
  m = 0
  for (j in 1:dim(df_index)[1]) {
    if ((df_index[j, 3] == 0 & df_index[j, 5] == 'No Walker') & df_index[j, 4] == 61) {
      m = m + 1
    }
  }
  # counter the number of correct rejection
  c = 0
  for (j in 1:dim(df_index)[1]) {
    if ((df_index[j, 3] == 1 & df_index[j, 5] == 'No Walker') & df_index[j, 4] == 61) {
      c = c + 1
    }
  }
  sum = h + f + m + c
  
  sdt[i, 1] = df_index[1, 1]
  sdt[i, 2] = h
  sdt[i, 3] = f
  sdt[i, 4] = m
  sdt[i, 5] = c
  sdt[i, 6] = scales::percent((h + c) / sum)
  sdt[i, 7] = sum
}

#sdt = na.omit(sdt)
write.table(sdt, file = paste0('sdt_', 'condition_61', '.csv'), sep = ",", 
            col.names = TRUE, row.names = FALSE, qmethod = "double")

#################################################################################

library(psycho)

# calculate different index

df_sdt_stats = data.frame(session_id = c(sdt$session_id),
                 n_hit = c(sdt$n_hit),
                 n_fa = c(sdt$n_fa), 
                 n_miss = c(sdt$n_miss),
                 n_cr = c(sdt$n_cr),
                acc_61 = c(sdt$acc_61),
                 sum = c(sdt$sum))
indices = psycho::dprime(df_sdt_stats$n_hit, df_sdt_stats$n_fa, df_sdt_stats$n_miss, df_sdt_stats$n_cr)

df_sdt_stats = cbind(df_sdt_stats, indices)
df_sdt_stats = cbind(df, df_sdt_stats[2:12])

write.table(df_sdt_stats, file = "sdt_stats_condition_61.csv", sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")



