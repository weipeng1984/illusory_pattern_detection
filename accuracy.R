
# This is to do data sorting. try to select the session which we need in the results.
# Session.id is in the original file, while session_id is what we created in the id_list.

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

##########################################################################
# batch calculate the accuracy

acc_total= data.frame(session_id = double(), acc_total = double(), stringsAsFactors = FALSE)

library('dplyr')

for (i in 1:dim(df)[1]) {
  df_acc = read.csv(paste0(df$session_id[i], '.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  h = 0
  for (j in 1:dim(df_acc)[1]) {
    if (df_acc[j, 3] == 0 & df_acc[j, 5] == 'Walker') {
      h = h + 1
    } else if (df_acc[j, 3] == 1 & df_acc[j, 5] == 'No Walker') {
      h = h + 1
    }
  }
  acc_total[i, 1] = df_acc[1, 1]
  acc_total[i, 2] = scales::percent(h / dim(df_acc)[1])
}

write.table(acc_total,
            file = paste0('acc_total', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")


