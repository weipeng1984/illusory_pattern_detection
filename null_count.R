# To sort the data and make it easier to process

library(tidyverse)
library(dplyr)

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

#################################################################################

# start counting the null button (didn't make a response)

null_count = data.frame(prolific_id = character(), null_count = double(), stringsAsFactors = FALSE)

for (i in 1:length(prolific_id)) {
  df = read.csv(paste0('RDK_', prolific_id[i], '.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  n = 0
  for (j in 1:dim(df)[1]) {
    if (df[j, 6] == 'null') {
      n = n + 1
    }
  }
  null_count[i, 1] = df[1, 1]
  null_count[i, 2] = n
}

write.table(null_count,
            file = paste0('null_count', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")



