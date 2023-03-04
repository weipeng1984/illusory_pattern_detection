# This is to connect all the data based on prolific ID

# reading the results of survey & point-light displays

df_survey_bio = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# reading the results of random-dots kinematogram

df_coh = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# reading the exclusion

df_excl = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# find the common prolific id in two experiments

common = intersect(df_survey_bio$prolific_id, df_coh$prolific_id)

# common remove outlier

common_new = common[! common %in% df_excl$prolific_id]
common_new = as.data.frame(common_new)
names(common_new)[1] = 'prolific_id'

write.table(common_new, file = paste0('id', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")


##########################################################################

# find valid and unique exclusion id

common = intersect(df_survey_bio$prolific_id, df_coh$prolific_id)

unique_excluion = intersect(common, df_excl$prolific_id)

unique_excluion = as.data.frame(unique_excluion)

names(unique_excluion)[1] = 'prolific_id'

write.table(unique_excluion, file = paste0('unique_excluion_id', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")

