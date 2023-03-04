# selection based on exclusion result

# read stats data

df = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# read exclusion results (exclusion_list)

df_excl = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

attach(df)
attach(df_excl)


df = subset(df, df$prolific_id %in% df_excl$prolific_id)
#df = subset(df, !(df$prolific_id %in% df_excl$prolific_id))

write.table(df, file = paste0('sdt_407_stats_exclusion', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")
