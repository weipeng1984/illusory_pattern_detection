# To process the survey data

library(tidyverse)

id = data.frame(response_id = double(), prolific_id = character(), stringsAsFactors = FALSE)

df_survey = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# reading detection prolific data

df_detection = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# Change the column names of survey data

names(df_survey)[1] = 'response_id'
names(df_survey)[9] = 'prolific_id'
names(df_survey)[10] = 'education'
names(df_survey)[11] = 'gender'
names(df_survey)[12] = 'age'
names(df_survey)[13] = 'country'
names(df_survey)[14] = 'f01'
names(df_survey)[15] = 'f02'
names(df_survey)[16] = 'f03'
names(df_survey)[17] = 'f04'
names(df_survey)[18] = 'f05'
names(df_survey)[19] = 'f06'
names(df_survey)[20] = 'f07'
names(df_survey)[21] = 'f08'
names(df_survey)[22] = 'f09'
names(df_survey)[23] = 'f10'
names(df_survey)[24] = 'f11'
names(df_survey)[25] = 'f12'
names(df_survey)[26] = 'f13'
names(df_survey)[27] = 'f14'
names(df_survey)[28] = 'f15'
names(df_survey)[29] = 'p01'
names(df_survey)[30] = 'p02'
names(df_survey)[31] = 'p03'
names(df_survey)[32] = 'p04'
names(df_survey)[33] = 'p05'
names(df_survey)[34] = 'p06'
names(df_survey)[35] = 'p07'
names(df_survey)[36] = 'p08'
names(df_survey)[37] = 'p09'
names(df_survey)[38] = 'p10'
names(df_survey)[39] = 'p11'
names(df_survey)[40] = 'p12'
names(df_survey)[41] = 'p13'
names(df_survey)[42] = 'p14'
names(df_survey)[43] = 'p15'
names(df_survey)[44] = 'p16'
names(df_survey)[45] = 'p17'
names(df_survey)[46] = 'p18'
names(df_survey)[47] = 'p19'
names(df_survey)[48] = 'p20'
names(df_survey)[49] = 'p21'
names(df_survey)[50] = 'p22'
names(df_survey)[51] = 'p23'
names(df_survey)[52] = 'p24'
names(df_survey)[53] = 'p25'
names(df_survey)[54] = 'p26'

df_survey = subset(df_survey, select = c(1, 9:54))

# check duplicate item

df_survey$prolific_id[duplicated(df_survey$prolific_id)]

# calculate the free will beliefs (don't reverse the Determinism subscale)
# dimension of the data-frame

# extract Free Will Inventory

for (i in 1:dim(df_survey)[1]) {
  for (j in 1:dim(df_survey)[2]) {
    if (df_survey[i, j] == 'Strongly disagree') {
      df_survey[i, j][df_survey[i, j] == 'Strongly disagree'] = 1
    } else if (df_survey[i, j] == 'Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Disagree'] = 2
    } else if (df_survey[i, j] == 'Somewhat disagree') {
      df_survey[i, j][df_survey[i, j] == 'Somewhat disagree'] = 3
    } else if (df_survey[i, j] == 'Neither agree nor disagree') {
      df_survey[i, j][df_survey[i, j] == 'Neither agree nor disagree'] = 4
    } else if (df_survey[i, j] == 'Somewhat agree') {
      df_survey[i, j][df_survey[i, j] == 'Somewhat agree'] = 5
    } else if (df_survey[i, j] == 'Agree') {
      df_survey[i, j][df_survey[i, j] == 'Agree'] = 6
    } else if (df_survey[i, j] == 'Strongly agree') {
      df_survey[i, j][df_survey[i, j] == 'Strongly agree'] = 7
    }
  }
}

freewill = c(7, 10, 13, 16, 19)
determinism = c(8, 11, 14, 17, 20)
dualism = c(9, 12, 15, 18, 21)

# calculating the free will beliefs

df_survey$fw = rowSums(sapply(df_survey[, freewill], as.numeric))

# calculating the Determinism

df_survey$de = rowSums(sapply(df_survey[, determinism], as.numeric))

# calculating the dualism subscale

df_survey$du = rowSums(sapply(df_survey[, dualism], as.numeric))

# starting to transform the Revised Paranormal Belief Scale 

rpbs = c(22:43, 45:47)

for (i in 1:dim(df_survey)[1]) {
  for (j in rpbs) {
    if (df_survey[i, j] == 'Strongly Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Strongly Disagree'] = 1
    } else if (df_survey[i, j] == 'Moderately Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Moderately Disagree'] = 2
    } else if (df_survey[i, j] == 'Slightly Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Slightly Disagree'] = 3
    } else if (df_survey[i, j] == 'Uncertain') {
      df_survey[i, j][df_survey[i, j] == 'Uncertain'] = 4
    } else if (df_survey[i, j] == 'Slightly Agree') {
      df_survey[i, j][df_survey[i, j] == 'Slightly Agree'] = 5
    } else if (df_survey[i, j] == 'Moderately Agree') {
      df_survey[i, j][df_survey[i, j] == 'Moderately Agree'] = 6
    } else if (df_survey[i, j] == 'Strongly Agree') {
      df_survey[i, j][df_survey[i, j] == 'Strongly Agree'] = 7
    }
  }
}

# reversed item-23 in the Revised Paranormal Belief Scale

rpbs_r = 44

for (i in 1:dim(df_survey)[1]) {
  for (j in rpbs_r) {
    if (df_survey[i, j] == 'Strongly Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Strongly Disagree'] = 7
    } else if (df_survey[i, j] == 'Moderately Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Moderately Disagree'] = 6
    } else if (df_survey[i, j] == 'Slightly Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Slightly Disagree'] = 5
    } else if (df_survey[i, j] == 'Uncertain') {
      df_survey[i, j][df_survey[i, j] == 'Uncertain'] = 4
    } else if (df_survey[i, j] == 'Slightly Agree') {
      df_survey[i, j][df_survey[i, j] == 'Slightly Agree'] = 3
    } else if (df_survey[i, j] == 'Moderately Agree') {
      df_survey[i, j][df_survey[i, j] == 'Moderately Agree'] = 2
    } else if (df_survey[i, j] == 'Strongly Agree') {
      df_survey[i, j][df_survey[i, j] == 'Strongly Agree'] = 1
    }
  }
}

trb = c(22, 29, 36, 43)
psi = c(23, 30, 37, 44)
witchcraft = c(24, 31, 38, 45)
superstition = c(25, 32, 39)
spiritualism = c(26, 33, 40, 46)
elf = c(27, 34, 41)
precognition = c(28, 35, 42, 47)

# calculating the Traditional Religious Belief
df_survey$trb = rowSums(sapply(df_survey[, trb], as.numeric))

# calculating the Psi
df_survey$psi = rowSums(sapply(df_survey[, psi], as.numeric))

# calculating the Witchcraft
df_survey$witchcraft = rowSums(sapply(df_survey[, witchcraft], as.numeric))

# calculating the Superstition
df_survey$superstition = rowSums(sapply(df_survey[, superstition], as.numeric))

# calculating the Spiritualism
df_survey$spiritualism = rowSums(sapply(df_survey[, spiritualism], as.numeric))

# calculating the Extraordinary Life Forms
df_survey$elf = rowSums(sapply(df_survey[, elf], as.numeric))

# calculating the Precognition
df_survey$precognition = rowSums(sapply(df_survey[, precognition], as.numeric))

# calculating the total score

df_survey$rpbs = rowSums(sapply(df_survey[, 22:47], as.numeric))

################################################################################################

# choose participants who have complete date-sets across the experiment

df_survey = subset(df_survey, df_survey$prolific_id %in% df_detection$prolific_id)

# Now start to match the two data-sets

# starting matching

for (i in 1:dim(df_survey)[1]) {
  for (j in 1:dim(df_detection)[1]) {
    if (df_survey[i, 2] == df_detection[j, 1]) {
      
      id[i, 1] = df_survey[i, 1]
      id[i, 2] = df_detection[j, 1]
    }
  }
}

id = id[complete.cases(id), ]

# add multiple column
id = cbind(id, df_survey[, 3:58])

write.table(id, file = paste0('id_list', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")





