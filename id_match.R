# this is to match the id between platforms - biological motion data and questionnaire data

library(tidyverse)

id = data.frame(session_id = double(), response_id = double(),
                 prolific_id = character(), ip = character(), stringsAsFactors = FALSE)

# reading survey data
df_survey = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)
# reading session data
df_detection = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# select the relevant column data
df_survey = subset(df_survey, select = c(1, 8:54))

# Change the column names of survey data

names(df_survey)[1] = 'response_id'
names(df_survey)[2] = 'ip'
names(df_survey)[3] = 'prolific_id'
names(df_survey)[4] = 'education'
names(df_survey)[5] = 'gender'
names(df_survey)[6] = 'age'
names(df_survey)[7] = 'country'
names(df_survey)[8] = 'f01'
names(df_survey)[9] = 'f02'
names(df_survey)[10] = 'f03'
names(df_survey)[11] = 'f04'
names(df_survey)[12] = 'f05'
names(df_survey)[13] = 'f06'
names(df_survey)[14] = 'f07'
names(df_survey)[15] = 'f08'
names(df_survey)[16] = 'f09'
names(df_survey)[17] = 'f10'
names(df_survey)[18] = 'f11'
names(df_survey)[19] = 'f12'
names(df_survey)[20] = 'f13'
names(df_survey)[21] = 'f14'
names(df_survey)[22] = 'f15'
names(df_survey)[23] = 'p01'
names(df_survey)[24] = 'p02'
names(df_survey)[25] = 'p03'
names(df_survey)[26] = 'p04'
names(df_survey)[27] = 'p05'
names(df_survey)[28] = 'p06'
names(df_survey)[29] = 'p07'
names(df_survey)[30] = 'p08'
names(df_survey)[31] = 'p09'
names(df_survey)[32] = 'p10'
names(df_survey)[33] = 'p11'
names(df_survey)[34] = 'p12'
names(df_survey)[35] = 'p13'
names(df_survey)[36] = 'p14'
names(df_survey)[37] = 'p15'
names(df_survey)[38] = 'p16'
names(df_survey)[39] = 'p17'
names(df_survey)[40] = 'p18'
names(df_survey)[41] = 'p19'
names(df_survey)[42] = 'p20'
names(df_survey)[43] = 'p21'
names(df_survey)[44] = 'p22'
names(df_survey)[45] = 'p23'
names(df_survey)[46] = 'p24'
names(df_survey)[47] = 'p25'
names(df_survey)[48] = 'p26'

# change column names of detection data

names(df_detection)[1] = 'session_id'
names(df_detection)[2] = 'prolific_id'
names(df_detection)[8] = 'ip'

df_detection = subset(df_detection, select = c(1:5, 8))

# identify duplicate ip address and prolific_id

df_survey$ip[duplicated(df_survey$ip)]
df_survey$prolific_id[duplicated(df_survey$prolific_id)]


df_detection$ip[duplicated(df_detection$ip)]
df_detection$prolific_id[duplicated(df_detection$prolific_id)]

# here after get the duplicates, we manually delete the duplicates later or now

# starting matching
for (i in 1:dim(df_survey)[1]) {
  for (j in 1:dim(df_detection)[1]) {
    if (df_survey[i, 2] == df_detection[j, 6]) {
      id[i, 1] = df_detection[j, 1]
      id[i, 2] = df_survey[i, 1]
      id[i, 3] = df_survey[i, 3]
      id[i, 4] = df_survey[i, 2]
    }
  }
}

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

freewill = c(8, 11, 14, 17, 20)
determinism = c(9, 12, 15, 18, 21)
dualism = c(10, 13, 16, 19, 22)


# calculating the free will beliefs

df_survey$fw = rowSums(sapply(df_survey[, freewill], as.numeric))

# calculating the Determinism

df_survey$de = rowSums(sapply(df_survey[, determinism], as.numeric))

# calculating the dualism subscale

df_survey$du = rowSums(sapply(df_survey[, dualism], as.numeric))


# starting to transform the Revised Paranormal Belief Scale 

rpbs = c(23:44, 46:48)

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

rpbs_r = 45

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

trb = c(23, 30, 37, 44)
psi = c(24, 31, 38, 45)
witchcraft = c(25, 32, 39, 46)
superstition = c(26, 33, 40)
spiritualism = c(27, 34, 41, 47)
elf = c(28, 35, 42)
precognition = c(29, 36, 43, 48)

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

df_survey$rpbs = rowSums(sapply(df_survey[, 23:48], as.numeric))

# add multiple column
id = cbind(id, df_survey[, 4:59])

# sorting the session_id by ascending order
id = id[order(id$session_id), ]

# recheck the duplicated 
id$ip[duplicated(id$ip)]
id$prolific_id[duplicated(id$prolific_id)]
id$prolific_id[duplicated(id$session_id)]

write.table(id, file = "id_list.csv", sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")

