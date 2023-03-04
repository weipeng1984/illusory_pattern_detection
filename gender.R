
df = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

names(df)[4] = 'gender'
# To unify the gender option

# unify answer option

gender = as.character(df$gender)

for (i in gender) {
  if (i == 'female') {
    gender[gender == 'female'] = 'f'
  } else if (i == 'Female') {
    gender[gender == 'Female'] = 'f'
  } else if (i == 'F') {
    gender[gender == 'F'] = 'f'
  } else if (i == 'FEMALE') {
    gender[gender == 'FEMALE'] = 'f'
  } else if (i == 'Feminino') {
    gender[gender == 'Feminino'] = 'f'
  } else if (i == 'Femmina') {
    gender[gender == 'Femmina'] = 'f'
  } else if (i == 'Famale') {
    gender[gender == 'Famale'] = 'f'
  } else if (i == 'male') {
    gender[gender == 'male'] = 'm'
  } else if (i == 'Male') {
    gender[gender == 'Male'] = 'm'
  } else if (i == 'MALE') {
    gender[gender == 'MALE'] = 'm'
  } else if (i == 'man') {
    gender[gender == 'man'] = 'm'
  } else if (i == 'men') {
    gender[gender == 'men'] = 'm'
  } else if (i == 'women') {
    gender[gender == 'women'] = 'f'
  } else if (i == 'Male ') {
    gender[gender == 'Male '] = 'm'
  } else if (i == 'Female ') {
    gender[gender == 'Female '] = 'f'
  } else if (i == 'Fem') {
    gender[gender == 'Fem'] = 'f'
  } else if (i == 'male ') {
    gender[gender == 'male '] = 'm'
  } else if (i == 'Feminine') {
    gender[gender == 'Feminine'] = 'f'
  } else if (i == 'woman') {
    gender[gender == 'woman'] = 'f'
  } else if (i == 'femmina') {
    gender[gender == 'femmina'] = 'f'
  } else if (i == 'Masculine') {
    gender[gender == 'Masculine'] = 'm'
  } else if (i == 'feminine') {
    gender[gender == 'feminine'] = 'f'
  } else if (i == 'feminine') {
    gender[gender == 'feminine'] = 'f'
  } else if (i == 'female ') {
    gender[gender == 'female '] = 'f'
  } else if (i == 'woman') {
    gender[gender == 'woman'] = 'f'
  } else if (i == 'Uomo') {
    gender[gender == 'Uomo'] = 'm'
  } else if (i == 'famale') {
    gender[gender == 'famale'] = 'f'
  } else if (i == 'MAle') {
    gender[gender == 'MAle'] = 'm'
  } else if (i == 'female ') {
    gender[gender == 'female '] = 'f'
  } else if (i == 'Woman') {
    gender[gender == 'Woman'] = 'f'
  } else if (i == 'Feminin') {
    gender[gender == 'Feminin'] = 'f'
  } else if (i == 'fem') {
    gender[gender == 'fem'] = 'f'
  } else if (i == 'M') {
    gender[gender == 'M'] = 'm'
  } else {
    print('Please check all the levels manually!')
  }
  #return(gender)
}

df$gender = gender


write.table(df, file = "gender.csv", sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")


