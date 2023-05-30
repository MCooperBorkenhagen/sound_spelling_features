
require(tidyverse)

words = read_csv("data/elp_3let_3phon_letphononly.csv")
phonreps = jsonlite::fromJSON("data/phonreps.json")
orthreps = jsonlite::fromJSON("data/orthreps.json")


# generate representations and write to file

# Orthography:
orth <- file("data/orth.csv", "w")

# Iterate over the elements of the vector
for (word in words$word) {
  orthrep = c()
  for (letter in str_split(word, '')[[1]]){
    orthrep = c(orthrep, unlist(orthreps[letter]))
    
  }
  writeLines(paste(orthrep, collapse = ","), orth)
}

# Close the file
close(orth)


# Phonology
# But we are going to remove the phoneme "A" and replace
# with "O" because the distinction isn't meaningful here
# we will also replace "@" with "V"


# Phonology:
phon <- file("data/phon.csv", "w")

# Iterate over the elements of the vector
for (word in words$pronunciation) {
  phonrep = c()
  for (phone in str_split(word, '')[[1]]){
    if (phone == "A"){
      phone = "O" # replace A with O
    }
    if (phone == "@"){
      phone = "V" # replace @ with V
    }
    phonrep = c(phonrep, unlist(phonreps[phone]))
 
  }
  writeLines(paste(phonrep, collapse = ","), phon)
}

# Close the file
close(phon)




