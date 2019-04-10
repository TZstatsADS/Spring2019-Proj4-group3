#Paper reference: for each incorrect word w_i in the document, we generate a list of all strings 
#that differ from w_i by zero, one or two characters.

alter.one.char = function(word, letter, index){ #original word, new letter, new position
  #stopifnot(index <= nchar(word), index >= 1)
  word.vector = strsplit(word, split = "")[[1]]
  word.vector[index]  = letter
  return(paste0(word.vector, collapse = "")) #new word
}

alter.two.char = function(word, letter.1, letter.2, index.1, index.2){ #original word, new letters, new positions
  #stopifnot(index1 <= nchar(word), index1 >= 1)
  #stopifnot(index2 <= nchar(word), index2 >= 1)
  word.vector = strsplit(word, split = "")[[1]]
  word.vector[index.1]  = letter.1
  word.vector[index.2]  = letter.2
  return(paste0(word.vector, collapse = "")) #new word
}

get.candidate.vector = function(error.word, dictionary){ #an error word (from detection function), dictionary (character vector of words from ground truth)
  word.vector = c(error.word) #by zero...
  error.word = tolower(error.word)
  
  for(i in 1:nchar(error.word)){
    for(letter in letters){ #letters here is not a local variable, but a list of 26 alphabetical characters in R
      candidate = alter.one.char(error.word, letter, i)
      if(candidate %in% dictionary){
        word.vector = c(word.vector, candidate)
      }
    }
  }
  
  for(i in 1:nchar(error.word)){
    for(j in (i+1):nchar(error.word)){
      for(letter.1 in letters){
        for(letter.2 in letters){
          candidate = alter.two.char(error.word, letter.1, letter.2, i, j)
          if(candidate %in% dictionary){
            word.vector = c(word.vector, candidate)
          }
        }
      }
    }
  }
  candicate.vector = unique(word.vector)
  return(candicate.vector) #candidate words for an given error word
}
