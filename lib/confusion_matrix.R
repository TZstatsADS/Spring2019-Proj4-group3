letterlist_add_on <- function(letterlist, filelist) {
  for (file in filelist) {
    lines = readLines(file, warn=FALSE, encoding = "UTF-8")
    add = unique(unlist(strsplit(lines,"")))
  }
  
  add = add[!(add%in%letterlist)]
  add_int = charToInt(add)
  if (NA%in%add_int) {
    ind = which(is.na(add_int))
    add = add[-ind]
  }

  return (add)
}

confusion_count_num <- function(truth_list, ocr_list) {
  lowerletters = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
  upperletters = toupper(lowerletters)
  numbers = c("0","1","2","3","4","5","6","7","8","9")
  letters = c(lowerletters, upperletters, numbers)
  
  punctuation = letterlist_add_on(letters, truth_list)
  letterlist = c(letters, punctuation)
  
  d = length(letterlist)
  mat = matrix(0, nrow=d, ncol=d)
  
  # iterate through each file
  for (index in 1:length(truth_list)) {
    # read files
    truth_txt = readLines(truth_list[index], warn=FALSE, encoding="UTF-8")
    ocr_txt = readLines(ocr_list[index], warn=FALSE, encoding="UTF-8")
    
    # split files by lines
    truth_lines = unlist(strsplit(truth_txt, "\n"))
    ocr_lines = unlist(strsplit(ocr_txt, "\n"))
    # if number of lines in files are different, skip this file
    if (length(truth_lines)!=length(ocr_lines)) {
      #print(index)
      next;
    }
    
    # iterate by each line
    for (i in 1:length(truth_lines)) {
      # split lines by words
      truth_words = unlist(strsplit(truth_lines[i], " "))
      ocr_words = unlist(strsplit(ocr_lines[i], " "))
      # if number of words in lines are different, skip this line
      if (length(truth_words)!=length(ocr_words)) {
        next;
      } 
      
      # iterate by each word
      for (j in 1:length(truth_words)) {
        # if number of characters in words are different, skip this word
        if (nchar(truth_words[j])!=nchar(ocr_words[j])) {
          next;
        }

        # iterate by each character
        for (k in 1:nchar(truth_words[j])) {
          truth_ind = which(letterlist==substr(truth_words[j], k, k))
          ocr_ind = which(letterlist==substr(ocr_words[j], k, k))
          mat[ocr_ind, truth_ind] = mat[ocr_ind, truth_ind] + 1
        }
      }
    }
  }

  #removed_vec = rowSums(mat)==0&colSums(mat)==0
  #letterlist = letterlist[!removed_vec]
  #mat = mat[!removed_vec,!removed_vec]
  
  rownames(mat) <- letterlist
  colnames(mat) <- letterlist
  print(mat)
  
  #mat = mat/colSums(mat)
  #mat[is.na(mat)] = 0
  smooth_mat = mat
  for (i in 1:d) {
    for (j in 1:d) {
      if (mat[i,j] == 0) {
        smooth_mat[i,j] = 0.1
      }
    }
  }
  prob_mat = smooth_mat/colSums(smooth_mat)
  #prob_mat = smooth_mat
  #for (i in 1:d) {
  #  for (j in 1:d) {
  #    prob_mat[i,j] = smooth_mat[i,j]/colSums(smooth_mat)[j]
  #  }
  #}
  
  #new_list <- list("mat" = prob_mat, "letterlist" = letterlist)
  #return(new_list)
  return(prob_mat)
}
