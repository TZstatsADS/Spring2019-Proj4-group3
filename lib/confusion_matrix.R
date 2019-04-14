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
  
  for (index in 1:length(truth_list)) {
    truth_txt = readLines(truth_list[index], warn=FALSE, encoding="UTF-8")
    ocr_txt = readLines(ocr_list[index], warn=FALSE, encoding="UTF-8")
    
    truth_words = unlist(strsplit(truth_txt, " "))
    ocr_words = unlist(strsplit(ocr_txt, " "))
    
    if (length(truth_words)!=length(ocr_words)) {
      print(index)
    }
    ## deal with mismatch
    
    # j = 1
    for (i in 1:length(truth_words)) {
      if ((nchar(truth_words[i])!=nchar(ocr_words[i]))) {
        #print("1")
        next;
      } 
      #print(truth_words[i])
      #print(ocr_words[i])
      #truth_char = strsplit(truth_words[i], "")
      #ocr_char = strsplit(ocr_words[i], "")
      for (k in 1:nchar(truth_words[i])) {
        #truth_char = substr(truth_words[i], k, k)
        #ocr_char = substr(ocr_words[i], k, k)
        #print(truth_char)
        #print(ocr_char)
        truth_ind = which(letterlist==substr(truth_words[i], k, k))
        ocr_ind = which(letterlist==substr(ocr_words[i], k, k))
        mat[ocr_ind, truth_ind] = mat[ocr_ind, truth_ind] + 1
      }
    }
  }
  #print(mat)

  #removed_vec = rowSums(mat)==0&colSums(mat)==0
  #letterlist = letterlist[!removed_vec]
  #mat = mat[!removed_vec,!removed_vec]
  
  rownames(mat) <- letterlist
  colnames(mat) <- letterlist
  
  #mat = mat/colSums(mat)
  #mat[is.na(mat)] = 0
  smooth_mat = mat
  for (i in 1:d) {
    for (j in 1:d) {
      if (mat[i,j] == 0) {
        smooth_mat[i,j] = 0.5
      }
    }
  }
  prob_mat = smooth_mat
  for (i in 1:d) {
    for (j in 1:d) {
      prob_mat[i,j] = smooth_mat[i,j]/colSums(smooth_mat)[j]
    }
  }
  
  new_list <- list("mat" = prob_mat, "letterlist" = letterlist)
  return(new_list)
}
