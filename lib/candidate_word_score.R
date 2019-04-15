

get.probability.word = function(word, beta.matrix){
  candidate.matrix = filter(beta.matrix, beta.matrix$term == word)
  candidate.matrix = arrange(candidate.matrix, topic)
  probability.word.in.topic = candidate.matrix$beta
  return(probability.word.in.topic) #P(w|t_k)vector of 12 conditional probabilities
}

get.confusion.score = function(ocr_word, candidate_word, confusion.prob) {
  prob = 1
  for (i in 1:nchar(ocr_word)) {
    ocr_char = substr(ocr_word, i, i)
    candidate_char = substr(candidate_word, i, i)
    prob = prob * confusion.prob[ocr_char, candidate_char]
  }
  return(prob)
}

candidate.word.score = function(ocr_word, candidate_list_nonempty, probability.topic, confusion.prob, beta.matrix){
  score = list()
  for (i in 1:length(candidate_list_nonempty)) {
    candidate_word = candidate_list_nonempty[i]
    prob.confusion.matrix = get.confusion.score(ocr_word, candidate_word, confusion.prob)
    prb.word.in.topic = get.probability.word(candidate_word, beta.matrix)
    score = c(score, sum(prb.word.in.topic*probability.topic)*prob.confusion.matrix*10^20)
  }

  return(unlist(score))
  #return(score)
}

ocr.correct = function(ocr.true.error.nonempty, candidates.list.nonempty, probability.topic, confusion.prob, beta.matrix){
  #error.detected = c()
  error.corrected = c()
  for (i in 1:length(candidates.list.nonempty)){
    candidate_score = lapply(ocr.true.error.nonempty[[i]], candidate.word.score, candidates.list.nonempty[[i]], probability.topic = probability.topic, confusion.prob = confusion.prob, beta.matrix = beta.matrix)
    #
    #print(candidates.list.nonempty[[i]])
    #print(candidate.word.score)
    candidate.word.score.max = candidates.list.nonempty[[i]][which.max(candidate_score)]
    #error.detected = c(error.detected, )
    error.corrected = c(error.corrected, candidate.word.score.max)
    #ocr.all.text.vec = unlist(str_split(paste(ocr.all.text, collapse = " "), " "))
    #corrected.text.vec = gsub(ocr.true.error.nonempty[[i]], candidate.word.score.max, ocr.all.text.vec, fixed=TRUE)
  }
  
  #corrected.text = paste(corrected.text.vec, collapse = " ")
  return(error.corrected)
}