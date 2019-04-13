get.probability.word = function(word, beta.matrix){
  candidate.matrix = filter(beta.matrix, beta.matrix$term == word)
  candidate.matrix = arrange(candidate.matrix, topic)
  probability.word.in.topic = candidate.matrix$beta
  return(probability.word.in.topic) #P(w|t_k)vector of 12 conditional probabilities
}

candidate.word.score = function(word, probability.topic, confusion.prob, beta.matrix){
  prb.word.in.topic = get.probability.word(word, beta.matrix)
  score = sum(prb.word.in.topic*probability.topic)*confusion.prob
  return(score)
}

ocr.correct = function(candidates.list.nonempty, probability.topic, confusion.prob, beta.matrix){
  
  #error.detected = c()
  error.corrected = c()
  for (i in 1:length(candidates.list.nonempty)){
    confusion.prob = 1
    candidate.word.score = lapply(candidates.list.nonempty[[i]], candidate.word.score, probability.topic = probability.topic, confusion.prob = confusion.prob, beta.matrix = beta.matrix)
    candidate.word.score.max = candidates.list.nonempty[[i]][which.max(candidate.word.score)]
    #error.detected = c(error.detected, )
    error.corrected = c(error.corrected, candidate.word.score.max)
    #ocr.all.text.vec = unlist(str_split(paste(ocr.all.text, collapse = " "), " "))
    #corrected.text.vec = gsub(ocr.true.error.nonempty[[i]], candidate.word.score.max, ocr.all.text.vec, fixed=TRUE)
  }
  
  #corrected.text = paste(corrected.text.vec, collapse = " ")
  return(error.corrected)
}