get.probability.word = function(word, beta.matrix){
  candidate.matrix = filter(beta.matrix, beta.matrix$term == word)
  candidate.matrix = arrange(candidate.matrix, topic)
  probability.word.in.topic = candidate.matrix$beta
  return(probability.word.in.topic) #vector of 12 conditional probabilities
}