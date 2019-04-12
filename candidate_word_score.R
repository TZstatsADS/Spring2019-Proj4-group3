candidate.word.score = function(word, probability.topic, confusion.prob, beta.matrix){
  prb.word.in.topic = get.probability.word(word, beta.matrix)
  score = sum(prb.word.in.topic*probability.topic)*confusion.prob
}