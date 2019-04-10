get.dtm <- function(text.data, remove.unique = F){
  corpus = VCorpus(VectorSource(text.data))
  dtm = DocumentTermMatrix(corpus, control = list(
    tolower = TRUE,
    removeNumbers = FALSE,#we aren't concerned with informativeness, so few clearning options
    stopwords = FALSE,
    removeNumbers = FALSE, 
    removePunctuation = FALSE,
    stripWhitespace = TRUE))
  if (remove.unique == T){
    non_zero_entries = unique(DTM$i) #omits zero entries
    dtm = dtm[non_zero_entries,]
  }
  return(dtm)
}
