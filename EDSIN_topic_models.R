# Drop words of less than frequency 5
minimumFrequency <- 5
DTM <- DocumentTermMatrix(EDSIN_sentence_processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))

# Drop empty rows
sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
EDSIN_sentences_df <- EDSIN_sentences_df[sel_idx, ]

iterated_topics <- as.data.frame(matrix(nrow = 10, ncol = 0))
# Run topics iteratively 100 times
for(i in 1:100) {
  # number of topics
  K <- 30
  # set random number generator seed
  set.seed(sample(1:10000, 1))
  # compute the LDA model, inference via 1000 iterations of Gibbs sampling
  topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))
  # produce a table of topics 
  topics <- as.data.frame(terms(topicModel, 10))
  iterated_topics <- rbind(topics, iterated_topics)
}

# set EDI seed words
edi_words <- c("diverse", "diversity", "underrepresented", "underrepresentation", "access", "accessibility", "inclusive", "inclusion", "equity", "racial")

# reduce dataframe to include just topics including EDI seed words
edi_topic_vec <- c()
for(c in 1:ncol(iterated_topics)) {
  col <- as.character(iterated_topics[,c])
  if(sum(edi_words %in% col) > 0) {
    edi_topic_vec <- append(col, edi_topic_vec)
  }
}

edi_topic_vec <- unique(edi_topic_vec)
