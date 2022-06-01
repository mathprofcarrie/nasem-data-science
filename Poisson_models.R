# Create a vector of all words in the interim report
interim_word_vec <- c()

for(r in 1:nrow(only_in_interim)) {
  sentence <- only_in_interim$interim_sentence_text[r]
  sentence_words <- strsplit(sentence, " ")[[1]]
  interim_word_vec <- append(interim_word_vec, sentence_words, after = length(interim_word_vec))
}

# Create a dataframe that contains a vector of 100 words from the interim report in each row
interim_word_vec_chunks <- split(interim_word_vec, ceiling(seq_along(interim_word_vec)/100))

interim_100_words <- as.data.frame(matrix(nrow = length(interim_word_vec_chunks), ncol = 5))
colnames(interim_100_words) <- c("index", "word_vec", "SJEDI_count", "SJEDI_rate", "report")

interim_100_words$word_vec <- interim_word_vec_chunks
interim_100_words$index <- 1:length(interim_word_vec_chunks)

for(r in 1:nrow(interim_100_words)) {
  word_vec <- as.vector(interim_100_words$word_vec[[r]])
  SJEDI_count <- sum(word_vec %in% JEDI_words)
  interim_100_words$SJEDI_count[r] <- SJEDI_count
  interim_100_words$SJEDI_rate[r] <- SJEDI_count/length(word_vec)
  interim_100_words$report <- 0
}

# Create a vector of all words in the final report
final_word_vec <- c()

for(r in 1:nrow(only_in_final)) {
  sentence <- only_in_final$final_sentence_text[r]
  sentence_words <- strsplit(sentence, " ")[[1]]
  final_word_vec <- append(final_word_vec, sentence_words, after = length(final_word_vec))
}

# Create a dataframe that contains a vector of 100 words from the interim report in each row
final_word_vec_chunks <- split(final_word_vec, ceiling(seq_along(final_word_vec)/100))

final_100_words <- as.data.frame(matrix(nrow = length(final_word_vec_chunks), ncol = 5))
colnames(final_100_words) <- c("index", "word_vec", "SJEDI_count", "SJEDI_rate", "report")

final_100_words$word_vec <- final_word_vec_chunks
final_100_words$index <- 1:length(final_word_vec_chunks)

for(r in 1:nrow(final_100_words)) {
  word_vec <- as.vector(final_100_words$word_vec[[r]])
  SJEDI_count <- sum(word_vec %in% JEDI_words)
  final_100_words$SJEDI_count[r] <- SJEDI_count
  final_100_words$SJEDI_rate[r] <- SJEDI_count/length(word_vec)
  final_100_words$report <- 1
}

# Combine both word vec dataframes
both_100_words <- rbind(interim_100_words, final_100_words)

# Model
mdl <- brm(SJEDI_count ~ report, data = both_100_words, family = poisson)

h <- 'exp(Intercept + report * 1) = exp(Intercept + report * 0)'
hypothesis(mdl, h)
