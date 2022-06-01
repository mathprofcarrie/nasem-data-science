# Finding sentences from the interim in the final report

# Create an empty dataframe to hold sentence comparrison scores
similarity_final_to_interim <- as.data.frame(matrix(nrow = 0, ncol = 5))
colnames(similarity_final_to_interim) <- c("final_sentence_number", "interim_most_similar_to", "final_sentence_text", "interim_sentence_text", "similarity_score")

# Create an empty row that can be added to the empty dataframe above
similarity_final_to_interim_row <- as.data.frame(matrix(nrow = 1, ncol = 5))
colnames(similarity_final_to_interim_row) <- c("final_sentence_number", "interim_most_similar_to", "final_sentence_text", "interim_sentence_text", "similarity_score")

# Iterate through the rows in the final sentences dataframe
for(rf in 1:nrow(final_sentences_clean)) {
  # Extract each row and convert the sentence to a vector
  final_sentence <- final_sentences_clean$sentence[rf]
  final_sentence_words <- strsplit(final_sentence, " ")[[1]]
  # Set counter variables for each row of the final sentences dataframe
  percent_identical_words_previous <- 0
  highest_match_interim_percent <- 0
  highest_match_interim_row <- 0
  highest_match_interim_text <- ""
  # Iterate through the rows in the interim sentences dataframe
  for(ri in 1:nrow(interim_sentences_clean)) {
    # Extract each row and convert the sentence to a vector
    interim_sentence <- interim_sentences_clean$sentence[ri]
    interim_sentence_words <- strsplit(interim_sentence, " ")[[1]]
    # Check the number of words shared between the final sentence and each interim sentence
    identical_words <- sum(final_sentence_words %in% interim_sentence_words)
    percent_identical_words <- identical_words / length(final_sentence_words)
    # If a given row has a greater precentage of identical words, store the percentage and row number in counter variables
    if(percent_identical_words > percent_identical_words_previous) {
      highest_match_interim_row <- ri
      highest_match_interim_percent <- percent_identical_words
      percent_identical_words_previous <- percent_identical_words
      highest_match_interim_text <- interim_sentence
    }
  }
  # Once done iterating through interim rows, store highest percentage rows similarity_final_to_interim_row
  similarity_final_to_interim_row$final_sentence_number[1] <- rf
  similarity_final_to_interim_row$final_sentence_text[1] <- final_sentence
  similarity_final_to_interim_row$interim_sentence_text[1] <- highest_match_interim_text
  similarity_final_to_interim_row$similarity_score[1] <- highest_match_interim_percent
  similarity_final_to_interim_row$interim_most_similar_to[1] <- highest_match_interim_row
  # Append row to the omnibus dataframe
  similarity_final_to_interim[rf, ] <- similarity_final_to_interim_row
}

write.csv(similarity_final_to_interim, "similarity_final_to_interim.csv", row.names = FALSE)

# Finding sentences from the final in the interim report

# Create an empty dataframe to hold sentence comparrison scores
similarity_interim_to_final <- as.data.frame(matrix(nrow = 0, ncol = 5))
colnames(similarity_interim_to_final) <- c("interim_sentence_number", "final_most_similar_to", "interim_sentence_text", "final_sentence_text", "similarity_score")

# Create an empty row that can be added to the empty dataframe above
similarity_interim_to_final_row <- as.data.frame(matrix(nrow = 1, ncol = 5))
colnames(similarity_interim_to_final_row) <- c("interim_sentence_number", "final_most_similar_to", "interim_sentence_text", "final_sentence_text", "similarity_score")

# Iterate through the rows in the interim sentences dataframe
for(ri in 1:nrow(interim_sentences_clean)) {
  # Extract each row and convert the sentence to a vector
  interim_sentence <- interim_sentences_clean$sentence[ri]
  interim_sentence_words <- strsplit(interim_sentence, " ")[[1]]
  # Set counter variables for each row of the final sentences dataframe
  percent_identical_words_previous <- 0
  highest_match_final_percent <- 0
  highest_match_final_row <- 0
  highest_match_final_text <- ""
  # Iterate through the rows in the interim sentences dataframe
  for(rf in 1:nrow(final_sentences_clean)) {
    # Extract each row and convert the sentence to a vector
    final_sentence <- final_sentences_clean$sentence[rf]
    final_sentence_words <- strsplit(final_sentence, " ")[[1]]
    # Check the number of words shared between the final sentence and each interim sentence
    identical_words <- sum(interim_sentence_words %in% final_sentence_words)
    percent_identical_words <- identical_words / length(interim_sentence_words)
    # If a given row has a greater percentage of identical words, store the percentage and row number in counter variables
    if(percent_identical_words > percent_identical_words_previous) {
      highest_match_final_row <- rf
      highest_match_final_percent <- percent_identical_words
      percent_identical_words_previous <- percent_identical_words
      highest_match_final_text <- final_sentence
    }
  }
  # Once done iterating through interim rows, store highest percentage rows similarity_final_to_interim_row
  similarity_interim_to_final_row$interim_sentence_number[1] <- ri
  similarity_interim_to_final_row$interim_sentence_text[1] <- interim_sentence
  similarity_interim_to_final_row$final_sentence_text[1] <- highest_match_final_text
  similarity_interim_to_final_row$similarity_score[1] <- highest_match_final_percent
  similarity_interim_to_final_row$final_most_similar_to[1] <- highest_match_final_row
  # Append row to the omnibus dataframe
  similarity_interim_to_final[ri, ] <- similarity_interim_to_final_row
}

write.csv(similarity_interim_to_final, "similarity_interim_to_final.csv", row.names = FALSE)

# Check to see which sentences matched interim -> final and final -> interim

similarity_interim_to_final$matches <- ""

for(r in 1:nrow(similarity_interim_to_final)) {
  interim_sent_num <- similarity_interim_to_final$interim_sentence_number[r]
  final_sent_num <- similarity_interim_to_final$final_most_similar_to[r]
  match_in_similarity_final_to_interim <- which(similarity_final_to_interim$interim_most_similar_to == interim_sent_num & similarity_final_to_interim$final_sentence_number == final_sent_num)
  if(length(match_in_similarity_final_to_interim) > 0) {
    similarity_interim_to_final$matches[r] <- TRUE
  } else {
    similarity_interim_to_final$matches[r] <- FALSE
  }
}

# Create dataframe that has all sentences where identification was shared and where more than 70% of words were shared
shared_between_reports <- similarity_interim_to_final[which(similarity_interim_to_final$matches == TRUE & similarity_interim_to_final$similarity_score >= .7),]
only_in_interim <- similarity_interim_to_final[which(similarity_interim_to_final$matches == FALSE | similarity_interim_to_final$similarity_score < .7), ]
only_in_interim <- only_in_interim[ , c("interim_sentence_number", "interim_sentence_text")]
only_in_interim <- only_in_interim[order(only_in_interim$interim_sentence_number), ]

matching_in_final <- similarity_interim_to_final$final_most_similar_to[which(similarity_interim_to_final$matches == TRUE & similarity_interim_to_final$similarity_score >= .7)]
only_in_final <- similarity_final_to_interim[which(!similarity_final_to_interim$final_sentence_number %in% matching_in_final),]
only_in_final <- only_in_final[ ,c("final_sentence_number", "final_sentence_text")]
only_in_final <- only_in_final[order(only_in_final$final_sentence_number), ]
  