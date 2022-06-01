# Strips headers specific to NASDS reports as well as chapter titles
strip_headers <- function(df) {
  for(i in 1:length(df)) {
    df[i] <- sub("Data Science for Undergraduates: Opportunities and Options", "", df[i])
    df[i] <- sub("DATA SCIENCE FOR UNDERGRADUATES", "", df[i])
    df[i] <- sub("Copyright National Academy of Sciences. All rights reserved.", "", df[i])
    df[i] <- sub("Envisioning the Data Science Discipline: The Undergraduate Perspective: Interim Report", "", df[i])
    df[i] <- sub("PREFACE", "", df[i])
    df[i] <- sub("ACKNOWLEDGEMENTS", "", df[i])
    df[i] <- sub("SUMMARY", "", df[i])
    df[i] <- sub("KNOWLEDGE FOR DATA SCIENTISTS", "", df[i])
    df[i] <- sub("DATA SCIENCE EDUCATION", "", df[i])
    df[i] <- sub("STARTING A DATA SCIENCE PROGRAM", "", df[i])
    df[i] <- sub("EVOLUTIONN AND EVALUATION", "", df[i])
    df[i] <- sub("CONCLUSIONS", "", df[i])
    df[i] <- gsub("\\n", "", df[i])
    df[i] <- gsub("\\s+", " ", df[i])
  }
  return(df)
}

# Clean reports
clean_report <- function(sentences) {
  sentences_clean <- as.data.frame(matrix(nrow = 0, ncol = 3))
  colnames(sentences_clean) <- c("sentence", "page", "sentence_number")
  
  sentence_clean <- as.data.frame(matrix(nrow = 1, ncol = 3))
  colnames(sentence_clean) <- c("sentence", "page", "sentence_number")
  
  for(i in 1:nrow(sentences)) {
    sentence <- sentences$sentence[i]
    sentence_page <- as.numeric(sentences$page[i])
    sentence_number <- as.numeric(sentences$sentence_number[i])
    sentence <- tolower(sentence)
    sentence <- str_replace_all(sentence, "- ", "")
    sentence <- str_replace_all(sentence, "[[:punct:]]", "")
    sentence <- str_replace_all(sentence, "[\\$,]", "")
    sentence <- str_replace_all(sentence, "[\\d+]", "")
    sentence <- str_replace_all(sentence, "●", "")
    sentence <- str_replace_all(sentence, "[0-9]", "")
    sentence <- removeWords(sentence, stopwords("en"))
    sentence <- str_replace_all(sentence, "\\s+", " ")
    sentence <- trimws(sentence)
    sentence_clean$sentence[1] <- sentence
    sentence_clean$page[1] <- sentence_page
    sentence_clean$sentence_number[1] <- sentence_number
    if(sentence != "") {
      sentences_clean <- rbind(sentences_clean, sentence_clean)
    }
  }
  return(sentences_clean)
}

# Converts each page of the report to a list of sentences, stores pages and sentences in a data frame
pages_to_sentences <- function(df) {
  all_page_sentences <- as.data.frame(matrix(nrow = 0, ncol = 3))
  colnames(all_page_sentences) <- c("sentence", "page", "sentence_number")
  one_page_sentence <- as.data.frame(matrix(nrow = 1, ncol = 3))
  colnames(one_page_sentence) <- c("sentence", "page", "sentence_number")
  for(i in 1:length(df)) {
    page_sentences <- strsplit(df[i], "\\. ")[[1]]
      for(j in 1:length(page_sentences)) {
        one_page_sentence$sentence <- as.character(page_sentences[j])
        one_page_sentence$page <- i
        one_page_sentence$sentence_number <- j
        all_page_sentences <- rbind(all_page_sentences, one_page_sentence)
      }
  }
  return(all_page_sentences)
}

# Removes rows for which the sentences are empty from the data frame
remove_blank_rows <- function(df) {
  clean_df <- as.data.frame(matrix(ncol = ncol(df), nrow = 0))
  colnames(clean_df) <- colnames(df)
  clean_df_row <- as.data.frame(matrix(ncol = ncol(df), nrow = 1))
  colnames(clean_df_row) <- colnames(df)
  for(r in 1:nrow(df)) {
    
  }
}

# Custom content transformer for tm_map
toNaught <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x))})


# Converts .csv file to document term matrix. The csv file MUST contain ONLY rows of text.
csv_to_dtm <- function(path) {
  text <- readLines(path)
  docs <- VCorpus(VectorSource(text))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, toNaught, "\a")
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  row.names(d) <- NULL
  return(d)
}

# Converts .csv file to stemmed document term matrix.  csv file MUST contain ONLY rows of text.
csv_to_dtm_stem <- function(path) {
  text <- readLines(path)
  docs <- VCorpus(VectorSource(text))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, toNaught, "\a")
  docs <- tm_map(docs, stemDocument)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  row.names(d) <- NULL
  return(d)
}

# Compares two corpuses, and tests whether the distribution of a particular specified word or stem is statistically signifcant using a chi square test
chi_compare_word_corpus <- function(corpus1, corpus2, word_to_test) {
  test_table <- matrix(nrow = 2, ncol = 2)
  colnames(test_table) <- c("word in corpus", "word not in corpus")
  rownames(test_table) <- c("corpus1", "corpus2")
  
  word_corpus1 <- 0
  not_word_corpus1 <- 0
  word_corpus2 <- 0
  not_word_corpus2 <- 0
  
  if(word_to_test %in% corpus1$word) {
    word_index1 <- as.numeric(which(corpus1$word == word_to_test))
    word_corpus1 <- corpus1$freq[word_index1]
    not_word_corpus1 <- sum(corpus1$freq[-word_index1])
  } else {
    not_word_corpus1 <- sum(corpus1$freq)
  }
  
  if(word_to_test %in% corpus2$word) {
    word_index2 <- as.numeric(which(corpus2$word == word_to_test))
    word_corpus2 <- corpus2$freq[word_index2]
    not_word_corpus2 <- sum(corpus2$freq[-word_index2])
  } else {
    not_word_corpus2 <- sum(corpus2$freq)
  }

  test_table[1,1] <- word_corpus1
  test_table[1,2] <- not_word_corpus1
  test_table[2,1] <- word_corpus2
  test_table[2,2] <- not_word_corpus2
  
  test_table <- as.table(test_table)
  
  print(test_table)
  chisq.test(test_table)
}

# Compares two corpuses, and tests whether the distribution of a particular specified word or stem is statistically signifcant using a chi square test.
chi_score_compare_word_corpus <- function(corpus1, corpus2, word_to_test) {
  test_table <- matrix(nrow = 2, ncol = 2)
  colnames(test_table) <- c("word in corpus", "word not in corpus")
  rownames(test_table) <- c("corpus1", "corpus2")
  
  word_corpus1 <- 0
  not_word_corpus1 <- 0
  word_corpus2 <- 0
  not_word_corpus2 <- 0
  
  if(word_to_test %in% corpus1$word) {
    word_index1 <- as.numeric(which(corpus1$word == word_to_test))
    word_corpus1 <- corpus1$freq[word_index1]
    not_word_corpus1 <- sum(corpus1$freq[-word_index1])
  } else {
    not_word_corpus1 <- sum(corpus1$freq)
  }
  
  if(word_to_test %in% corpus2$word) {
    word_index2 <- as.numeric(which(corpus2$word == word_to_test))
    word_corpus2 <- corpus2$freq[word_index2]
    not_word_corpus2 <- sum(corpus2$freq[-word_index2])
  } else {
    not_word_corpus2 <- sum(corpus2$freq)
  }
  
  test_table[1,1] <- word_corpus1
  test_table[1,2] <- not_word_corpus1
  test_table[2,1] <- word_corpus2
  test_table[2,2] <- not_word_corpus2
  
  test_table <- as.table(test_table)
  
  return(c(chisq.test(test_table), word_corpus1, word_corpus2, not_word_corpus1, not_word_corpus2))
}

# Produces table of p-values using a chi-square test
chi_compare_corpus_p_value <- function(corpus1, corpus2) {
  all_words <- unique(c(as.character(corpus1$word), as.character(corpus2$word)))
  compare_words <- as.data.frame(matrix(ncol = 6, nrow = length(all_words)))
  colnames(compare_words) <- c("word", "pvalue", "interim_rel_freq", "final_rel_freq", "interim_freq", "final_freq")
  compare_words$word <- all_words
  for(i in 1:length(all_words)) {
    word_to_test <- all_words[i]
    corpus_test <- chi_score_compare_word_corpus(corpus1, corpus2, word_to_test)
    compare_words$pvalue[i] <- corpus_test$p.value
    compare_words$interim_freq[i] <- corpus_test[[10]][1]
    compare_words$final_freq[i] <- corpus_test[[11]][1]
    compare_words$interim_rel_freq[i] <- corpus_test[[10]][1] / sum(corpus_test[[10]][1], corpus_test[[12]][1])
    compare_words$final_rel_freq[i] <- corpus_test[[11]][1] / sum(corpus_test[[11]][1], corpus_test[[13]][1])
  }
  return(compare_words)
}

# Converts a list of sentences, their associated page and associated sentence number to an edge-list
sentences_to_el <- function(sentence_list) {
  words_df <- as.data.frame(matrix(nrow = 0, ncol = 2))
  names(words_df) <- c("word", "page_sentence")
  
  words_line <- as.data.frame(matrix(nrow = 1, ncol = 2))
  names(words_line) <- c("word", "page_sentence")
  
  for(r in 1:nrow(sentence_list)) {
    page <- as.numeric(sentence_list$page[r])
    sentence <- as.numeric(sentence_list$sentence_number[r])/100
    page_sentence <- as.character(page + sentence)
    text <- as.character(sentence_list$sentence[r])
    text <- str_replace_all(text, "\\s+", " ")
    all_words <- strsplit(text, " ")[[1]]
    for(w in 1:length(all_words)) {
      words_line$word[1] <- all_words[w]
      words_line$page_sentence[1] <- page_sentence
      words_df <- rbind(words_df, words_line)
    }
  }
  return(words_df)
}

# Converts a list of sentences, their associated page and associated sentence number to an edge-list of stemmed words
sentences_to_el_stem <- function(sentence_list) {
  words_df <- as.data.frame(matrix(nrow = 0, ncol = 2))
  names(words_df) <- c("word", "page_sentence")
  
  words_line <- as.data.frame(matrix(nrow = 1, ncol = 2))
  names(words_line) <- c("word", "page_sentence")
  
  for(r in 1:nrow(sentence_list)) {
    page <- as.numeric(sentence_list$page[r])
    sentence <- as.numeric(sentence_list$sentence_number[r])/100
    page_sentence <- as.character(page + sentence)
    text <- as.character(sentence_list$sentence[r])
    text <- str_replace_all(text, "\\s+", " ")
    all_words <- strsplit(text, " ")[[1]]
    all_words <- wordStem(all_words, language = "en")
    for(w in 1:length(all_words)) {
      words_line$word[1] <- all_words[w]
      words_line$page_sentence[1] <- page_sentence
      words_df <- rbind(words_df, words_line)
    }
  }
  words_df <- unique(words_df)
  return(words_df)
}

# Converts the edgelist from sentences_to_el to a matrix
word_sentence_el_to_matrix <- function(word_sentence_el) {
  all_sentence_nums <- unique(word_sentence_el$page_sentence)
  all_sentence_nums <- all_sentence_nums[order(all_sentence_nums)]
  
  all_words <- unique(word_sentence_el$word)
  all_words <- all_words[order(all_words)]
  
  word_matrix <- matrix(data = 0, nrow = length(all_words), ncol = length(all_sentence_nums))
  
  row.names(word_matrix) <- all_words
  
  colnames(word_matrix) <- all_sentence_nums
  
  for(r in 1:nrow(word_sentence_el)) {
    word <- as.character(word_sentence_el$word[r])
    page_sentence <- as.character(word_sentence_el$page_sentence[r])
    word_dim <- which(all_words == word)
    page_sentence_dim <- which(all_sentence_nums == page_sentence)
    word_matrix[word_dim, page_sentence_dim] <- word_matrix[word_dim, page_sentence_dim] + 1
  }
  return(word_matrix)
}

# Creates a dataframe that divides words into buckets of n words in length
bucket_words <- function(text_to_bucket, num_buckets) {
  # Paste together cleaned sentences
  text_string <- ""
  for(r in 1:nrow(text_to_bucket)) {
    sentence <- text_to_bucket$sentence[r]
    text_string <- paste(text_string, sentence, sep = " ")
  }
  
  # Split all text into individual words
  all_words <- strsplit(text_string, " ")[[1]]
  all_words <- all_words[all_words != ""]
  words_in_order <- as.data.frame(all_words)
  words_in_order$index <- 1:nrow(words_in_order)
  words_in_order$bucket <- ""
  
  # Creates number of buckets, and items per bucket
  item_per_bucket <- round(max(words_in_order$index)/num_buckets, digits = 0)
  
  # Creates a dataframe with upper and lower bounds of buckets
  buckets <- as.data.frame(matrix(nrow = num_buckets, ncol = 2))
  colnames(buckets) <- c("lower_bound", "upper_bound")
  buckets$lower_bound[1] <- 0
  for(r in 2:num_buckets) {
    buckets$lower_bound[r] <- buckets$lower_bound[r-1] + item_per_bucket
  }
  for(r in 1:num_buckets-1) {
    buckets$upper_bound[r] <- buckets$lower_bound[r+1] - 1
  }
  buckets$upper_bound[num_buckets] <- max(words_in_order$index)
  
  # Assigns items to buckets
  for(b in 1:nrow(buckets)) {
    upper_bound <- buckets$upper_bound[b]
    lower_bound <- buckets$lower_bound[b]
    words_in_order$bucket[words_in_order$index <= upper_bound & words_in_order$index >= lower_bound] <- b
  }
  
  return(words_in_order)
}

bucket_frequency <- function(word_buckets, word_to_check) {
  # Creates a list of unique buckets for a given word
  # Arguments
  ## word_buckets - a dataframe with three columns: words (individual word), index (where that word appears in the text), and bucket (the bucket number that word appears in)
  total_words <- max(word_buckets$index)
  buckets <- unique(word_buckets$bucket)
  buckets <- as.data.frame(buckets)
  words_per_bucket <- round(total_words / nrow(buckets), digits = 0)
  colnames(buckets) <- "bucket"
  buckets$count <- 0
  
  # Counts the instances of the word in each bucket
  for(b in 1:nrow(buckets)) {
    bucket_subset <- word_buckets[word_buckets$bucket == b,]
    count_in_bucket <- sum(bucket_subset$all_words == word_to_check)
    buckets$count[b] <- count_in_bucket
  }
  
  buckets$prob <- buckets$count/words_per_bucket
  return(buckets)
}

# Creates a dataframe with one row per instance of word in a given bucket
bucket_density <- function(frequency_per_bucket) {
  # Drops rows with no instances of a word in that bucket
  frequency_per_bucket <- frequency_per_bucket[frequency_per_bucket$count > 0,]
  
  # Creates an empty dataframe to hold one row for each instance of the word in a bucket
  bucket_density_df <- as.data.frame(matrix(nrow = 0, ncol = 1))
  colnames(bucket_density_df) <- "bucket"
  
  # Creates a row to build that dataframe in a for loop
  row_to_add <- as.data.frame(matrix(nrow = 1, ncol = 1))
  colnames(row_to_add) <- "bucket"
  
  for(r in 1:nrow(frequency_per_bucket)) {
    bucket_number <- frequency_per_bucket$bucket[r]
    row_to_add$bucket[1] <- as.numeric(bucket_number)
    words_per_bucket <- frequency_per_bucket$count[r]
    for(w in 1:words_per_bucket) {
      bucket_density_df <- rbind(bucket_density_df, row_to_add)
    }
  }
  return(bucket_density_df)
}