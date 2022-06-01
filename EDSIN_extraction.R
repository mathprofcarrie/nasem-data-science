# Read in EDSIN proceedings
EDSIN_proceedings <- pdf_text("2019_EDSIN_Conference_Proceedings.pdf")

# Trim to proceedings content
EDSIN_proceedings <- EDSIN_proceedings[c(4:15, 30:43)]
EDSIN_proceedings_text <- ""
for(r in 1:length(EDSIN_proceedings)) {
  row_text <- as.character(EDSIN_proceedings[r])
  EDSIN_proceedings_text <- paste(EDSIN_proceedings_text, row_text)
}

# Split into words associated with sentences
EDSIN_sentences <- tokenize_sentences(EDSIN_proceedings_text)
EDSIN_words <- tokenize_words(EDSIN_proceedings_text)

# Read in stopwords
english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")

# Convert sentence lists to df, add id numbers
EDSIN_sentences_df <- as.data.frame(EDSIN_sentences[[1]])
EDSIN_sentences_df$doc_id <- row.names(EDSIN_sentences_df)
names(EDSIN_sentences_df)[1] <- "text"

# Convert word lists to df, add id numbers
EDSIN_words_df <- as.data.frame(EDSIN_words[[1]])
EDSIN_words_df$doc_id <- row.names(EDSIN_words_df)
names(EDSIN_words_df)[1] <- "text"

# Create corpora
EDSIN_sentence_corpus <- Corpus(DataframeSource(EDSIN_sentences_df))
EDSIN_word_corpus <- Corpus(DataframeSource(EDSIN_words_df))

# Clean corpora
EDSIN_sentence_processedCorpus <- tm_map(EDSIN_sentence_corpus, content_transformer(tolower))
EDSIN_sentence_processedCorpus <- tm_map(EDSIN_sentence_processedCorpus, removeWords, english_stopwords)
EDSIN_sentence_processedCorpus <- tm_map(EDSIN_sentence_processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
EDSIN_sentence_processedCorpus <- tm_map(EDSIN_sentence_processedCorpus, removeNumbers)
#EDSIN_sentence_processedCorpus <- tm_map(EDSIN_sentence_processedCorpus, stemDocument)
EDSIN_sentence_processedCorpus <- tm_map(EDSIN_sentence_processedCorpus, stripWhitespace)

rm(EDSIN_proceedings)
rm(EDSIN_sentences)
rm(EDSIN_sentence_corpus)
rm(EDSIN_word_corpus)