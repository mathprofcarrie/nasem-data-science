# Read in google corpuses
wf <- read.csv("wf.csv")
wf_stem <- read.csv("wf_stem.csv")

# Table of all words in EDSIN report
EDSIN_words_freq <- table(EDSIN_words[[1]])
EDSIN_words_freq <- as.data.frame(EDSIN_words_freq)
names(EDSIN_words_freq) <- c("word", "freq")
EDSIN_words_freq <- EDSIN_words_freq[!(EDSIN_words_freq$word %in% english_stopwords),]
EDSIN_words_freq$word <- as.character(EDSIN_words_freq$word)
EDSIN_words_freq <- EDSIN_words_freq[!(grepl("[0-9]", EDSIN_words_freq$word)),]
EDSIN_words_freq$rel_freq <- EDSIN_words_freq$freq/sum(EDSIN_words_freq$freq)

# Merge EDSIN words with all google words, keeping only EDSIN words
EDSIN_words_freq_google <- merge(EDSIN_words_freq, wf, by = "word", all.x = TRUE, all.y = FALSE)
names(EDSIN_words_freq_google)[5] <- "google_freq"
EDSIN_words_freq_google <- EDSIN_words_freq_google[!is.na(EDSIN_words_freq_google$language),]

# Drop words that have a frequency of less than .1% in the google corpus
EDSIN_words_infreq_google <- EDSIN_words_freq_google[EDSIN_words_freq_google$google_freq < 0.1,]

# Compare word frequency in interim and final report to google corpus
final_words_freq <- csv_to_dtm("final_sentences_clean.csv")
final_words_freq$rel_freq <- final_words_freq$freq/sum(final_words_freq$freq)
final_words_freq_google <- merge(final_words_freq, wf, by = "word", all.x = TRUE, all.y = FALSE)
names(final_words_freq_google)[5] <- "google_freq"
final_words_freq_google <- final_words_freq_google[!is.na(final_words_freq_google$language),]
final_words_infreq_google <- final_words_freq_google[final_words_freq_google$google_freq < 0.1,]

interim_words_freq <- csv_to_dtm("interim_sentences_clean.csv")
interim_words_freq$rel_freq <- interim_words_freq$freq/sum(interim_words_freq$freq)
interim_words_freq_google <- merge(interim_words_freq, wf, by = "word", all.x = TRUE, all.y = FALSE)
names(interim_words_freq_google)[5] <- "google_freq"
interim_words_freq_google <- interim_words_freq_google[!is.na(interim_words_freq_google$language),]
interim_words_infreq_google <- interim_words_freq_google[interim_words_freq_google$google_freq < 0.1,]

# Merge interim and final frequencies
compare_interim_final <- merge(interim_words_freq, final_words_freq, by = "word", all.x = TRUE, all.y = TRUE)
names(compare_interim_final) <- c("word", "interim_freq", "interim_rel_freq", "final_freq", "final_rel_freq")
for(c in 2:ncol(compare_interim_final)) {
  compare_interim_final[is.na(compare_interim_final[,c]),c] <- 0
}

# Compare word frequency in stemmed interim and final report to google corpus
final_stem_freq <- csv_to_dtm_stem("final_sentences_clean.csv")
final_stem_freq$rel_freq <- final_stem_freq$freq/sum(final_stem_freq$freq)
final_stem_freq_google <- merge(final_stem_freq, wf_stem, by = "word", all.x = TRUE, all.y = FALSE)
names(final_stem_freq_google)[5] <- "google_freq"
final_stem_freq_google <- final_stem_freq_google[!is.na(final_stem_freq_google$language),]
final_stem_infreq_google <- final_stem_freq_google[final_stem_freq_google$google_freq < 0.1,]

interim_stem_freq <- csv_to_dtm_stem("interim_sentences_clean.csv")
interim_stem_freq$rel_freq <- interim_stem_freq$freq/sum(interim_stem_freq$freq)
interim_stem_freq_google <- merge(interim_stem_freq, wf, by = "word", all.x = TRUE, all.y = FALSE)
names(interim_stem_freq_google)[5] <- "google_freq"
interim_stem_freq_google <- interim_stem_freq_google[!is.na(interim_stem_freq_google$language),]
interim_stem_infreq_google <- interim_stem_freq_google[interim_stem_freq_google$google_freq < 0.1,]

# Merge interim and final frequencies
compare_interim_final_stem <- merge(interim_stem_freq, final_stem_freq, by = "word", all.x = TRUE, all.y = TRUE)
names(compare_interim_final_stem) <- c("word", "interim_freq", "interim_rel_freq", "final_freq", "final_rel_freq")
for(c in 2:ncol(compare_interim_final_stem)) {
  compare_interim_final_stem[is.na(compare_interim_final_stem[,c]),c] <- 0
}