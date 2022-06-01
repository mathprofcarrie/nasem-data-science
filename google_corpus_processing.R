# Read in corpus of crawled google words
base_url <- "https://programminghistorian.org/assets/basic-text-processing-in-r"
wf <- read_csv(sprintf("%s/%s", base_url, "word_frequency.csv"))

# Create stemmed google corpus
stem_dict <- wf
stem_dict$stem <- text_tokens(stem_dict$word, stemmer = "en")

wf_stem <- as.data.frame(matrix(ncol = 3, nrow = length(unique(stem_dict$stem))))
names(wf_stem) <- names(wf)
wf_stem$language <- "en"
wf_stem$word <- unique(stem_dict$stem)

for(w in 1:nrow(wf_stem)) {
  stem <- as.character(wf_stem$word[w])
  matching_rows <- stem_dict[stem_dict$stem == stem,]
  wf_stem$frequency[w] <- sum(matching_rows$frequency)
}

wf_stem$word <- as.character(wf_stem$word)

write.csv(wf_stem, "wf_stem.csv", row.names = FALSE)
write.csv(wf, "wf.csv", row.names = FALSE)