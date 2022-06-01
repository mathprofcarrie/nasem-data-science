# Create vectors of normalized frequencies in "texts" - i.e. 100 word chunks
# Maybe we could have done chapters and sections here - but I think that the assumption holds
interim_100_word_freqs <- interim_100_words$SJEDI_count
final_100_word_freqs <- final_100_words$SJEDI_count

bootstraptest(interim_100_word_freqs, final_100_word_freqs, 10000)
