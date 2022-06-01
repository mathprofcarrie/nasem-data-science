# Call in pdfs
final_from_pdf <- pdf_text("FinalNASDSreport_reduced.pdf")
interim_from_pdf <- pdf_text("InterimNASDSreport_reduced.pdf")

# Remove headers etc.
final <- strip_headers(final_from_pdf)
interim <- strip_headers(interim_from_pdf)

# Split pdfs into sentences, writes sentences to dataframe with pages
final_sentences <- pages_to_sentences(final)
interim_sentences <- pages_to_sentences(interim)
EDSIN_sentences <- pages_to_sentences(EDSIN_proceedings_text)

# For the final report: removes whitespace, coverts to lower, removes punctuation, removes stopwords, stores cleaned data in final_sentences_clean
final_sentences_clean <- clean_report(final_sentences)
write.csv(final_sentences_clean$sentence, "final_sentences_clean.csv", row.names = FALSE)

# For the interim report: removes whitespace, coverts to lower, removes punctuation, removes stopwords, stores cleaned data in interim_sentences_clean
interim_sentences_clean <- clean_report(interim_sentences)
write.csv(interim_sentences_clean$sentence, "interim_sentences_clean.csv", row.names = FALSE)

# For the EDSIN report: removes whitespace, coverts to lower, removes punctuation, removes stopwords, stores cleaned data in EDSIN_sentences_clean
EDSIN_sentences_clean <- clean_report(EDSIN_sentences)
write.csv(EDSIN_sentences_clean$sentence, "EDSIN_sentences_clean.csv", row.names = FALSE)

rm(final_from_pdf) 
rm(interim_from_pdf)
rm(final, interim)
rm(final_sentences)
rm(interim_sentences)
