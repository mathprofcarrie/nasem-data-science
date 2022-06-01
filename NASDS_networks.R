# Convert sentences to edge lists connecting words and sentences
interim_words_el <- sentences_to_el(interim_sentences_clean)
interim_words_el_stem <- sentences_to_el_stem(interim_sentences_clean)
final_words_el <- sentences_to_el(final_sentences_clean)
final_words_el_stem <- sentences_to_el_stem(final_sentences_clean)
EDSIN_words_el <- sentences_to_el(EDSIN_sentences_clean)
EDSIN_words_el_stem <- sentences_to_el_stem(EDSIN_sentences_clean)

# Converts edge lists to matrixes
interim_words_sent_matrix <- as.matrix(word_sentence_el_to_matrix(interim_words_el))
interim_words_stem_sent_matrix <- as.matrix(word_sentence_el_to_matrix(interim_words_el_stem))
final_words_sent_matrix <- as.matrix(word_sentence_el_to_matrix(final_words_el))
final_words_sent_stem_matrix <- as.matrix(word_sentence_el_to_matrix(final_words_el_stem))
EDSIN_words_sent_matrix <- as.matrix(word_sentence_el_to_matrix(EDSIN_words_el))
EDSIN_words_sent_stem_matrix <- as.matrix(word_sentence_el_to_matrix(EDSIN_words_el_stem))

# Produces symetrical matrixes
interim_words_matrix <- interim_words_sent_matrix %*% t(interim_words_sent_matrix)
interim_words_stem_matrix <- interim_words_stem_sent_matrix %*% t(interim_words_stem_sent_matrix)
final_words_matrix <- final_words_sent_matrix %*% t(final_words_sent_matrix)
final_words_stem_matrix <- final_words_sent_stem_matrix %*% t(final_words_sent_stem_matrix)
EDSIN_words_matrix <- EDSIN_words_sent_matrix %*% t(EDSIN_words_sent_matrix)
EDSIN_words_stem_matrix <- EDSIN_words_sent_stem_matrix %*% t(EDSIN_words_sent_stem_matrix)

# Produces network graph objects
interim_words.g <- graph.adjacency(interim_words_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
interim_words_stem.g <- graph.adjacency(interim_words_stem_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
final_words.g <- graph.adjacency(final_words_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
final_words_stem.g <- graph.adjacency(final_words_stem_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
EDSIN_words.g <- graph.adjacency(EDSIN_words_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
EDSIN_words_stem.g <- graph.adjacency(EDSIN_words_stem_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)

# Degree Centrality
degree.interim_words <- degree(interim_words.g)
degree.interim_words <- as.data.frame(degree.interim_words)
names(degree.interim_words) <- "interim_degree"
degree.interim_words$word <- row.names(degree.interim_words)
row.names(degree.interim_words) <- NULL

degree.interim_words_stem <- degree(interim_words_stem.g)
degree.interim_words_stem <- as.data.frame(degree.interim_words_stem)
degree.interim_words_stem <- as.data.frame(degree.interim_words_stem)
names(degree.interim_words_stem) <- "interim_degree"
degree.interim_words_stem$word <- row.names(degree.interim_words_stem)
row.names(degree.interim_words_stem) <- NULL

degree.final_words <- degree(final_words.g)
degree.final_words <- as.data.frame(degree.final_words)
names(degree.final_words) <- "final_degree"
degree.final_words$word <- row.names(degree.final_words)
row.names(degree.final_words) <- NULL

degree.final_words_stem <- degree(final_words_stem.g)
degree.final_words_stem <- as.data.frame(degree.final_words_stem)
names(degree.final_words_stem) <- "final_degree"
degree.final_words_stem$word <- row.names(degree.final_words_stem)
row.names(degree.final_words_stem) <- NULL

degree.words <- merge(degree.interim_words, degree.final_words, by = "word", all = TRUE)
degree.words[is.na(degree.words$interim_degree),]$interim_degree <- 0
degree.words[is.na(degree.words$final_degree),]$final_degree <- 0
degree.words$diff_degree <- degree.words$interim_degree - degree.words$final_degree
write.csv(degree.words, "degree_words.csv", row.names = FALSE)

degree.words_stem <- merge(degree.interim_words_stem, degree.final_words_stem, by = "word", all = TRUE)
degree.words_stem[is.na(degree.words_stem$interim_degree),]$interim_degree <- 0
degree.words_stem[is.na(degree.words_stem$final_degree),]$final_degree <- 0
write.csv(degree.words_stem, "degree_words_stem.csv", row.names = FALSE)

# Betweenness Centrality
btwn.interim_words <- betweenness(interim_words.g)
btwn.interim_words <- as.data.frame(btwn.interim_words)
names(btwn.interim_words) <- "interim_btwn"
btwn.interim_words$word <- row.names(btwn.interim_words)
row.names(btwn.interim_words) <- NULL

btwn.interim_words_stem <- betweenness(interim_words_stem.g)
btwn.interim_words_stem <- as.data.frame(btwn.interim_words_stem)
btwn.interim_words_stem <- as.data.frame(btwn.interim_words_stem)
names(btwn.interim_words_stem) <- "interim_btwn"
btwn.interim_words_stem$word <- row.names(btwn.interim_words_stem)
row.names(btwn.interim_words_stem) <- NULL

btwn.final_words <- betweenness(final_words.g)
btwn.final_words <- as.data.frame(btwn.final_words)
names(btwn.final_words) <- "final_btwn"
btwn.final_words$word <- row.names(btwn.final_words)
row.names(btwn.final_words) <- NULL

btwn.final_words_stem <- betweenness(final_words_stem.g)
btwn.final_words_stem <- as.data.frame(btwn.final_words_stem)
names(btwn.final_words_stem) <- "final_btwn"
btwn.final_words_stem$word <- row.names(btwn.final_words_stem)
row.names(btwn.final_words_stem) <- NULL

btwn.words <- merge(btwn.interim_words, btwn.final_words, by = "word", all = TRUE)
btwn.words[is.na(btwn.words$interim_btwn),]$interim_btwn <- 0
btwn.words[is.na(btwn.words$final_btwn),]$final_btwn <- 0
write.csv(btwn.words, "btwn_words.csv", row.names = FALSE)

btwn.words_stem <- merge(btwn.interim_words_stem, btwn.final_words_stem, by = "word", all = TRUE)
btwn.words_stem[is.na(btwn.words_stem$interim_btwn),]$interim_btwn <- 0
btwn.words_stem[is.na(btwn.words_stem$final_btwn),]$final_btwn <- 0
write.csv(btwn.words_stem, "btwn_words_stem.csv", row.names = FALSE)


# Eigenvector Centrality
cent.interim_words <- evcent(interim_words.g)
cent.interim_words <- as.data.frame(cent.interim_words$vector)
names(cent.interim_words) <- "interim_cent"
cent.interim_words$word <- row.names(cent.interim_words)
row.names(cent.interim_words) <- NULL

cent.interim_words_stem <- evcent(interim_words_stem.g)
cent.interim_words_stem <- as.data.frame(cent.interim_words_stem$vector)
cent.interim_words_stem <- as.data.frame(cent.interim_words_stem)
names(cent.interim_words_stem) <- "interim_cent"
cent.interim_words_stem$word <- row.names(cent.interim_words_stem)
row.names(cent.interim_words_stem) <- NULL

cent.final_words <- evcent(final_words.g)
cent.final_words <- as.data.frame(cent.final_words$vector)
names(cent.final_words) <- "final_cent"
cent.final_words$word <- row.names(cent.final_words)
row.names(cent.final_words) <- NULL

cent.final_words_stem <- evcent(final_words_stem.g)
cent.final_words_stem <- as.data.frame(cent.final_words_stem$vector)
names(cent.final_words_stem) <- "final_cent"
cent.final_words_stem$word <- row.names(cent.final_words_stem)
row.names(cent.final_words_stem) <- NULL

cent.words <- merge(cent.interim_words, cent.final_words, by = "word", all = TRUE)
cent.words[is.na(cent.words$interim_cent),]$interim_cent <- 0
cent.words[is.na(cent.words$final_cent),]$final_cent <- 0
write.csv(cent.words, "cent_words.csv", row.names = FALSE)

cent.words_stem <- merge(cent.interim_words_stem, cent.final_words_stem, by = "word", all = TRUE)
cent.words_stem[is.na(cent.words_stem$interim_cent),]$interim_cent <- 0
cent.words_stem[is.na(cent.words_stem$final_cent),]$final_cent <- 0
write.csv(cent.words_stem, "cent_words_stem.csv", row.names = FALSE)

# Reduce networks
interim_words_reduced.g <- delete.edges(interim_words.g, E(interim_words.g)[E(interim_words.g)$weight <= 5])
interim_words_reduced.g <- delete.vertices(interim_words_reduced.g, V(interim_words_reduced.g)[degree(interim_words_reduced.g)< 1])

interim_words_stem_reduced.g <- delete.edges(interim_words_stem.g, E(interim_words_stem.g)[E(interim_words_stem.g)$weight <= 1])
interim_words_stem_reduced.g <- delete.vertices(interim_words_stem_reduced.g, V(interim_words_stem_reduced.g)[degree(interim_words_stem_reduced.g)< 1])

final_words_reduced.g <- delete.edges(final_words.g, E(final_words.g)[E(final_words.g)$weight <= 1])
final_words_reduced.g <- delete.vertices(final_words_reduced.g, V(final_words_reduced.g)[degree(final_words_reduced.g)< 1])

final_words_stem_reduced.g <- delete.edges(final_words_stem.g, E(final_words_stem.g)[E(final_words_stem.g)$weight <= 1])
final_words_stem_reduced.g <- delete.vertices(final_words_stem_reduced.g, V(final_words_stem_reduced.g)[degree(final_words_stem_reduced.g)< 1])

EDSIN_words_reduced.g <- delete.edges(EDSIN_words.g, E(EDSIN_words.g)[E(EDSIN_words.g)$weight <= 1])
EDSIN_words_reduced.g <- delete.vertices(EDSIN_words_reduced.g, V(EDSIN_words_reduced.g)[degree(EDSIN_words_reduced.g)< 1])

EDSIN_words_stem_reduced.g <- delete.edges(EDSIN_words_stem.g, E(EDSIN_words_stem.g)[E(EDSIN_words_stem.g)$weight <= 1])
EDSIN_words_stem_reduced.g <- delete.vertices(EDSIN_words_stem_reduced.g, V(EDSIN_words_stem_reduced.g)[degree(EDSIN_words_stem_reduced.g)< 1])



# Communities
interim_communities <- cluster_walktrap(interim_words_reduced.g, weight = E(interim_words.g)$weight, steps = 6)
interim_communities <- as.data.frame(cbind(interim_communities$membership, interim_communities$names))
names(interim_communities) <- c("community", "word")
interim_communities$community <- as.numeric(interim_communities$community)
hist(interim_communities$community)

final_communities <- cluster_walktrap(final_words_reduced.g, weight = E(final_words.g)$weight, steps = 3)
final_communities <- as.data.frame(cbind(final_communities$membership, final_communities$names))
names(final_communities) <- c("community", "word")
final_communities$community <- as.numeric(final_communities$community)
hist(final_communities$community)
