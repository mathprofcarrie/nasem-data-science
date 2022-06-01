## INTERIM VISUALIZATION

# Write edge list to .csv and manually remove URLs and other nonsense words
#write.csv(interim_words_el_stem, "interim_words_el_stem.csv")
interim_words_el_stem_clean <- read.csv("interim_words_el_stem.csv")
interim_words_el_stem_clean <- interim_words_el_stem_clean[,2:3]

# Creates a graph object connecting words to sentences
interim_stem_sent.net <- graph.data.frame(interim_words_el_stem_clean, directed = FALSE)
# Adds a type attribute that distinguishes between words and sentences
V(interim_stem_sent.net)$type <- bipartite_mapping(interim_stem_sent.net)$type

# Set sentence nodes to blue squares
V(interim_stem_sent.net)$shape <- ifelse(V(interim_stem_sent.net)$type, "square", "circle")
V(interim_stem_sent.net)$color <- ifelse(V(interim_stem_sent.net)$type, rgb(.68, .85, .9, 0), "#b8e186")

# Set degrees of nodes
V(interim_stem_sent.net)$degree <- degree(interim_stem_sent.net)

# Set labels
V(interim_stem_sent.net)[V(interim_stem_sent.net)$type == TRUE]$label <- ""
V(interim_stem_sent.net)[V(interim_stem_sent.net)$type == FALSE]$label <- V(interim_stem_sent.net)[V(interim_stem_sent.net)$type == FALSE]$name

# Set vertex sizes
V(interim_stem_sent.net)[V(interim_stem_sent.net)$type == TRUE]$size <- 0
V(interim_stem_sent.net)[V(interim_stem_sent.net)$type == FALSE]$size <- 2

# Highlight SJEDI words
V(interim_stem_sent.net)[V(interim_stem_sent.net)$name %in% JEDI_stem]$color <- "#d01c8b"

png("interim_bipartite.png", width = 15000, height = 15000, res = 150)
plot(interim_stem_sent.net, vertex.label = V(interim_stem_sent.net)$label, vertex.label.cex = 0.7, vertex.size = V(interim_stem_sent.net)$size, layout=layout_with_fr)
dev.off()

## FINAL VISUALIZATION

# Write edge list to .csv and manually remove URLs and other nonsense words
#write.csv(final_words_el_stem, "final_words_el_stem.csv")
final_words_el_stem_clean <- read.csv("final_words_el_stem.csv")
final_words_el_stem_clean <- final_words_el_stem_clean[,2:3]

# Drop all instances of words that appear only once
final_words_el_stem_clean <- final_words_el_stem_clean[!final_words_el_stem_clean$word %in% stems_to_delete,]

# Creates a graph object connecting words to sentences
final_stem_sent.net <- graph.data.frame(final_words_el_stem_clean, directed = FALSE)
# Adds a type attribute that distinguishes between words and sentences
V(final_stem_sent.net)$type <- bipartite_mapping(final_stem_sent.net)$type

# Set sentence nodes to blue squares
V(final_stem_sent.net)$shape <- ifelse(V(final_stem_sent.net)$type, "square", "circle")
V(final_stem_sent.net)$color <- ifelse(V(final_stem_sent.net)$type, rgb(.68, .85, .9, .5), "#b8e186")

# Set degrees of nodes
V(final_stem_sent.net)$degree <- degree(final_stem_sent.net)

# Set labels
V(final_stem_sent.net)[V(final_stem_sent.net)$type == TRUE]$label <- ""
V(final_stem_sent.net)[V(final_stem_sent.net)$type == FALSE]$label <- V(final_stem_sent.net)[V(final_stem_sent.net)$type == FALSE]$name

# Set vertex sizes
V(final_stem_sent.net)[V(final_stem_sent.net)$type == TRUE]$size <- 0
V(final_stem_sent.net)[V(final_stem_sent.net)$type == FALSE]$size <- 2

# Highlight SJEDI words
V(final_stem_sent.net)[V(final_stem_sent.net)$name %in% JEDI_stem]$color <- "#d01c8b"

png("final_bipartite.png", width = 15000, height = 15000, res = 150)
plot(final_stem_sent.net, vertex.label = V(final_stem_sent.net)$label, vertex.label.cex = 0.7, vertex.size = V(final_stem_sent.net)$size, layout=layout_with_fr)
dev.off()

# COMPARE CENTRALITIES - STEMS

# Eigenvector Centrality on the bipartite network
interim_words_stem_all.net_bip <- graph.data.frame(interim_words_el_stem, directed = FALSE)
eig.interim_words_stem_bip <- evcent(interim_words_stem_all.net_bip)
eig.interim_words_stem_bip <- as.data.frame(eig.interim_words_stem_bip$vector)
eig.interim_words_stem_bip <- as.data.frame(eig.interim_words_stem_bip)
names(eig.interim_words_stem_bip) <- "interim_eig_cent"
eig.interim_words_stem_bip$word <- row.names(eig.interim_words_stem_bip)
row.names(eig.interim_words_stem_bip) <- NULL

final_words_stem_all.net_bip <- graph.data.frame(final_words_el_stem, directed = FALSE)
eig.final_words_stem_bip <- evcent(final_words_stem_all.net_bip)
eig.final_words_stem_bip <- as.data.frame(eig.final_words_stem_bip$vector)
names(eig.final_words_stem_bip) <- "final_eig_cent"
eig.final_words_stem_bip$word <- row.names(eig.final_words_stem_bip)
row.names(eig.final_words_stem_bip) <- NULL

eig.words_stem_bip <- merge(eig.interim_words_stem_bip, eig.final_words_stem_bip, by = "word", all = TRUE)
eig.words_stem_bip <- eig.words_stem_bip[!grepl("[[:digit:]]", eig.words_stem_bip$word), ]
row.names(eig.words_stem_bip) <- NULL

eig.words_stem_bip[is.na(eig.words_stem_bip$interim_eig_cent),]$interim_eig_cent <- 0
eig.words_stem_bip[is.na(eig.words_stem_bip$final_eig_cent),]$final_eig_cent <- 0
write.csv(eig.words_stem_bip, "eig_words_stem_bip.csv", row.names = FALSE)

# Degree Centrality on the bipartite network
degree.interim_words_stem_bip <- degree(interim_words_stem_all.net_bip)
degree.interim_words_stem_bip <- as.data.frame(degree.interim_words_stem_bip)
names(degree.interim_words_stem_bip) <- "interim_degree"
degree.interim_words_stem_bip$word <- row.names(degree.interim_words_stem_bip)
row.names(degree.interim_words_stem_bip) <- NULL

degree.final_words_stem_bip <- degree(final_words_stem_all.net_bip)
degree.final_words_stem_bip <- as.data.frame(degree.final_words_stem_bip)
names(degree.final_words_stem_bip) <- "final_degree"
degree.final_words_stem_bip$word <- row.names(degree.final_words_stem_bip)
row.names(degree.final_words_stem_bip) <- NULL

degree.words_stem_bip <- merge(degree.interim_words_stem_bip, degree.final_words_stem_bip, by = "word", all = TRUE)
degree.words_stem_bip <- degree.words_stem_bip[!grepl("[[:digit:]]", degree.words_stem_bip$word), ]

degree.words_stem_bip[is.na(degree.words_stem_bip$interim_degree),]$interim_degree <- 0
degree.words_stem_bip[is.na(degree.words_stem_bip$final_degree),]$final_degree <- 0
write.csv(degree.words_stem_bip, "degree_words_stem_bip.csv", row.names = FALSE)

# Combine centrality dataframes
cent.words_stem_bip <- merge(eig.words_stem_bip, degree.words_stem_bip, by = "word", all = TRUE)

## VISUALIZE COMPARRISON
# Add colors based on JEDI word status
JEDI_test <- cent.words_stem_bip$word %in% JEDI_stem
cent.words_stem_bip$JEDI[JEDI_test] <- "SJEDI word"
cent.words_stem_bip$JEDI[!JEDI_test] <- "Not SJEDI word"
cent.words_stem_bip$opacity[JEDI_test] <- 1
cent.words_stem_bip$opacity[!JEDI_test] <- 0.7
cent.words_stem_bip <- cent.words_stem_bip[order(cent.words_stem_bip$JEDI), ]

# Eigenvector stems from bipartite network
png("eig_cent_plot_stems_bip.png", width = 2500, height = 2500, res = 300)
degree.plot <- ggplot(cent.words_stem_bip)  + geom_point(x = cent.words_stem_bip$interim_eig_cent, y = cent.words_stem_bip$final_eig_cent, aes(color = JEDI), alpha = cent.words_stem_bip$opacity) + xlim(0, 0.2) + ylim(0, 0.2) + coord_fixed() + geom_abline() + labs(x = "Interim Report Eigenvector Centrality (stem)",y = "Final Report Eigenvector Centrality (stem)") + scale_colour_manual(values = c("#b8e186", "#d01c8b"), aesthetics = "colour",  breaks = waiver(),na.value = "grey50")
degree.plot
dev.off()
