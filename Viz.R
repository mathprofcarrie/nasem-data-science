# Stores keywords
JEDI_words <- c("ability", "access", "accessibility", "accessible", "ally", "allies", "background", "backgrounds", "barriers", "collaboration", "colaborative", "communities", "community", "connect", "cultural", "cultures", "intercultural", "dei", "diverse", "divide", "diversity", "engage", "engagement", "engaging", "equitable", "equity", "ethnic", "ethnicity", "ethic", "ethical", "ethics", "hawaiian", "hispanic", "identified", "identify", "identity", "impact", "inclusion", "inclusive", "indigenous", "islander", "justice", "latin", "marginalized", "perspectives", "pipeline", "poc", "racial", "race", "recruitment", "retention", "underrepresent", "underrepresented", "underserve", "underserved")

# Plot interim vs. final centrality measures
all_centrality_words <- merge(degree.words, btwn.words, by = "word")
all_centrality_words <- merge(all_centrality_words, cent.words, by = "word")
JEDI_test <- all_centrality_words$word %in% JEDI_words
all_centrality_words$JEDI[JEDI_test] <- "JEDI word"
all_centrality_words$JEDI[!JEDI_test] <- "Not JEDI word"

# Plot interim vs. final centrality measures
all_centrality_stems <- merge(degree.words_stem, btwn.words_stem, by = "word")
all_centrality_stems <- merge(all_centrality_stems, cent.words_stem, by = "word")
JEDI_test <- all_centrality_stems$word %in% JEDI_stem
all_centrality_stems$JEDI[JEDI_test] <- "JEDI word"
all_centrality_stems$JEDI[!JEDI_test] <- "Not JEDI word"

png("degree_plot.png", width = 2500, height = 2500, res = 300)
degree.plot <- ggplot(all_centrality_words, aes(interim_degree, final_degree, color = JEDI))  + geom_point(aes(alpha = JEDI)) + xlim(0, 1500) + ylim(0, 1500) + coord_fixed()+ geom_abline() + scale_alpha_discrete(range=c(1, 0.1))
degree.plot
dev.off()

png("btwn_plot.png", width = 2500, height = 2500, res = 300)
degree.plot <- ggplot(all_centrality_words, aes(interim_btwn, final_btwn, color = JEDI))  + geom_point(aes(alpha = JEDI)) + xlim(0, 50000) + ylim(0, 50000) + coord_fixed() + geom_abline() + scale_alpha_discrete(range=c(1, 0.1))
degree.plot
dev.off()

# Eigenvector words
png("cent_plot.png", width = 2500, height = 2500, res = 300)
degree.plot <- ggplot(all_centrality_words, aes(interim_cent, final_cent, color = JEDI))  + geom_point(aes(alpha = JEDI)) + xlim(0, 0.2) + ylim(0, 0.2) + scale_alpha_discrete(range=c(1, 0.1)) + coord_fixed() + geom_abline() + labs(x = "Interim Report Eigenvector Centrality",y = "Final Report Eigenvector Centrality")
degree.plot
dev.off()

# Eigenvector stems
png("cent_plot_stems.png", width = 2500, height = 2500, res = 300)
degree.plot <- ggplot(all_centrality_stems, aes(interim_cent, final_cent, color = JEDI))  + geom_point(aes(alpha = JEDI)) + xlim(0, 0.2) + ylim(0, 0.2) + scale_alpha_discrete(range=c(1, 0.1)) + coord_fixed() + geom_abline() + labs(x = "Interim Report Eigenvector Centrality (stem)",y = "Final Report Eigenvector Centrality (stem)")
degree.plot
dev.off()

# Plots EDSIN word network
V(EDSIN_words_reduced.g)$JEDI <- V(EDSIN_words_reduced.g)$name %in% JEDI_words
V(EDSIN_words_reduced.g)$JEDI[V(EDSIN_words_reduced.g)$JEDI == TRUE] <- 1
V(EDSIN_words_reduced.g)$JEDI <- V(EDSIN_words_reduced.g)$JEDI + 1

V(EDSIN_words_reduced.g)$label <- NA
network_colors <- c(rgb(255, 194, 10, max = 255), rgb(12, 123, 220, max = 255))
V(EDSIN_words_reduced.g)$color <- network_colors[1]
V(EDSIN_words_reduced.g)$color <- network_colors[V(EDSIN_words_reduced.g)$JEDI]


png("EDSIN_network.png", width = 2500, height = 2500, res = 150)
la <- layout_with_fr(EDSIN_words_reduced.g)
#e.wt <- get.edge.attribute(interim_words_reduced.g, "weight")
plot(EDSIN_words_reduced.g, vertex.size = 2, edge.width = 1, edge.curved = .5)
dev.off()

# Plots interim word network
V(interim_words_reduced.g)$JEDI <- V(interim_words_reduced.g)$name %in% JEDI_words
V(interim_words_reduced.g)$JEDI[V(interim_words_reduced.g)$JEDI == TRUE] <- 1
V(interim_words_reduced.g)$JEDI <- V(interim_words_reduced.g)$JEDI + 1

V(interim_words_reduced.g)$label <- NA
network_colors <- c(rgb(255, 194, 10, max = 255), rgb(12, 123, 220, max = 255))
V(interim_words_reduced.g)$color <- network_colors[1]
V(interim_words_reduced.g)$color <- network_colors[V(interim_words_reduced.g)$JEDI]


png("interim_network.png", width = 2500, height = 2500, res = 150)
la <- layout_with_kk(interim_words_reduced.g)
#e.wt <- get.edge.attribute(interim_words_reduced.g, "weight")
plot(interim_words_reduced.g, vertex.size = 1, edge.width = 1, edge.curved = .5)
dev.off()

# Plots final word network
V(final_words_reduced.g)$JEDI <- V(final_words_reduced.g)$name %in% JEDI_words
V(final_words_reduced.g)$JEDI[V(final_words_reduced.g)$JEDI == TRUE] <- 1
V(final_words_reduced.g)$JEDI <- V(final_words_reduced.g)$JEDI + 1

V(final_words_reduced.g)$label <- NA
V(final_words_reduced.g)$color <- network_colors[1]
V(final_words_reduced.g)$color <- network_colors[V(final_words_reduced.g)$JEDI]


png("final_network.png", width = 2500, height = 2500, res = 150)
la <- layout_with_kk(final_words_reduced.g)
#e.wt <- get.edge.attribute(final_words_reduced.g, "weight")
plot(final_words_reduced.g, vertex.size = 2, edge.width = 1, edge.curved = 0.5)
dev.off()

# Plots interim stem network
V(interim_words_stem_reduced.g)$JEDI <- V(interim_words_stem_reduced.g)$name %in% JEDI_words
V(interim_words_stem_reduced.g)$JEDI[V(interim_words_stem_reduced.g)$JEDI == TRUE] <- 1
V(interim_words_stem_reduced.g)$JEDI <- V(interim_words_stem_reduced.g)$JEDI + 1

V(interim_words_stem_reduced.g)$label <- NA
network_colors <- c(rgb(255, 194, 10, max = 255), rgb(12, 123, 220, max = 255))
V(interim_words_stem_reduced.g)$color <- network_colors[1]
V(interim_words_stem_reduced.g)$color <- network_colors[V(interim_words_stem_reduced.g)$JEDI]


png("interim_network_stem.png", width = 2500, height = 2500, res = 150)
la <- layout_with_kk(interim_words_stem_reduced.g)
#e.wt <- get.edge.attribute(interim_words_reduced.g, "weight")
plot(interim_words_stem_reduced.g, vertex.size = 1, edge.width = 1, edge.curved = .5)
dev.off()

# Plots final stem network
V(final_words_reduced.g)$JEDI <- V(final_words_reduced.g)$name %in% JEDI_words
V(final_words_reduced.g)$JEDI[V(final_words_reduced.g)$JEDI == TRUE] <- 1
V(final_words_reduced.g)$JEDI <- V(final_words_reduced.g)$JEDI + 1

V(final_words_reduced.g)$label <- NA
V(final_words_reduced.g)$color <- network_colors[1]
V(final_words_reduced.g)$color <- network_colors[V(final_words_reduced.g)$JEDI]

png("final_network.png", width = 2500, height = 2500, res = 150)
la <- layout_with_kk(final_words_reduced.g)
#e.wt <- get.edge.attribute(final_words_reduced.g, "weight")
plot(final_words_reduced.g, vertex.size = 2, edge.width = 1, edge.curved = 0.5)
dev.off()