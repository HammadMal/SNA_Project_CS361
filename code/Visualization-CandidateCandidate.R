# ==============================================================================
# Social Network Analysis - Pakistan Elections
# Script: Candidate-Candidate Network Visualizations (ENHANCED - MORE LABELS)
# ==============================================================================
# Authors: Hammad Malik (hm08298) & Mehlab Kashani (mk07950)
# Course: CS/SDP 361/352
# Date: November 2, 2025
# Network Type: Candidate-Candidate Unipartite Projection (Weighted)
# ENHANCEMENT: MORE node labels (50-60% threshold) for better visibility
# ==============================================================================

# --- Load Required Libraries ---
library(igraph)

# ==============================================================================
# 1. CONFIGURATION
# ==============================================================================

cat("================================================================================\n")
cat("CANDIDATE-CANDIDATE NETWORK VISUALIZATIONS (ENHANCED - MORE LABELS)\n")
cat("================================================================================\n\n")

# Set your data paths here
resultpath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/results"

# Input files
metrics_dir <- file.path(resultpath, "network_metrics_candidate_candidate")
bipartite_dir <- file.path(resultpath, "Bipartition_CandidateCandidate_output")
network_file <- file.path(bipartite_dir, "candidate_candidate_network.rds")
centrality_file <- file.path(metrics_dir, "centrality_scores_all.csv")

# Output directory
output_dir <- file.path(resultpath, "visualizations_candidate_candidate")

# Create output directory if it doesn't exist
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ==============================================================================
# 2. LOAD DATA
# ==============================================================================

cat("Loading data...\n")

# Load network
g_candidate <- readRDS(network_file)
cat(sprintf("✓ Loaded candidate network: %d nodes, %d edges\n", 
            vcount(g_candidate), ecount(g_candidate)))

# Load centrality scores
centrality <- read.csv(centrality_file, stringsAsFactors = FALSE)
cat(sprintf("✓ Loaded centrality scores for %d candidates\n\n", nrow(centrality)))

# ==============================================================================
# 3. DEGREE DISTRIBUTION PLOTS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING DEGREE DISTRIBUTION PLOTS\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "01_degree_distribution.pdf"), width = 12, height = 8)

# Get degrees
deg <- degree(g_candidate)

# Layout for 2x2 plots
par(mfrow = c(2, 2))

# --- Plot 1: Histogram ---
hist(deg, 
     breaks = 50,
     main = "Degree Distribution - Candidate-Candidate Network",
     xlab = "Degree (Number of Connections)",
     ylab = "Number of Candidates",
     col = "steelblue",
     border = "white")
abline(v = mean(deg), col = "red", lwd = 2, lty = 2)
legend("topright", 
       legend = c(sprintf("Mean = %.2f", mean(deg))),
       col = "red", lty = 2, lwd = 2)

# --- Plot 2: Log-Log Scale (Power Law Test) ---
deg_table <- table(deg)
deg_counts <- as.numeric(deg_table)
deg_values <- as.numeric(names(deg_table))

plot(deg_values, deg_counts,
     log = "xy",
     main = "Log-Log Degree Distribution",
     xlab = "Degree (log scale)",
     ylab = "Frequency (log scale)",
     pch = 16,
     col = "steelblue",
     cex = 1.5)

# --- Plot 3: Cumulative Distribution ---
deg_sorted <- sort(deg, decreasing = TRUE)
plot(1:length(deg_sorted), deg_sorted,
     type = "l",
     main = "Cumulative Degree Distribution",
     xlab = "Rank",
     ylab = "Degree",
     col = "steelblue",
     lwd = 2)
grid()

# --- Plot 4: Boxplot ---
boxplot(deg,
        main = "Degree Distribution - Boxplot",
        ylab = "Degree",
        col = "lightblue",
        border = "steelblue")
points(1, mean(deg), col = "red", pch = 18, cex = 2)
legend("topright", 
       legend = c("Mean"),
       col = "red", pch = 18, pt.cex = 2)

par(mfrow = c(1, 1))
dev.off()

cat("✓ Saved: 01_degree_distribution.pdf\n\n")

# ==============================================================================
# 4. CENTRALITY COMPARISON PLOTS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING CENTRALITY COMPARISON PLOTS\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "02_centrality_comparison.pdf"), width = 14, height = 10)

# Get top 15 candidates by degree
top_candidates <- head(centrality[order(-centrality$Degree), ], 15)

# Normalize centralities to 0-1 scale
top_candidates$Degree_norm <- top_candidates$Degree / max(centrality$Degree)
top_candidates$Betweenness_norm <- top_candidates$Betweenness / max(centrality$Betweenness)
top_candidates$Closeness_norm <- top_candidates$Closeness / max(centrality$Closeness)
top_candidates$Eigenvector_norm <- top_candidates$Eigenvector / max(centrality$Eigenvector)
top_candidates$PageRank_norm <- top_candidates$PageRank / max(centrality$PageRank)

# --- Plot: Grouped Bar Chart ---
cent_matrix <- as.matrix(top_candidates[, c("Degree_norm", "Betweenness_norm", 
                                              "Closeness_norm", "Eigenvector_norm", 
                                              "PageRank_norm")])
rownames(cent_matrix) <- top_candidates$Candidate_ID

barplot(t(cent_matrix),
        beside = TRUE,
        main = "Normalized Centrality Measures - Top 15 Candidates",
        xlab = "",
        ylab = "Normalized Centrality (0-1)",
        col = c("steelblue", "coral", "gold", "lightgreen", "purple"),
        legend.text = c("Degree", "Betweenness", "Closeness", "Eigenvector", "PageRank"),
        args.legend = list(x = "topright", cex = 0.9),
        las = 2,
        cex.names = 0.6)

dev.off()

cat("✓ Saved: 02_centrality_comparison.pdf\n\n")

# ==============================================================================
# 5. INDIVIDUAL CENTRALITY PLOTS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING INDIVIDUAL CENTRALITY PLOTS\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "03_centrality_individual.pdf"), width = 14, height = 16)

# Set up layout with adjusted margins
par(mfrow = c(2, 3),
    mar = c(10, 4, 5, 2),
    oma = c(2, 0, 3, 0),
    mgp = c(3, 1, 0))

# --- Degree Centrality ---
top_deg <- head(centrality[order(-centrality$Degree), ], 20)
barplot(top_deg$Degree,
        names.arg = top_deg$Candidate_ID,
        main = "Top 20 Candidates by\nDegree Centrality",
        ylab = "Degree",
        col = "steelblue",
        las = 2,
        cex.names = 0.65,
        cex.main = 1.0)

# --- Betweenness Centrality ---
top_betw <- head(centrality[order(-centrality$Betweenness), ], 20)
barplot(top_betw$Betweenness,
        names.arg = top_betw$Candidate_ID,
        main = "Top 20 Candidates by\nBetweenness Centrality",
        ylab = "Betweenness",
        col = "coral",
        las = 2,
        cex.names = 0.65,
        cex.main = 1.0)

# --- Closeness Centrality ---
top_clos <- head(centrality[order(-centrality$Closeness), ], 20)
barplot(top_clos$Closeness,
        names.arg = top_clos$Candidate_ID,
        main = "Top 20 Candidates by\nCloseness Centrality",
        ylab = "Closeness",
        col = "gold",
        las = 2,
        cex.names = 0.65,
        cex.main = 1.0)

# --- Eigenvector Centrality ---
top_eigen <- head(centrality[order(-centrality$Eigenvector), ], 20)
barplot(top_eigen$Eigenvector,
        names.arg = top_eigen$Candidate_ID,
        main = "Top 20 Candidates by\nEigenvector Centrality",
        ylab = "Eigenvector",
        col = "lightgreen",
        las = 2,
        cex.names = 0.65,
        cex.main = 1.0)

# --- PageRank Centrality ---
top_pr <- head(centrality[order(-centrality$PageRank), ], 20)
barplot(top_pr$PageRank,
        names.arg = top_pr$Candidate_ID,
        main = "Top 20 Candidates by\nPageRank Centrality",
        ylab = "PageRank",
        col = "purple",
        las = 2,
        cex.names = 0.65,
        cex.main = 1.0)

# --- Eccentricity ---
top_ecc <- head(centrality[order(centrality$Eccentricity), ], 20)
barplot(top_ecc$Eccentricity,
        names.arg = top_ecc$Candidate_ID,
        main = "Top 20 Candidates by\nEccentricity (Lowest)",
        ylab = "Eccentricity",
        col = "skyblue",
        las = 2,
        cex.names = 0.65,
        cex.main = 1.0)

# Reset plotting parameters
par(mfrow = c(1, 1),
    mar = c(5, 4, 4, 2),
    oma = c(0, 0, 0, 0),
    mgp = c(3, 1, 0))
dev.off()

cat("✓ Saved: 03_centrality_individual.pdf\n\n")

# ==============================================================================
# 6. CENTRALITY CORRELATION PLOTS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING CENTRALITY CORRELATION PLOTS\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "04_centrality_correlations.pdf"), width = 12, height = 10)

# Create correlation matrix
cent_cols <- c("Degree", "Betweenness", "Closeness", "Eigenvector", "PageRank")
cent_data <- centrality[, cent_cols]

# Compute correlations
cor_matrix <- cor(cent_data, use = "complete.obs")

# Plot correlation matrix
par(mar = c(8, 8, 3, 2))
image(1:5, 1:5, cor_matrix,
      col = colorRampPalette(c("blue", "white", "red"))(100),
      xlab = "", ylab = "",
      main = "Centrality Measures Correlation Matrix",
      axes = FALSE)
axis(1, at = 1:5, labels = cent_cols, las = 2)
axis(2, at = 1:5, labels = cent_cols, las = 2)

# Add correlation values
for(i in 1:5) {
  for(j in 1:5) {
    text(i, j, sprintf("%.2f", cor_matrix[i, j]), cex = 1.2)
  }
}

par(mar = c(5, 4, 4, 2))

dev.off()

cat("✓ Saved: 04_centrality_correlations.pdf\n\n")

# ==============================================================================
# 7. SCATTER PLOTS - CENTRALITY RELATIONSHIPS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING CENTRALITY SCATTER PLOTS\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "05_centrality_scatterplots.pdf"), width = 14, height = 10)

par(mfrow = c(2, 3))

# --- Degree vs Betweenness ---
plot(centrality$Degree, centrality$Betweenness,
     main = "Degree vs Betweenness",
     xlab = "Degree Centrality",
     ylab = "Betweenness Centrality",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(lm(centrality$Betweenness ~ centrality$Degree), col = "red", lwd = 2)

# --- Degree vs Closeness ---
plot(centrality$Degree, centrality$Closeness,
     main = "Degree vs Closeness",
     xlab = "Degree Centrality",
     ylab = "Closeness Centrality",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(lm(centrality$Closeness ~ centrality$Degree), col = "red", lwd = 2)

# --- Degree vs Eigenvector ---
plot(centrality$Degree, centrality$Eigenvector,
     main = "Degree vs Eigenvector",
     xlab = "Degree Centrality",
     ylab = "Eigenvector Centrality",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(lm(centrality$Eigenvector ~ centrality$Degree), col = "red", lwd = 2)

# --- Betweenness vs Closeness ---
plot(centrality$Betweenness, centrality$Closeness,
     main = "Betweenness vs Closeness",
     xlab = "Betweenness Centrality",
     ylab = "Closeness Centrality",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(lm(centrality$Closeness ~ centrality$Betweenness), col = "red", lwd = 2)

# --- Eigenvector vs PageRank ---
plot(centrality$Eigenvector, centrality$PageRank,
     main = "Eigenvector vs PageRank",
     xlab = "Eigenvector Centrality",
     ylab = "PageRank Centrality",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(lm(centrality$PageRank ~ centrality$Eigenvector), col = "red", lwd = 2)

# --- Degree vs PageRank ---
plot(centrality$Degree, centrality$PageRank,
     main = "Degree vs PageRank",
     xlab = "Degree Centrality",
     ylab = "PageRank Centrality",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(lm(centrality$PageRank ~ centrality$Degree), col = "red", lwd = 2)

par(mfrow = c(1, 1))
dev.off()

cat("✓ Saved: 05_centrality_scatterplots.pdf\n\n")

# ==============================================================================
# 8. NETWORK VISUALIZATION - TOP CANDIDATES (ALL LABELS)
# ==============================================================================

cat("================================================================================\n")
cat("CREATING TOP CANDIDATES NETWORK VISUALIZATION\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "06_network_top_candidates.pdf"), width = 16, height = 12)

# Get top 50 candidates by degree
top_50_candidates <- head(centrality[order(-centrality$Degree), "Candidate_ID"], 50)

# Create subgraph with only top candidates
g_top <- induced_subgraph(g_candidate, V(g_candidate)$name %in% top_50_candidates)

# Layout
set.seed(123)
layout_top <- layout_with_fr(g_top)

# Node sizes by degree
node_size_top <- degree(g_top)
node_size_top <- (node_size_top - min(node_size_top)) / 
                 (max(node_size_top) - min(node_size_top)) * 15 + 5

# Node colors by betweenness
betw_top <- betweenness(g_top)
betw_norm <- (betw_top - min(betw_top)) / (max(betw_top) - min(betw_top))
node_colors_top <- rgb(betw_norm, 0, 1 - betw_norm, 0.8)

plot(g_top,
     layout = layout_top,
     vertex.size = node_size_top,
     vertex.color = node_colors_top,
     vertex.label = V(g_top)$name,
     vertex.label.cex = 0.60,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Top 50 Candidates Network - ALL LABELS\n(Size = Degree, Color = Betweenness)")

legend("topright",
       legend = c("High Betweenness", "Low Betweenness"),
       col = c("red", "blue"),
       pch = 16,
       pt.cex = 2,
       cex = 0.9,
       bg = "white")

dev.off()

cat("✓ Saved: 06_network_top_candidates.pdf\n\n")

# ==============================================================================
# 9. NETWORK VISUALIZATIONS BY CENTRALITY MEASURE (MORE LABELS)
# ==============================================================================

cat("================================================================================\n")
cat("CREATING NETWORK VISUALIZATIONS BY CENTRALITY (MORE LABELS)\n")
cat("================================================================================\n\n")

# Use top 50 candidates for clearer visualization
top_50_for_viz <- head(centrality[order(-centrality$Degree), "Candidate_ID"], 50)
g_viz <- induced_subgraph(g_candidate, V(g_candidate)$name %in% top_50_for_viz)

# Common layout for all plots
set.seed(123)
common_layout <- layout_with_fr(g_viz)

# LABEL THRESHOLD: Show top 50-60% of nodes (changed from 30%)
LABEL_THRESHOLD <- 0.50  # Show top 50% = MORE LABELS!

# --- Plot 1: Degree Centrality ---
pdf(file.path(output_dir, "07_network_degree_centrality.pdf"), width = 14, height = 10)

deg_viz <- degree(g_viz)
deg_norm <- (deg_viz - min(deg_viz)) / (max(deg_viz) - min(deg_viz))
node_size_deg <- deg_norm * 15 + 5
node_colors_deg <- rgb(deg_norm, 0, 1 - deg_norm, 0.8)

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_deg,
     vertex.color = node_colors_deg,
     vertex.label = ifelse(deg_viz > quantile(deg_viz, 1 - LABEL_THRESHOLD), V(g_viz)$name, NA),
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Candidate Network - Degree Centrality\n(Size and Color by Degree, Top 50% Labeled)")

legend("topright",
       legend = c("High Degree", "Medium Degree", "Low Degree"),
       col = c("red", "purple", "blue"),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 07_network_degree_centrality.pdf\n")

# --- Plot 2: Betweenness Centrality ---
pdf(file.path(output_dir, "08_network_betweenness_centrality.pdf"), width = 14, height = 10)

betw_viz <- betweenness(g_viz)
betw_norm <- (betw_viz - min(betw_viz)) / (max(betw_viz) - min(betw_viz))
node_size_betw <- betw_norm * 15 + 5
node_colors_betw <- rgb(betw_norm, 0, 1 - betw_norm, 0.8)

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_betw,
     vertex.color = node_colors_betw,
     vertex.label = ifelse(betw_viz > quantile(betw_viz, 1 - LABEL_THRESHOLD), V(g_viz)$name, NA),
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Candidate Network - Betweenness Centrality\n(Size and Color by Betweenness, Top 50% Labeled)")

legend("topright",
       legend = c("High Betweenness", "Medium Betweenness", "Low Betweenness"),
       col = c("red", "purple", "blue"),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 08_network_betweenness_centrality.pdf\n")

# --- Plot 3: Closeness Centrality ---
pdf(file.path(output_dir, "09_network_closeness_centrality.pdf"), width = 14, height = 10)

clos_viz <- closeness(g_viz, normalized = TRUE)
clos_norm <- (clos_viz - min(clos_viz)) / (max(clos_viz) - min(clos_viz))
node_size_clos <- clos_norm * 15 + 5
node_colors_clos <- rgb(clos_norm, 0, 1 - clos_norm, 0.8)

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_clos,
     vertex.color = node_colors_clos,
     vertex.label = ifelse(clos_viz > quantile(clos_viz, 1 - LABEL_THRESHOLD), V(g_viz)$name, NA),
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Candidate Network - Closeness Centrality\n(Size and Color by Closeness, Top 50% Labeled)")

legend("topright",
       legend = c("High Closeness", "Medium Closeness", "Low Closeness"),
       col = c("red", "purple", "blue"),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 09_network_closeness_centrality.pdf\n")

# --- Plot 4: Eigenvector Centrality ---
pdf(file.path(output_dir, "10_network_eigenvector_centrality.pdf"), width = 14, height = 10)

eigen_viz <- eigen_centrality(g_viz)$vector
eigen_norm <- (eigen_viz - min(eigen_viz)) / (max(eigen_viz) - min(eigen_viz))
node_size_eigen <- eigen_norm * 15 + 5
node_colors_eigen <- rgb(eigen_norm, 0, 1 - eigen_norm, 0.8)

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_eigen,
     vertex.color = node_colors_eigen,
     vertex.label = ifelse(eigen_viz > quantile(eigen_viz, 1 - LABEL_THRESHOLD), V(g_viz)$name, NA),
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Candidate Network - Eigenvector Centrality\n(Size and Color by Eigenvector, Top 50% Labeled)")

legend("topright",
       legend = c("High Eigenvector", "Medium Eigenvector", "Low Eigenvector"),
       col = c("red", "purple", "blue"),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 10_network_eigenvector_centrality.pdf\n")

# --- Plot 5: PageRank Centrality ---
pdf(file.path(output_dir, "11_network_pagerank_centrality.pdf"), width = 14, height = 10)

pr_viz <- page_rank(g_viz)$vector
pr_norm <- (pr_viz - min(pr_viz)) / (max(pr_viz) - min(pr_viz))
node_size_pr <- pr_norm * 15 + 5
node_colors_pr <- rgb(pr_norm, 0, 1 - pr_norm, 0.8)

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_pr,
     vertex.color = node_colors_pr,
     vertex.label = ifelse(pr_viz > quantile(pr_viz, 1 - LABEL_THRESHOLD), V(g_viz)$name, NA),
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Candidate Network - PageRank Centrality\n(Size and Color by PageRank, Top 50% Labeled)")

legend("topright",
       legend = c("High PageRank", "Medium PageRank", "Low PageRank"),
       col = c("red", "purple", "blue"),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 11_network_pagerank_centrality.pdf\n")

# --- Plot 6: Eccentricity ---
pdf(file.path(output_dir, "12_network_eccentricity.pdf"), width = 14, height = 10)

ecc_viz <- eccentricity(g_viz)
if(max(ecc_viz) == min(ecc_viz)) {
  ecc_norm <- rep(0, length(ecc_viz))
} else {
  ecc_norm <- (ecc_viz - min(ecc_viz)) / (max(ecc_viz) - min(ecc_viz))
}

node_size_ecc <- (1 - ecc_norm) * 10 + 3
node_colors_ecc <- rgb(1 - ecc_norm, 0, ecc_norm, 0.8)

# For eccentricity, show labels for lowest values (most central)
label_cutoff <- quantile(ecc_viz, probs = LABEL_THRESHOLD, na.rm = TRUE)
vertex_labels <- ifelse(ecc_viz <= label_cutoff, V(g_viz)$name, NA)

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_ecc,
     vertex.color = node_colors_ecc,
     vertex.label = vertex_labels,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Candidate Network - Eccentricity\n(Size and Color by Eccentricity, Top 50% Labeled)")

legend("topright",
       legend = c("Low Eccentricity (central)", "Medium Eccentricity", "High Eccentricity (peripheral)"),
       col = c("red", "purple", "blue"),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 12_network_eccentricity.pdf\n\n")

# ==============================================================================
# 10. CLUSTERING COEFFICIENT VISUALIZATION
# ==============================================================================

cat("================================================================================\n")
cat("CREATING CLUSTERING COEFFICIENT VISUALIZATION\n")
cat("================================================================================\n\n")

# Calculate local clustering coefficients for full network
local_clustering_full <- transitivity(g_candidate, type = "local")
local_clustering_full[is.nan(local_clustering_full)] <- 0

# Create dataframe for full network
clustering_df_full <- data.frame(
  Candidate_ID = V(g_candidate)$name,
  Clustering = local_clustering_full,
  Degree = degree(g_candidate),
  stringsAsFactors = FALSE
)
clustering_df_full <- clustering_df_full[!is.na(clustering_df_full$Clustering), ]

# Save clustering coefficient data
write.csv(clustering_df_full[order(-clustering_df_full$Clustering), ],
          file.path(output_dir, "clustering_coefficient.csv"),
          row.names = FALSE)
cat("✓ Saved: clustering_coefficient.csv\n")

# --- Clustering Bar Chart and Stats ---
pdf(file.path(output_dir, "13_clustering_coefficient.pdf"), width = 14, height = 10)

par(mfrow = c(2, 2),
    mar = c(10, 5, 5, 2))

# Top 20 by clustering
top_20_clust <- head(clustering_df_full[order(-clustering_df_full$Clustering), ], 20)
barplot(top_20_clust$Clustering,
        names.arg = top_20_clust$Candidate_ID,
        main = "Top 20 Candidates by Clustering Coefficient",
        ylab = "Clustering Coefficient",
        col = "salmon",
        las = 2,
        cex.names = 0.65)

# Histogram
hist(clustering_df_full$Clustering,
     breaks = 40,
     main = "Clustering Coefficient Distribution",
     xlab = "Clustering Coefficient",
     ylab = "Frequency",
     col = "lightsalmon",
     border = "white")
abline(v = mean(clustering_df_full$Clustering), col = "red", lwd = 2, lty = 2)

# Boxplot
boxplot(clustering_df_full$Clustering,
        main = "Clustering Coefficient - Boxplot",
        ylab = "Clustering Coefficient",
        col = "lightsalmon",
        border = "salmon")
points(1, mean(clustering_df_full$Clustering), col = "red", pch = 18, cex = 2)

# Scatter vs Degree
plot(clustering_df_full$Degree, clustering_df_full$Clustering,
     main = "Clustering Coefficient vs Degree",
     xlab = "Degree",
     ylab = "Clustering Coefficient",
     pch = 16,
     col = rgb(0.9, 0.5, 0.4, 0.6))
if(nrow(clustering_df_full) > 1) {
  abline(lm(Clustering ~ Degree, data = clustering_df_full), col = "darkred", lwd = 2)
}

par(mfrow = c(1, 1),
    mar = c(5, 4, 4, 2))

dev.off()
cat("✓ Saved: 13_clustering_coefficient.pdf\n")

# --- Network Visualization by Clustering Coefficient (Top 50) ---
pdf(file.path(output_dir, "14_network_clustering_coefficient.pdf"), width = 14, height = 10)

# Use top 50 candidates subgraph
clust_viz <- transitivity(g_viz, type = "local")
clust_viz[is.nan(clust_viz)] <- 0

# Normalize clustering for sizing and coloring
clust_norm <- (clust_viz - min(clust_viz)) / (max(clust_viz) - min(clust_viz))

# Size by clustering
node_size_clust <- clust_norm * 15 + 5

# Color by clustering
node_colors_clust <- rgb(clust_norm, 0, 1 - clust_norm, 0.8)

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_clust,
     vertex.color = node_colors_clust,
     vertex.label = ifelse(clust_viz > quantile(clust_viz, 1 - LABEL_THRESHOLD), V(g_viz)$name, NA),
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Candidate Network - Clustering Coefficient\n(Size and Color by Clustering, Top 50% Labeled)")

legend("topright",
       legend = c("High Clustering", "Medium Clustering", "Low Clustering"),
       col = c("red", "purple", "blue"),
       pch = 16,
       pt.cex = 2,
       cex = 0.9,
       bg = "white")

dev.off()
cat("✓ Saved: 14_network_clustering_coefficient.pdf\n\n")

# ==============================================================================
# 11. COMMUNITY DETECTION
# ==============================================================================

cat("================================================================================\n")
cat("COMMUNITY DETECTION\n")
cat("================================================================================\n\n")

cat("Running Louvain algorithm...\n")
louvain_comm <- cluster_louvain(g_candidate)

cat(sprintf("Number of communities (Louvain): %d\n", length(louvain_comm)))
cat(sprintf("Modularity: %.4f\n", modularity(louvain_comm)))
cat(sprintf("Top 10 community sizes: %s\n\n", 
            paste(head(sort(sizes(louvain_comm), decreasing = TRUE), 10), collapse = ", ")))

louvain_df <- data.frame(
  Candidate_ID = V(g_candidate)$name,
  Community = membership(louvain_comm),
  stringsAsFactors = FALSE
)

write.csv(louvain_df[order(louvain_df$Community), ],
          file.path(output_dir, "louvain_communities.csv"),
          row.names = FALSE)
cat("✓ Saved: louvain_communities.csv\n\n")

# Visualize top 50 candidates with communities (ALL LABELS)
pdf(file.path(output_dir, "15_louvain_communities_top50.pdf"), width = 16, height = 12)

louvain_viz <- cluster_louvain(g_viz)

num_communities_viz <- length(unique(membership(louvain_viz)))
community_colors_viz <- rainbow(num_communities_viz, alpha = 0.8)
node_colors_comm <- community_colors_viz[membership(louvain_viz)]

node_size_comm <- degree(g_viz)
node_size_comm <- (node_size_comm - min(node_size_comm)) / 
                  (max(node_size_comm) - min(node_size_comm)) * 15 + 5

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_comm,
     vertex.color = node_colors_comm,
     vertex.label = V(g_viz)$name,
     vertex.label.cex = 0.45,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = sprintf("Louvain Communities - Top 50 Candidates - ALL LABELS\n%d communities, Modularity = %.3f",
                   length(louvain_viz), modularity(louvain_viz)))

dev.off()
cat("✓ Saved: 15_louvain_communities_top50.pdf\n\n")

# ==============================================================================
# 12. SUMMARY
# ==============================================================================

cat("================================================================================\n")
cat("VISUALIZATION COMPLETE!\n")
cat("================================================================================\n\n")

cat("FILES SAVED TO:", output_dir, "\n\n")

cat("DISTRIBUTION PLOTS:\n")
cat("  • 01_degree_distribution.pdf (4 plots)\n")
cat("  • 02_centrality_comparison.pdf (grouped bar chart)\n")
cat("  • 03_centrality_individual.pdf (6 plots including Eccentricity)\n")
cat("  • 04_centrality_correlations.pdf (correlation matrix)\n")
cat("  • 05_centrality_scatterplots.pdf (6 scatter plots)\n\n")

cat("NETWORK VISUALIZATIONS WITH MORE LABELS:\n")
cat("  • 06_network_top_candidates.pdf (top 50 with ALL labels)\n")
cat("  • 07_network_degree_centrality.pdf (top 50% labeled = ~25 nodes)\n")
cat("  • 08_network_betweenness_centrality.pdf (top 50% labeled = ~25 nodes)\n")
cat("  • 09_network_closeness_centrality.pdf (top 50% labeled = ~25 nodes)\n")
cat("  • 10_network_eigenvector_centrality.pdf (top 50% labeled = ~25 nodes)\n")
cat("  • 11_network_pagerank_centrality.pdf (top 50% labeled = ~25 nodes)\n")
cat("  • 12_network_eccentricity.pdf (top 50% labeled = ~25 nodes)\n\n")

cat("CLUSTERING & COMMUNITY:\n")
cat("  • 13_clustering_coefficient.pdf (4 plots)\n")
cat("  • 14_network_clustering_coefficient.pdf (top 50% labeled)\n")
cat("  • 15_louvain_communities_top50.pdf (with ALL labels)\n\n")

cat("CSV FILES:\n")
cat("  • clustering_coefficient.csv\n")
cat("  • louvain_communities.csv\n\n")

cat("✓ All visualizations created successfully!\n")
cat("✓ Total: 15 PDFs + 2 CSVs\n")
cat("✓ Network plots now show TOP 50% of nodes labeled (up from 30%)\n")
cat("✓ This means ~25 nodes labeled per plot (out of 50 total)\n")
cat("✓ Files 06 and 15 show ALL 50 labels for maximum detail\n\n")

cat("LABELING STRATEGY:\n")
cat("  - Files 06, 15: ALL 50 nodes labeled\n")
cat("  - Files 07-14: Top 50% labeled (~25 nodes each)\n")
cat("  - Adjustable: Change LABEL_THRESHOLD variable to show more/fewer labels\n\n")

cat("================================================================================\n")