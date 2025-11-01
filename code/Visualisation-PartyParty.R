# ==============================================================================
# Social Network Analysis - Pakistan Elections
# Script 04: Party-Party Network Visualizations
# ==============================================================================
# Authors: Hammad Malik (hm08298) & Mehlab Kashani (mk07950)
# Course: CS/SDP 361/352
# Date: November 1, 2025
# Network Type: Party-Party Unipartite Projection (Weighted)
# ==============================================================================

# --- Load Required Libraries ---
install.packages("igraph", dependencies=TRUE)

library(igraph)

# ==============================================================================
# 1. CONFIGURATION
# ==============================================================================

cat("================================================================================\n")
cat("PARTY-PARTY NETWORK VISUALIZATIONS\n")
cat("================================================================================\n\n")

# Set your data paths here
resultpath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/results"

# Input files
metrics_dir <- file.path(resultpath, "network_metrics_party_party")
network_file <- file.path(metrics_dir, "party_party_network.rds")
centrality_file <- file.path(metrics_dir, "centrality_scores_all.csv")

# Output directory
output_dir <- file.path(resultpath, "visualizations_party_party")

# Create output directory if it doesn't exist
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ==============================================================================
# 2. LOAD DATA
# ==============================================================================

cat("Loading data...\n")

# Load network
g_party <- readRDS(network_file)
cat(sprintf("✓ Loaded party network: %d nodes, %d edges\n", 
            vcount(g_party), ecount(g_party)))

# Load centrality scores
centrality <- read.csv(centrality_file, stringsAsFactors = FALSE)
cat(sprintf("✓ Loaded centrality scores for %d parties\n\n", nrow(centrality)))

# ==============================================================================
# 3. DEGREE DISTRIBUTION PLOTS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING DEGREE DISTRIBUTION PLOTS\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "01_degree_distribution.pdf"), width = 12, height = 8)

# Get degrees
deg <- degree(g_party)

# Layout for 2x2 plots
par(mfrow = c(2, 2))

# --- Plot 1: Histogram ---
hist(deg, 
     breaks = 30,
     main = "Degree Distribution - Party-Party Network",
     xlab = "Degree (Number of Connections)",
     ylab = "Number of Parties",
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

# Get top 15 parties by degree
top_parties <- head(centrality[order(-centrality$Degree), ], 15)

# Normalize centralities to 0-1 scale
top_parties$Degree_norm <- top_parties$Degree / max(centrality$Degree)
top_parties$Betweenness_norm <- top_parties$Betweenness / max(centrality$Betweenness)
top_parties$Closeness_norm <- top_parties$Closeness / max(centrality$Closeness)
top_parties$Eigenvector_norm <- top_parties$Eigenvector / max(centrality$Eigenvector)
top_parties$PageRank_norm <- top_parties$PageRank / max(centrality$PageRank)

# --- Plot 1: Grouped Bar Chart ---
cent_matrix <- as.matrix(top_parties[, c("Degree_norm", "Betweenness_norm", 
                                          "Closeness_norm", "Eigenvector_norm", 
                                          "PageRank_norm")])
rownames(cent_matrix) <- top_parties$Party

barplot(t(cent_matrix),
        beside = TRUE,
        main = "Normalized Centrality Measures - Top 15 Parties",
        xlab = "",
        ylab = "Normalized Centrality (0-1)",
        col = c("steelblue", "coral", "gold", "lightgreen", "purple"),
        legend.text = c("Degree", "Betweenness", "Closeness", "Eigenvector", "PageRank"),
        args.legend = list(x = "topright", cex = 0.9),
        las = 2,
        cex.names = 0.8)

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
        names.arg = top_deg$Party,
        main = "Top 20 Parties by\nDegree Centrality",
        ylab = "Degree",
        col = "steelblue",
        las = 2,
        cex.names = 0.65,
        cex.main = 1.0)

# --- Betweenness Centrality ---
top_betw <- head(centrality[order(-centrality$Betweenness), ], 20)
barplot(top_betw$Betweenness,
        names.arg = top_betw$Party,
        main = "Top 20 Parties by\nBetweenness Centrality",
        ylab = "Betweenness",
        col = "coral",
        las = 2,
        cex.names = 0.65,
        cex.main = 1.0)

# --- Closeness Centrality ---
top_clos <- head(centrality[order(-centrality$Closeness), ], 20)
barplot(top_clos$Closeness,
        names.arg = top_clos$Party,
        main = "Top 20 Parties by\nCloseness Centrality",
        ylab = "Closeness",
        col = "gold",
        las = 2,
        cex.names = 0.65,
        cex.main = 1.0)

# --- Eigenvector Centrality ---
top_eigen <- head(centrality[order(-centrality$Eigenvector), ], 20)
barplot(top_eigen$Eigenvector,
        names.arg = top_eigen$Party,
        main = "Top 20 Parties by\nEigenvector Centrality",
        ylab = "Eigenvector",
        col = "lightgreen",
        las = 2,
        cex.names = 0.65,
        cex.main = 1.0)

# --- PageRank Centrality ---
top_pr <- head(centrality[order(-centrality$PageRank), ], 20)
barplot(top_pr$PageRank,
        names.arg = top_pr$Party,
        main = "Top 20 Parties by\nPageRank Centrality",
        ylab = "PageRank",
        col = "purple",
        las = 2,
        cex.names = 0.65,
        cex.main = 1.0)

# --- Combined Histogram ---
hist(centrality$Degree,
     breaks = 30,
     main = "Degree Distribution\nAll Parties",
     xlab = "Degree",
     ylab = "Frequency",
     col = "lightblue",
     border = "white",
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

# Add color scale legend
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
# 9. NETWORK VISUALIZATION - TOP PARTIES
# ==============================================================================

cat("================================================================================\n")
cat("CREATING TOP PARTIES NETWORK VISUALIZATION\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "07_network_top_parties.pdf"), width = 16, height = 12)

# Get top 50 parties by degree
top_50_parties <- head(centrality[order(-centrality$Degree), "Party"], 50)

# Create subgraph with only top parties
g_top <- induced_subgraph(g_party, V(g_party)$name %in% top_50_parties)

# Layout
set.seed(123)
layout_top <- layout_with_fr(g_top)

# Node sizes by degree
node_size_top <- degree(g_top)
node_size_top <- (node_size_top - min(node_size_top)) / 
                 (max(node_size_top) - min(node_size_top)) * 15 + 3

# Node colors by betweenness
betw_top <- betweenness(g_top)
betw_norm <- (betw_top - min(betw_top)) / (max(betw_top) - min(betw_top))
node_colors_top <- rgb(betw_norm, 0, 1 - betw_norm, 0.8)

plot(g_top,
     layout = layout_top,
     vertex.size = node_size_top,
     vertex.color = node_colors_top,
     vertex.label = V(g_top)$name,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.5,
     edge.color = rgb(0, 0, 0, 0.2),
     main = "Top 50 Parties Network\n(Size = Degree, Color = Betweenness)")

legend("topright",
       legend = c("High Betweenness", "Low Betweenness"),
       col = c("red", "blue"),
       pch = 16,
       pt.cex = 2,
       cex = 0.9,
       bg = "white")

dev.off()

cat("✓ Saved: 07_network_top_parties.pdf\n\n")

# ==============================================================================
# 10. NETWORK VISUALIZATIONS BY CENTRALITY MEASURE
# ==============================================================================

cat("================================================================================\n")
cat("CREATING NETWORK VISUALIZATIONS BY CENTRALITY\n")
cat("================================================================================\n\n")

# Use top 50 parties for clearer visualization
top_50_parties <- head(centrality[order(-centrality$Degree), "Party"], 50)
g_viz <- induced_subgraph(g_party, V(g_party)$name %in% top_50_parties)

# Common layout for all plots
set.seed(123)
common_layout <- layout_with_graphopt(g_viz, charge = 0.01)

# --- Plot 1: Degree Centrality ---
pdf(file.path(output_dir, "08_network_degree_centrality.pdf"), width = 14, height = 10)

deg_viz <- degree(g_viz)
deg_norm <- (deg_viz - min(deg_viz)) / (max(deg_viz) - min(deg_viz))
node_size_deg <- deg_norm * 15 + 3
node_colors_deg <- rgb(deg_norm, 0, 1 - deg_norm, 0.8)

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_deg,
     vertex.color = node_colors_deg,
     vertex.label = ifelse(deg_viz > quantile(deg_viz, 0.70), V(g_viz)$name, NA),
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Party Network - Degree Centrality\n(Size and Color by Degree)")

legend("topright",
       legend = c("High Degree", "Medium Degree", "Low Degree"),
       col = c("red", "purple", "blue"),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 08_network_degree_centrality.pdf\n")

# --- Plot 2: Betweenness Centrality ---
pdf(file.path(output_dir, "09_network_betweenness_centrality.pdf"), width = 14, height = 10)

betw_viz <- betweenness(g_viz)
betw_norm <- (betw_viz - min(betw_viz)) / (max(betw_viz) - min(betw_viz))
node_size_betw <- betw_norm * 15 + 3
node_colors_betw <- rgb(betw_norm, 0, 1 - betw_norm, 0.8)

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_betw,
     vertex.color = node_colors_betw,
     vertex.label = ifelse(betw_viz > quantile(betw_viz, 0.70), V(g_viz)$name, NA),
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Party Network - Betweenness Centrality\n(Size and Color by Betweenness)")

legend("topright",
       legend = c("High Betweenness", "Medium Betweenness", "Low Betweenness"),
       col = c("red", "purple", "blue"),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 09_network_betweenness_centrality.pdf\n")

# --- Plot 3: Closeness Centrality ---
pdf(file.path(output_dir, "10_network_closeness_centrality.pdf"), width = 14, height = 10)

clos_viz <- closeness(g_viz, normalized = TRUE)
clos_norm <- (clos_viz - min(clos_viz)) / (max(clos_viz) - min(clos_viz))
node_size_clos <- clos_norm * 15 + 3
node_colors_clos <- rgb(clos_norm, 0, 1 - clos_norm, 0.8)

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_clos,
     vertex.color = node_colors_clos,
     vertex.label = ifelse(clos_viz > quantile(clos_viz, 0.70), V(g_viz)$name, NA),
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Party Network - Closeness Centrality\n(Size and Color by Closeness)")

legend("topright",
       legend = c("High Closeness", "Medium Closeness", "Low Closeness"),
       col = c("red", "purple", "blue"),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 10_network_closeness_centrality.pdf\n")

# --- Plot 4: Eigenvector Centrality ---
pdf(file.path(output_dir, "11_network_eigenvector_centrality.pdf"), width = 14, height = 10)

eigen_viz <- eigen_centrality(g_viz)$vector
eigen_norm <- (eigen_viz - min(eigen_viz)) / (max(eigen_viz) - min(eigen_viz))
node_size_eigen <- eigen_norm * 15 + 3
node_colors_eigen <- rgb(eigen_norm, 0, 1 - eigen_norm, 0.8)

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_eigen,
     vertex.color = node_colors_eigen,
     vertex.label = ifelse(eigen_viz > quantile(eigen_viz, 0.70), V(g_viz)$name, NA),
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Party Network - Eigenvector Centrality\n(Size and Color by Eigenvector)")

legend("topright",
       legend = c("High Eigenvector", "Medium Eigenvector", "Low Eigenvector"),
       col = c("red", "purple", "blue"),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 11_network_eigenvector_centrality.pdf\n")

# --- Plot 5: PageRank Centrality ---
pdf(file.path(output_dir, "12_network_pagerank_centrality.pdf"), width = 14, height = 10)

pr_viz <- page_rank(g_viz)$vector
pr_norm <- (pr_viz - min(pr_viz)) / (max(pr_viz) - min(pr_viz))
node_size_pr <- pr_norm * 15 + 3
node_colors_pr <- rgb(pr_norm, 0, 1 - pr_norm, 0.8)

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_pr,
     vertex.color = node_colors_pr,
     vertex.label = ifelse(pr_viz > quantile(pr_viz, 0.70), V(g_viz)$name, NA),
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Party Network - PageRank Centrality\n(Size and Color by PageRank)")

legend("topright",
       legend = c("High PageRank", "Medium PageRank", "Low PageRank"),
       col = c("red", "purple", "blue"),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 12_network_pagerank_centrality.pdf\n")

# --- Plot 6: Eccentricity ---
pdf(file.path(output_dir, "13_network_eccentricity.pdf"), width = 14, height = 10)

ecc_viz <- eccentricity(g_viz)
if(max(ecc_viz) == min(ecc_viz)) {
  ecc_norm <- rep(0, length(ecc_viz))
} else {
  ecc_norm <- (ecc_viz - min(ecc_viz)) / (max(ecc_viz) - min(ecc_viz))
}

node_size_ecc <- (1 - ecc_norm) * 8 + 2
node_colors_ecc <- rgb(1 - ecc_norm, 0, ecc_norm, 0.8)

label_threshold <- 0.30
label_cutoff <- quantile(ecc_viz, probs = label_threshold, na.rm = TRUE)
vertex_labels <- ifelse(ecc_viz <= label_cutoff, V(g_viz)$name, NA)

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_ecc,
     vertex.color = node_colors_ecc,
     vertex.label = vertex_labels,
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Party Network - Eccentricity\n(Size and Color by Eccentricity)")

legend("topright",
       legend = c("Low Eccentricity (central)", "Medium Eccentricity", "High Eccentricity (peripheral)"),
       col = c("red", "purple", "blue"),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 13_network_eccentricity.pdf\n\n")

# ==============================================================================
# 11. CLUSTERING COEFFICIENT VISUALIZATION
# ==============================================================================

cat("================================================================================\n")
cat("CREATING CLUSTERING COEFFICIENT VISUALIZATION\n")
cat("================================================================================\n\n")

# Calculate local clustering coefficients for full network
local_clustering_full <- transitivity(g_party, type = "local")
local_clustering_full[is.nan(local_clustering_full)] <- 0

# Create dataframe for full network
clustering_df_full <- data.frame(
  Party = V(g_party)$name,
  Clustering = local_clustering_full,
  Degree = degree(g_party),
  stringsAsFactors = FALSE
)
clustering_df_full <- clustering_df_full[!is.na(clustering_df_full$Clustering), ]

# Save clustering coefficient data
write.csv(clustering_df_full[order(-clustering_df_full$Clustering), ],
          file.path(output_dir, "clustering_coefficient.csv"),
          row.names = FALSE)
cat("✓ Saved: clustering_coefficient.csv\n")

# --- Network Visualization by Clustering Coefficient (Top 50) ---
pdf(file.path(output_dir, "14_network_clustering_coefficient.pdf"), width = 14, height = 10)

# Use top 50 parties subgraph
top_50_parties_clust <- head(centrality[order(-centrality$Degree), "Party"], 50)
g_clust_viz <- induced_subgraph(g_party, V(g_party)$name %in% top_50_parties_clust)

# Calculate clustering for this subgraph
clust_viz <- transitivity(g_clust_viz, type = "local")
clust_viz[is.nan(clust_viz)] <- 0

# Normalize clustering for sizing and coloring
clust_norm <- (clust_viz - min(clust_viz)) / (max(clust_viz) - min(clust_viz))

# Size by clustering
node_size_clust <- clust_norm * 15 + 3

# Color by clustering
node_colors_clust <- rgb(clust_norm, 0, 1 - clust_norm, 0.8)

# Use same layout as other centrality plots
set.seed(123)
layout_clust <- layout_with_graphopt(g_clust_viz, charge = 0.01)

plot(g_clust_viz,
     layout = layout_clust,
     vertex.size = node_size_clust,
     vertex.color = node_colors_clust,
     vertex.label = ifelse(clust_viz > quantile(clust_viz, 0.70), V(g_clust_viz)$name, NA),
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Party Network - Clustering Coefficient\n(Size and Color by Clustering)")

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
# 12. COMMUNITY DETECTION - LOUVAIN ALGORITHM
# ==============================================================================

cat("================================================================================\n")
cat("COMMUNITY DETECTION - LOUVAIN ALGORITHM\n")
cat("================================================================================\n\n")

louvain_comm <- cluster_louvain(g_party)

cat(sprintf("Number of communities (Louvain): %d\n", length(louvain_comm)))
cat(sprintf("Modularity: %.4f\n", modularity(louvain_comm)))
cat(sprintf("Sizes: %s\n\n", paste(sizes(louvain_comm), collapse = ", ")))

louvain_df <- data.frame(
  Party = V(g_party)$name,
  Community = membership(louvain_comm),
  stringsAsFactors = FALSE
)

write.csv(louvain_df[order(louvain_df$Community), ],
          file.path(output_dir, "louvain_communities.csv"),
          row.names = FALSE)
cat("✓ Saved: louvain_communities.csv\n")

# Visualize Louvain communities - Top 50 Parties ONLY
pdf(file.path(output_dir, "15_louvain_communities_top50.pdf"), width = 16, height = 12)

top_50_parties_comm <- head(centrality[order(-centrality$Degree), "Party"], 50)
g_top_comm <- induced_subgraph(g_party, V(g_party)$name %in% top_50_parties_comm)
louvain_top <- cluster_louvain(g_top_comm)

set.seed(123)
layout_top_comm <- layout_with_fr(g_top_comm)

num_communities_top <- length(unique(membership(louvain_top)))
community_colors_top <- rainbow(num_communities_top, alpha = 0.8)
node_colors_top_comm <- community_colors_top[membership(louvain_top)]

node_size_top_comm <- degree(g_top_comm)
node_size_top_comm <- (node_size_top_comm - min(node_size_top_comm)) / 
                      (max(node_size_top_comm) - min(node_size_top_comm)) * 15 + 3

plot(g_top_comm,
     layout = layout_top_comm,
     vertex.size = node_size_top_comm,
     vertex.color = node_colors_top_comm,
     vertex.label = V(g_top_comm)$name,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.5,
     edge.color = rgb(0, 0, 0, 0.2),
     main = sprintf("Louvain Communities - Top 50 Parties\n%d communities, Modularity = %.3f",
                   length(louvain_top), modularity(louvain_top)))

dev.off()
cat("✓ Saved: 15_louvain_communities_top50.pdf\n\n")

# ==============================================================================
# 13. COMMUNITY DETECTION - WALKTRAP ALGORITHM
# ==============================================================================

cat("================================================================================\n")
cat("COMMUNITY DETECTION - WALKTRAP ALGORITHM\n")
cat("================================================================================\n\n")

walktrap_comm <- cluster_walktrap(g_party)

cat(sprintf("Number of communities (Walktrap): %d\n", length(walktrap_comm)))
cat(sprintf("Modularity: %.4f\n", modularity(walktrap_comm)))
cat(sprintf("Sizes: %s\n\n", paste(sizes(walktrap_comm), collapse = ", ")))

walktrap_df <- data.frame(
  Party = V(g_party)$name,
  Community = membership(walktrap_comm),
  stringsAsFactors = FALSE
)

write.csv(walktrap_df[order(walktrap_df$Community), ],
          file.path(output_dir, "walktrap_communities.csv"),
          row.names = FALSE)
cat("✓ Saved: walktrap_communities.csv\n")

# Visualize Walktrap communities - Top 50 Parties ONLY
pdf(file.path(output_dir, "16_walktrap_communities_top50.pdf"), width = 16, height = 12)

walktrap_top <- cluster_walktrap(g_top_comm)

set.seed(123)
layout_top_wt <- layout_with_fr(g_top_comm)

num_communities_wt_top <- length(unique(membership(walktrap_top)))
community_colors_wt_top <- rainbow(num_communities_wt_top, alpha = 0.8)
node_colors_top_wt <- community_colors_wt_top[membership(walktrap_top)]

plot(g_top_comm,
     layout = layout_top_wt,
     vertex.size = node_size_top_comm,
     vertex.color = node_colors_top_wt,
     vertex.label = V(g_top_comm)$name,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.5,
     edge.color = rgb(0, 0, 0, 0.2),
     main = sprintf("Walktrap Communities - Top 50 Parties\n%d communities, Modularity = %.3f",
                   length(walktrap_top), modularity(walktrap_top)))

dev.off()
cat("✓ Saved: 16_walktrap_communities_top50.pdf\n\n")

# ==============================================================================
# 14. COMMUNITY COMPARISON
# ==============================================================================

cat("================================================================================\n")
cat("CREATING COMMUNITY COMPARISON PLOT\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "17_community_comparison.pdf"), width = 14, height = 8)

par(mfrow = c(1, 2))

louvain_sizes <- sort(sizes(louvain_comm), decreasing = TRUE)
barplot(louvain_sizes,
        main = sprintf("Louvain Community Sizes\n%d communities, Modularity = %.3f",
                      length(louvain_comm), modularity(louvain_comm)),
        xlab = "Community",
        ylab = "Number of Parties",
        col = "steelblue",
        border = "white")

walktrap_sizes <- sort(sizes(walktrap_comm), decreasing = TRUE)
barplot(walktrap_sizes,
        main = sprintf("Walktrap Community Sizes\n%d communities, Modularity = %.3f",
                      length(walktrap_comm), modularity(walktrap_comm)),
        xlab = "Community",
        ylab = "Number of Parties",
        col = "coral",
        border = "white")

par(mfrow = c(1, 1))
dev.off()

cat("✓ Saved: 19_community_comparison.pdf\n")

community_summary <- data.frame(
  Algorithm = c("Louvain", "Walktrap"),
  Number_of_Communities = c(length(louvain_comm), length(walktrap_comm)),
  Modularity = c(modularity(louvain_comm), modularity(walktrap_comm)),
  Largest_Community = c(max(sizes(louvain_comm)), max(sizes(walktrap_comm))),
  Smallest_Community = c(min(sizes(louvain_comm)), min(sizes(walktrap_comm))),
  stringsAsFactors = FALSE
)

write.csv(community_summary,
          file.path(output_dir, "community_summary.csv"),
          row.names = FALSE)
cat("✓ Saved: community_summary.csv\n\n")

# ==============================================================================
# 15. SUMMARY
# ==============================================================================

cat("================================================================================\n")
cat("VISUALIZATION COMPLETE!\n")
cat("================================================================================\n\n")

cat("FILES SAVED TO:", output_dir, "\n")
cat("  • 01_degree_distribution.pdf (4 plots)\n")
cat("  • 02_centrality_comparison.pdf (grouped bar chart)\n")
cat("  • 03_centrality_individual.pdf (6 plots)\n")
cat("  • 04_centrality_correlations.pdf (correlation matrix)\n")
cat("  • 05_centrality_scatterplots.pdf (6 scatter plots)\n")
cat("  • 07_network_top_parties.pdf (top 50 parties)\n")
cat("  • 08_network_degree_centrality.pdf\n")
cat("  • 09_network_betweenness_centrality.pdf\n")
cat("  • 10_network_closeness_centrality.pdf\n")
cat("  • 11_network_eigenvector_centrality.pdf\n")
cat("  • 12_network_pagerank_centrality.pdf\n")
cat("  • 13_network_eccentricity.pdf\n")
cat("  • 14_network_clustering_coefficient.pdf\n")
cat("  • 15_louvain_communities_top50.pdf\n")
cat("  • 16_walktrap_communities_top50.pdf\n")
cat("  • 17_community_comparison.pdf (2 plots)\n\n")

cat("CSV FILES:\n")
cat("  • clustering_coefficient.csv\n")
cat("  • louvain_communities.csv\n")
cat("  • walktrap_communities.csv\n")
cat("  • community_summary.csv\n\n")

cat("✓ All visualizations created successfully!\n")
cat("✓ Total: 17 PDFs + 4 CSVs\n\n")

cat("================================================================================\n")