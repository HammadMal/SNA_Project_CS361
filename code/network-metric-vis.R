# ==============================================================================
# Social Network Analysis - Pakistan Elections
# Script 05: Network Metrics Visualizations
# ==============================================================================
# Authors: Hammad Malik (hm08298) & Mehlab Kashani (mk07950)
# Course: CS/SDP 361/352
# Date: November 1, 2025
# Purpose: Clear visualizations for network metrics with no overlapping labels
# ==============================================================================

# --- Load Required Libraries ---
library(igraph)

# ==============================================================================
# 1. CONFIGURATION
# ==============================================================================

cat("================================================================================\n")
cat("NETWORK METRICS VISUALIZATIONS\n")
cat("================================================================================\n\n")

# Set your data paths here
resultpath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/results"

# Input files
metrics_dir <- file.path(resultpath, "network_metrics_party_party")
network_file <- file.path(metrics_dir, "party_party_network.rds")
centrality_file <- file.path(metrics_dir, "centrality_scores_all.csv")

# Output directory
output_dir <- file.path(resultpath, "network_metrics_visualizations")

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
# 3. DEGREE CENTRALITY VISUALIZATIONS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING DEGREE CENTRALITY VISUALIZATIONS\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "01_degree_centrality_plots.pdf"), width = 16, height = 12)

# Set up layout with extra spacing
par(mfrow = c(2, 2),
    mar = c(12, 5, 5, 2),   # Bottom, Left, Top, Right margins
    oma = c(2, 0, 2, 0))    # Outer margins

# --- Plot 1: Top 30 Parties ---
top_30_deg <- head(centrality[order(-centrality$Degree), ], 30)

barplot(top_30_deg$Degree,
        names.arg = top_30_deg$Party,
        main = "Top 30 Parties by Degree Centrality",
        ylab = "Degree (Number of Connections)",
        col = "steelblue",
        las = 2,
        cex.names = 0.7,
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        border = NA)
grid(nx = NA, ny = NULL)

# --- Plot 2: Histogram ---
hist(centrality$Degree,
     breaks = 40,
     main = "Degree Distribution",
     xlab = "Degree",
     ylab = "Frequency (Number of Parties)",
     col = "lightblue",
     border = "white",
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
abline(v = mean(centrality$Degree), col = "red", lwd = 3, lty = 2)
legend("topright",
       legend = c(sprintf("Mean = %.2f", mean(centrality$Degree)),
                 sprintf("Median = %.2f", median(centrality$Degree))),
       col = c("red", "black"),
       lty = c(2, 0),
       lwd = c(3, 0),
       cex = 1.1,
       bg = "white")

# --- Plot 3: Boxplot ---
boxplot(centrality$Degree,
        main = "Degree Centrality - Boxplot",
        ylab = "Degree",
        col = "lightblue",
        border = "steelblue",
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        lwd = 2)
points(1, mean(centrality$Degree), col = "red", pch = 18, cex = 2.5)
text(1.3, mean(centrality$Degree), 
     sprintf("Mean = %.2f", mean(centrality$Degree)),
     cex = 1.1)

# --- Plot 4: Cumulative Distribution ---
deg_sorted <- sort(centrality$Degree, decreasing = TRUE)
plot(1:length(deg_sorted), deg_sorted,
     type = "l",
     main = "Cumulative Degree Distribution",
     xlab = "Rank",
     ylab = "Degree",
     col = "steelblue",
     lwd = 3,
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
grid()

par(mfrow = c(1, 1),
    mar = c(5, 4, 4, 2),
    oma = c(0, 0, 0, 0))

dev.off()
cat("✓ Saved: 01_degree_centrality_plots.pdf\n\n")

# ==============================================================================
# 4. BETWEENNESS CENTRALITY VISUALIZATIONS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING BETWEENNESS CENTRALITY VISUALIZATIONS\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "02_betweenness_centrality_plots.pdf"), width = 16, height = 12)

par(mfrow = c(2, 2),
    mar = c(12, 5, 5, 2),
    oma = c(2, 0, 2, 0))

# --- Plot 1: Top 30 Parties ---
top_30_betw <- head(centrality[order(-centrality$Betweenness), ], 30)

barplot(top_30_betw$Betweenness,
        names.arg = top_30_betw$Party,
        main = "Top 30 Parties by Betweenness Centrality",
        ylab = "Betweenness",
        col = "coral",
        las = 2,
        cex.names = 0.7,
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        border = NA)
grid(nx = NA, ny = NULL)

# --- Plot 2: Histogram ---
hist(centrality$Betweenness,
     breaks = 40,
     main = "Betweenness Distribution",
     xlab = "Betweenness",
     ylab = "Frequency (Number of Parties)",
     col = "lightsalmon",
     border = "white",
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
abline(v = mean(centrality$Betweenness), col = "red", lwd = 3, lty = 2)
legend("topright",
       legend = c(sprintf("Mean = %.2f", mean(centrality$Betweenness)),
                 sprintf("Median = %.2f", median(centrality$Betweenness))),
       col = c("red", "black"),
       lty = c(2, 0),
       lwd = c(3, 0),
       cex = 1.1,
       bg = "white")

# --- Plot 3: Boxplot ---
boxplot(centrality$Betweenness,
        main = "Betweenness Centrality - Boxplot",
        ylab = "Betweenness",
        col = "lightsalmon",
        border = "coral",
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        lwd = 2)
points(1, mean(centrality$Betweenness), col = "red", pch = 18, cex = 2.5)
text(1.3, mean(centrality$Betweenness),
     sprintf("Mean = %.2f", mean(centrality$Betweenness)),
     cex = 1.1)

# --- Plot 4: Scatter vs Degree ---
plot(centrality$Degree, centrality$Betweenness,
     main = "Betweenness vs Degree",
     xlab = "Degree Centrality",
     ylab = "Betweenness Centrality",
     pch = 16,
     col = rgb(0.8, 0.3, 0.3, 0.6),
     cex = 1.2,
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
abline(lm(centrality$Betweenness ~ centrality$Degree), col = "darkred", lwd = 3)
grid()

par(mfrow = c(1, 1),
    mar = c(5, 4, 4, 2),
    oma = c(0, 0, 0, 0))

dev.off()
cat("✓ Saved: 02_betweenness_centrality_plots.pdf\n\n")

# ==============================================================================
# 5. CLOSENESS CENTRALITY VISUALIZATIONS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING CLOSENESS CENTRALITY VISUALIZATIONS\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "03_closeness_centrality_plots.pdf"), width = 16, height = 12)

par(mfrow = c(2, 2),
    mar = c(12, 5, 5, 2),
    oma = c(2, 0, 2, 0))

# --- Plot 1: Top 30 Parties ---
top_30_clos <- head(centrality[order(-centrality$Closeness), ], 30)

barplot(top_30_clos$Closeness,
        names.arg = top_30_clos$Party,
        main = "Top 30 Parties by Closeness Centrality",
        ylab = "Closeness",
        col = "gold",
        las = 2,
        cex.names = 0.7,
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        border = NA)
grid(nx = NA, ny = NULL)

# --- Plot 2: Histogram ---
hist(centrality$Closeness,
     breaks = 40,
     main = "Closeness Distribution",
     xlab = "Closeness",
     ylab = "Frequency (Number of Parties)",
     col = "lightyellow",
     border = "white",
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
abline(v = mean(centrality$Closeness), col = "red", lwd = 3, lty = 2)
legend("topright",
       legend = c(sprintf("Mean = %.4f", mean(centrality$Closeness)),
                 sprintf("Median = %.4f", median(centrality$Closeness))),
       col = c("red", "black"),
       lty = c(2, 0),
       lwd = c(3, 0),
       cex = 1.1,
       bg = "white")

# --- Plot 3: Boxplot ---
boxplot(centrality$Closeness,
        main = "Closeness Centrality - Boxplot",
        ylab = "Closeness",
        col = "lightyellow",
        border = "gold",
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        lwd = 2)
points(1, mean(centrality$Closeness), col = "red", pch = 18, cex = 2.5)
text(1.3, mean(centrality$Closeness),
     sprintf("Mean = %.4f", mean(centrality$Closeness)),
     cex = 1.1)

# --- Plot 4: Scatter vs Degree ---
plot(centrality$Degree, centrality$Closeness,
     main = "Closeness vs Degree",
     xlab = "Degree Centrality",
     ylab = "Closeness Centrality",
     pch = 16,
     col = rgb(0.8, 0.8, 0.2, 0.6),
     cex = 1.2,
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
abline(lm(centrality$Closeness ~ centrality$Degree), col = "darkgoldenrod", lwd = 3)
grid()

par(mfrow = c(1, 1),
    mar = c(5, 4, 4, 2),
    oma = c(0, 0, 0, 0))

dev.off()
cat("✓ Saved: 03_closeness_centrality_plots.pdf\n\n")

# ==============================================================================
# 6. EIGENVECTOR CENTRALITY VISUALIZATIONS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING EIGENVECTOR CENTRALITY VISUALIZATIONS\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "04_eigenvector_centrality_plots.pdf"), width = 16, height = 12)

par(mfrow = c(2, 2),
    mar = c(12, 5, 5, 2),
    oma = c(2, 0, 2, 0))

# --- Plot 1: Top 30 Parties ---
top_30_eigen <- head(centrality[order(-centrality$Eigenvector), ], 30)

barplot(top_30_eigen$Eigenvector,
        names.arg = top_30_eigen$Party,
        main = "Top 30 Parties by Eigenvector Centrality",
        ylab = "Eigenvector",
        col = "lightgreen",
        las = 2,
        cex.names = 0.7,
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        border = NA)
grid(nx = NA, ny = NULL)

# --- Plot 2: Histogram ---
hist(centrality$Eigenvector,
     breaks = 40,
     main = "Eigenvector Distribution",
     xlab = "Eigenvector",
     ylab = "Frequency (Number of Parties)",
     col = "lightgreen",
     border = "white",
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
abline(v = mean(centrality$Eigenvector), col = "red", lwd = 3, lty = 2)
legend("topright",
       legend = c(sprintf("Mean = %.4f", mean(centrality$Eigenvector)),
                 sprintf("Median = %.4f", median(centrality$Eigenvector))),
       col = c("red", "black"),
       lty = c(2, 0),
       lwd = c(3, 0),
       cex = 1.1,
       bg = "white")

# --- Plot 3: Boxplot ---
boxplot(centrality$Eigenvector,
        main = "Eigenvector Centrality - Boxplot",
        ylab = "Eigenvector",
        col = "lightgreen",
        border = "darkgreen",
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        lwd = 2)
points(1, mean(centrality$Eigenvector), col = "red", pch = 18, cex = 2.5)
text(1.3, mean(centrality$Eigenvector),
     sprintf("Mean = %.4f", mean(centrality$Eigenvector)),
     cex = 1.1)

# --- Plot 4: Scatter vs Degree ---
plot(centrality$Degree, centrality$Eigenvector,
     main = "Eigenvector vs Degree",
     xlab = "Degree Centrality",
     ylab = "Eigenvector Centrality",
     pch = 16,
     col = rgb(0.3, 0.8, 0.3, 0.6),
     cex = 1.2,
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
abline(lm(centrality$Eigenvector ~ centrality$Degree), col = "darkgreen", lwd = 3)
grid()

par(mfrow = c(1, 1),
    mar = c(5, 4, 4, 2),
    oma = c(0, 0, 0, 0))

dev.off()
cat("✓ Saved: 04_eigenvector_centrality_plots.pdf\n\n")

# ==============================================================================
# 7. PAGERANK CENTRALITY VISUALIZATIONS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING PAGERANK CENTRALITY VISUALIZATIONS\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "05_pagerank_centrality_plots.pdf"), width = 16, height = 12)

par(mfrow = c(2, 2),
    mar = c(12, 5, 5, 2),
    oma = c(2, 0, 2, 0))

# --- Plot 1: Top 30 Parties ---
top_30_pr <- head(centrality[order(-centrality$PageRank), ], 30)

barplot(top_30_pr$PageRank,
        names.arg = top_30_pr$Party,
        main = "Top 30 Parties by PageRank Centrality",
        ylab = "PageRank",
        col = "mediumpurple",
        las = 2,
        cex.names = 0.7,
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        border = NA)
grid(nx = NA, ny = NULL)

# --- Plot 2: Histogram ---
hist(centrality$PageRank,
     breaks = 40,
     main = "PageRank Distribution",
     xlab = "PageRank",
     ylab = "Frequency (Number of Parties)",
     col = "lavender",
     border = "white",
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
abline(v = mean(centrality$PageRank), col = "red", lwd = 3, lty = 2)
legend("topright",
       legend = c(sprintf("Mean = %.4f", mean(centrality$PageRank)),
                 sprintf("Median = %.4f", median(centrality$PageRank))),
       col = c("red", "black"),
       lty = c(2, 0),
       lwd = c(3, 0),
       cex = 1.1,
       bg = "white")

# --- Plot 3: Boxplot ---
boxplot(centrality$PageRank,
        main = "PageRank Centrality - Boxplot",
        ylab = "PageRank",
        col = "lavender",
        border = "mediumpurple",
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        lwd = 2)
points(1, mean(centrality$PageRank), col = "red", pch = 18, cex = 2.5)
text(1.3, mean(centrality$PageRank),
     sprintf("Mean = %.4f", mean(centrality$PageRank)),
     cex = 1.1)

# --- Plot 4: Scatter vs Degree ---
plot(centrality$Degree, centrality$PageRank,
     main = "PageRank vs Degree",
     xlab = "Degree Centrality",
     ylab = "PageRank Centrality",
     pch = 16,
     col = rgb(0.6, 0.3, 0.8, 0.6),
     cex = 1.2,
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
abline(lm(centrality$PageRank ~ centrality$Degree), col = "purple", lwd = 3)
grid()

par(mfrow = c(1, 1),
    mar = c(5, 4, 4, 2),
    oma = c(0, 0, 0, 0))

dev.off()
cat("✓ Saved: 05_pagerank_centrality_plots.pdf\n\n")

# ==============================================================================
# 8. ECCENTRICITY VISUALIZATIONS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING ECCENTRICITY VISUALIZATIONS\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "06_eccentricity_plots.pdf"), width = 16, height = 12)

par(mfrow = c(2, 2),
    mar = c(12, 5, 5, 2),
    oma = c(2, 0, 2, 0))

# --- Plot 1: Top 30 Parties (Lowest Eccentricity = Most Central) ---
top_30_ecc <- head(centrality[order(centrality$Eccentricity), ], 30)

barplot(top_30_ecc$Eccentricity,
        names.arg = top_30_ecc$Party,
        main = "Top 30 Parties by Eccentricity (Lowest = Most Central)",
        ylab = "Eccentricity",
        col = "skyblue",
        las = 2,
        cex.names = 0.7,
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        border = NA)
grid(nx = NA, ny = NULL)

# --- Plot 2: Histogram ---
hist(centrality$Eccentricity,
     breaks = 20,
     main = "Eccentricity Distribution",
     xlab = "Eccentricity",
     ylab = "Frequency (Number of Parties)",
     col = "lightblue",
     border = "white",
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
abline(v = mean(centrality$Eccentricity), col = "red", lwd = 3, lty = 2)
legend("topright",
       legend = c(sprintf("Mean = %.2f", mean(centrality$Eccentricity)),
                 sprintf("Median = %.2f", median(centrality$Eccentricity))),
       col = c("red", "black"),
       lty = c(2, 0),
       lwd = c(3, 0),
       cex = 1.1,
       bg = "white")

# --- Plot 3: Boxplot ---
boxplot(centrality$Eccentricity,
        main = "Eccentricity - Boxplot",
        ylab = "Eccentricity",
        col = "lightblue",
        border = "skyblue",
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        lwd = 2)
points(1, mean(centrality$Eccentricity), col = "red", pch = 18, cex = 2.5)
text(1.3, mean(centrality$Eccentricity),
     sprintf("Mean = %.2f", mean(centrality$Eccentricity)),
     cex = 1.1)

# --- Plot 4: Scatter vs Degree ---
plot(centrality$Degree, centrality$Eccentricity,
     main = "Eccentricity vs Degree",
     xlab = "Degree Centrality",
     ylab = "Eccentricity",
     pch = 16,
     col = rgb(0.3, 0.6, 0.8, 0.6),
     cex = 1.2,
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
abline(lm(centrality$Eccentricity ~ centrality$Degree), col = "darkblue", lwd = 3)
grid()

par(mfrow = c(1, 1),
    mar = c(5, 4, 4, 2),
    oma = c(0, 0, 0, 0))

dev.off()
cat("✓ Saved: 06_eccentricity_plots.pdf\n\n")

# ==============================================================================
# 9. CLUSTERING COEFFICIENT VISUALIZATIONS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING CLUSTERING COEFFICIENT VISUALIZATIONS\n")
cat("================================================================================\n\n")

# Calculate clustering coefficients
local_clustering <- transitivity(g_party, type = "local")
local_clustering[is.nan(local_clustering)] <- 0

clustering_df <- data.frame(
  Party = V(g_party)$name,
  Clustering = local_clustering,
  Degree = degree(g_party),
  stringsAsFactors = FALSE
)
clustering_df <- clustering_df[!is.na(clustering_df$Clustering), ]

pdf(file.path(output_dir, "07_clustering_coefficient_plots.pdf"), width = 16, height = 12)

par(mfrow = c(2, 2),
    mar = c(12, 5, 5, 2),
    oma = c(2, 0, 2, 0))

# --- Plot 1: Top 30 Parties ---
top_30_clust <- head(clustering_df[order(-clustering_df$Clustering), ], 30)

barplot(top_30_clust$Clustering,
        names.arg = top_30_clust$Party,
        main = "Top 30 Parties by Clustering Coefficient",
        ylab = "Clustering Coefficient",
        col = "salmon",
        las = 2,
        cex.names = 0.7,
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        border = NA)
grid(nx = NA, ny = NULL)

# --- Plot 2: Histogram ---
hist(clustering_df$Clustering,
     breaks = 40,
     main = "Clustering Coefficient Distribution",
     xlab = "Clustering Coefficient",
     ylab = "Frequency (Number of Parties)",
     col = "lightsalmon",
     border = "white",
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
abline(v = mean(clustering_df$Clustering), col = "red", lwd = 3, lty = 2)
legend("topright",
       legend = c(sprintf("Mean = %.4f", mean(clustering_df$Clustering)),
                 sprintf("Median = %.4f", median(clustering_df$Clustering))),
       col = c("red", "black"),
       lty = c(2, 0),
       lwd = c(3, 0),
       cex = 1.1,
       bg = "white")

# --- Plot 3: Boxplot ---
boxplot(clustering_df$Clustering,
        main = "Clustering Coefficient - Boxplot",
        ylab = "Clustering Coefficient",
        col = "lightsalmon",
        border = "salmon",
        cex.axis = 1.0,
        cex.lab = 1.2,
        cex.main = 1.3,
        lwd = 2)
points(1, mean(clustering_df$Clustering), col = "red", pch = 18, cex = 2.5)
text(1.3, mean(clustering_df$Clustering),
     sprintf("Mean = %.4f", mean(clustering_df$Clustering)),
     cex = 1.1)

# --- Plot 4: Scatter vs Degree ---
plot(clustering_df$Degree, clustering_df$Clustering,
     main = "Clustering Coefficient vs Degree",
     xlab = "Degree",
     ylab = "Clustering Coefficient",
     pch = 16,
     col = rgb(0.9, 0.5, 0.4, 0.6),
     cex = 1.2,
     cex.axis = 1.0,
     cex.lab = 1.2,
     cex.main = 1.3)
if(nrow(clustering_df) > 1) {
  abline(lm(Clustering ~ Degree, data = clustering_df), col = "darkred", lwd = 3)
}
grid()

par(mfrow = c(1, 1),
    mar = c(5, 4, 4, 2),
    oma = c(0, 0, 0, 0))

dev.off()
cat("✓ Saved: 07_clustering_coefficient_plots.pdf\n\n")

# ==============================================================================
# 10. BASIC NETWORK METRICS VISUALIZATIONS
# ==============================================================================

cat("================================================================================\n")
cat("CREATING BASIC NETWORK METRICS VISUALIZATIONS\n")
cat("================================================================================\n\n")

# Calculate basic metrics
n_nodes <- vcount(g_party)
n_edges <- ecount(g_party)
density <- edge_density(g_party)
avg_path <- mean_distance(g_party, directed = FALSE)
diam <- diameter(g_party, directed = FALSE)
clustering_global <- transitivity(g_party, type = "global")
clustering_avg <- transitivity(g_party, type = "average")
components_info <- components(g_party)
n_components <- components_info$no
largest_comp <- max(components_info$csize)

pdf(file.path(output_dir, "08_basic_network_metrics.pdf"), width = 16, height = 12)

par(mfrow = c(2, 3),
    mar = c(5, 5, 5, 2),
    oma = c(2, 0, 3, 0))

# --- Plot 1: Network Size ---
barplot(c(n_nodes, n_edges),
        names.arg = c("Nodes\n(Parties)", "Edges\n(Connections)"),
        main = "Network Size",
        ylab = "Count",
        col = c("steelblue", "coral"),
        cex.names = 1.2,
        cex.axis = 1.1,
        cex.lab = 1.2,
        cex.main = 1.3,
        border = NA,
        ylim = c(0, max(n_nodes, n_edges) * 1.15))
text(c(0.7, 1.9), c(n_nodes, n_edges) + max(n_nodes, n_edges) * 0.05,
     labels = c(n_nodes, n_edges),
     cex = 1.3,
     font = 2)
grid(nx = NA, ny = NULL)

# --- Plot 2: Network Density ---
barplot(density,
        main = "Network Density",
        ylab = "Density",
        col = "gold",
        cex.axis = 1.1,
        cex.lab = 1.2,
        cex.main = 1.3,
        border = NA,
        ylim = c(0, max(0.1, density * 1.3)),
        names.arg = "")
text(0.7, density + 0.005,
     sprintf("%.4f\n(%.2f%%)", density, density * 100),
     cex = 1.3,
     font = 2)
grid(nx = NA, ny = NULL)

# --- Plot 3: Path Length & Diameter ---
barplot(c(avg_path, diam),
        names.arg = c("Average\nPath Length", "Network\nDiameter"),
        main = "Path Metrics",
        ylab = "Number of Steps",
        col = c("lightgreen", "lightcoral"),
        cex.names = 1.2,
        cex.axis = 1.1,
        cex.lab = 1.2,
        cex.main = 1.3,
        border = NA,
        ylim = c(0, max(avg_path, diam) * 1.2))
text(c(0.7, 1.9), c(avg_path, diam) + max(avg_path, diam) * 0.05,
     labels = sprintf("%.2f", c(avg_path, diam)),
     cex = 1.3,
     font = 2)
grid(nx = NA, ny = NULL)

# --- Plot 4: Clustering Coefficients ---
barplot(c(clustering_global, clustering_avg),
        names.arg = c("Global\nClustering", "Average Local\nClustering"),
        main = "Clustering Coefficients",
        ylab = "Clustering Coefficient",
        col = c("mediumpurple", "plum"),
        cex.names = 1.2,
        cex.axis = 1.1,
        cex.lab = 1.2,
        cex.main = 1.3,
        border = NA,
        ylim = c(0, 1))
text(c(0.7, 1.9), c(clustering_global, clustering_avg) + 0.05,
     labels = sprintf("%.3f", c(clustering_global, clustering_avg)),
     cex = 1.3,
     font = 2)
grid(nx = NA, ny = NULL)

# --- Plot 5: Connected Components ---
barplot(c(n_components, largest_comp),
        names.arg = c("Number of\nComponents", "Largest\nComponent Size"),
        main = "Network Components",
        ylab = "Count",
        col = c("skyblue", "steelblue"),
        cex.names = 1.2,
        cex.axis = 1.1,
        cex.lab = 1.2,
        cex.main = 1.3,
        border = NA,
        ylim = c(0, max(n_components, largest_comp) * 1.15))
text(c(0.7, 1.9), c(n_components, largest_comp) + max(n_components, largest_comp) * 0.05,
     labels = c(n_components, sprintf("%d (%.1f%%)", largest_comp, 100 * largest_comp / n_nodes)),
     cex = 1.3,
     font = 2)
grid(nx = NA, ny = NULL)

# --- Plot 6: Component Size Distribution ---
comp_sizes <- sort(components_info$csize, decreasing = TRUE)
if(length(comp_sizes) > 20) {
  comp_sizes_plot <- c(comp_sizes[1:20], sum(comp_sizes[21:length(comp_sizes)]))
  comp_names <- c(1:20, "Others")
} else {
  comp_sizes_plot <- comp_sizes
  comp_names <- 1:length(comp_sizes)
}

barplot(comp_sizes_plot,
        names.arg = comp_names,
        main = "Component Size Distribution (Top 20)",
        xlab = "Component Rank",
        ylab = "Size (Number of Parties)",
        col = "lightsalmon",
        cex.names = 0.9,
        cex.axis = 1.1,
        cex.lab = 1.2,
        cex.main = 1.3,
        border = NA)
grid(nx = NA, ny = NULL)

# Add overall title
mtext("Basic Network Metrics Summary", 
      outer = TRUE, 
      cex = 1.5, 
      font = 2,
      line = 0.5)

par(mfrow = c(1, 1),
    mar = c(5, 4, 4, 2),
    oma = c(0, 0, 0, 0))

dev.off()
cat("✓ Saved: 08_basic_network_metrics.pdf\n\n")

# Create summary table visualization
pdf(file.path(output_dir, "09_network_metrics_table.pdf"), width = 12, height = 8)

par(mar = c(1, 1, 3, 1))

# Create metrics table
metrics_data <- data.frame(
  Metric = c("Number of Parties (Nodes)",
             "Number of Connections (Edges)",
             "Network Density",
             "Average Path Length",
             "Network Diameter",
             "Global Clustering Coefficient",
             "Average Local Clustering Coefficient",
             "Number of Connected Components",
             "Largest Component Size",
             "Largest Component Percentage"),
  Value = c(sprintf("%d", n_nodes),
            sprintf("%d", n_edges),
            sprintf("%.4f (%.2f%%)", density, density * 100),
            sprintf("%.2f steps", avg_path),
            sprintf("%d steps", diam),
            sprintf("%.3f", clustering_global),
            sprintf("%.3f", clustering_avg),
            sprintf("%d", n_components),
            sprintf("%d parties", largest_comp),
            sprintf("%.1f%%", 100 * largest_comp / n_nodes)),
  stringsAsFactors = FALSE
)

# Plot as table
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

title("Network Metrics Summary Table", cex.main = 1.8, font.main = 2)

# Draw table
n_rows <- nrow(metrics_data)
row_height <- 0.8 / (n_rows + 1)

# Header
rect(0.05, 0.95 - row_height, 0.55, 0.95, col = "steelblue", border = "black", lwd = 2)
rect(0.55, 0.95 - row_height, 0.95, 0.95, col = "steelblue", border = "black", lwd = 2)
text(0.3, 0.95 - row_height/2, "Metric", cex = 1.3, font = 2, col = "white")
text(0.75, 0.95 - row_height/2, "Value", cex = 1.3, font = 2, col = "white")

# Data rows
for(i in 1:n_rows) {
  y_top <- 0.95 - (i + 1) * row_height
  y_bottom <- 0.95 - i * row_height
  
  # Alternate row colors
  row_col <- if(i %% 2 == 0) "gray95" else "white"
  
  rect(0.05, y_top, 0.55, y_bottom, col = row_col, border = "gray70")
  rect(0.55, y_top, 0.95, y_bottom, col = row_col, border = "gray70")
  
  text(0.06, (y_top + y_bottom)/2, metrics_data$Metric[i], 
       cex = 1.0, adj = 0, font = 1)
  text(0.75, (y_top + y_bottom)/2, metrics_data$Value[i], 
       cex = 1.1, font = 2, col = "darkblue")
}

dev.off()
cat("✓ Saved: 09_network_metrics_table.pdf\n\n")

# ==============================================================================
# 11. COMPREHENSIVE COMPARISON
# ==============================================================================

cat("================================================================================\n")
cat("CREATING COMPREHENSIVE COMPARISON PLOTS\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "10_centrality_comparison_comprehensive.pdf"), 
    width = 18, height = 14)

# Get top 20 parties overall
top_20_overall <- head(centrality[order(-centrality$Degree), ], 20)

# Normalize all centralities to 0-1 scale for comparison
top_20_overall$Degree_norm <- top_20_overall$Degree / max(centrality$Degree)
top_20_overall$Betweenness_norm <- top_20_overall$Betweenness / max(centrality$Betweenness)
top_20_overall$Closeness_norm <- top_20_overall$Closeness / max(centrality$Closeness)
top_20_overall$Eigenvector_norm <- top_20_overall$Eigenvector / max(centrality$Eigenvector)
top_20_overall$PageRank_norm <- top_20_overall$PageRank / max(centrality$PageRank)

# Create comparison plot
par(mar = c(14, 5, 5, 2),
    oma = c(2, 0, 2, 0))

cent_matrix <- as.matrix(top_20_overall[, c("Degree_norm", "Betweenness_norm",
                                              "Closeness_norm", "Eigenvector_norm",
                                              "PageRank_norm")])
rownames(cent_matrix) <- top_20_overall$Party

barplot(t(cent_matrix),
        beside = TRUE,
        main = "Top 20 Parties - All Centrality Measures (Normalized)",
        xlab = "",
        ylab = "Normalized Centrality Score (0-1)",
        col = c("steelblue", "coral", "gold", "lightgreen", "mediumpurple"),
        legend.text = c("Degree", "Betweenness", "Closeness", "Eigenvector", "PageRank"),
        args.legend = list(x = "topright", cex = 1.2, bg = "white"),
        las = 2,
        cex.names = 0.8,
        cex.axis = 1.1,
        cex.lab = 1.3,
        cex.main = 1.4,
        border = NA)
grid(nx = NA, ny = NULL)

dev.off()
cat("✓ Saved: 10_centrality_comparison_comprehensive.pdf\n\n")

# ==============================================================================
# 12. SUMMARY
# ==============================================================================

cat("================================================================================\n")
cat("NETWORK METRICS VISUALIZATIONS COMPLETE!\n")
cat("================================================================================\n\n")

cat("FILES SAVED TO:", output_dir, "\n")
cat("  • 01_degree_centrality_plots.pdf (4 plots)\n")
cat("  • 02_betweenness_centrality_plots.pdf (4 plots)\n")
cat("  • 03_closeness_centrality_plots.pdf (4 plots)\n")
cat("  • 04_eigenvector_centrality_plots.pdf (4 plots)\n")
cat("  • 05_pagerank_centrality_plots.pdf (4 plots)\n")
cat("  • 06_eccentricity_plots.pdf (4 plots)\n")
cat("  • 07_clustering_coefficient_plots.pdf (4 plots)\n")
cat("  • 08_basic_network_metrics.pdf (6 plots)\n")
cat("  • 09_network_metrics_table.pdf (summary table)\n")
cat("  • 10_centrality_comparison_comprehensive.pdf (1 plot)\n\n")

cat("CENTRALITY PLOTS (01-07) EACH CONTAIN:\n")
cat("  1. Top 30 parties bar chart (NO OVERLAP)\n")
cat("  2. Distribution histogram\n")
cat("  3. Boxplot with statistics\n")
cat("  4. Scatter plot vs Degree (or cumulative for degree)\n\n")

cat("BASIC METRICS PLOTS (08-09) CONTAIN:\n")
cat("  • Network size (nodes & edges)\n")
cat("  • Network density\n")
cat("  • Path metrics (average path length & diameter)\n")
cat("  • Clustering coefficients (global & local)\n")
cat("  • Component analysis\n")
cat("  • Component size distribution\n")
cat("  • Professional summary table\n\n")

cat("✓ Total: 10 PDFs with 36 total plots\n")
cat("✓ All labels clearly visible with no overlap\n")
cat("✓ Larger fonts for better readability\n")
cat("✓ Includes all basic network metrics from your summary\n\n")

cat("================================================================================\n")