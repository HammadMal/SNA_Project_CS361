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

pdf(file.path(output_dir, "02_centrality_comparison.pdf"), width = 16, height = 12)

# Get top 15 parties by degree
top_parties <- head(centrality[order(-centrality$Degree), ], 15)

# Normalize centralities to 0-1 scale
top_parties$Degree_norm <- top_parties$Degree / max(centrality$Degree)
top_parties$Betweenness_norm <- top_parties$Betweenness / max(centrality$Betweenness)
top_parties$Closeness_norm <- top_parties$Closeness / max(centrality$Closeness)
top_parties$Eigenvector_norm <- top_parties$Eigenvector / max(centrality$Eigenvector)
top_parties$PageRank_norm <- top_parties$PageRank / max(centrality$PageRank)

# --- Plot 1: Grouped Bar Chart with better spacing ---
cent_matrix <- as.matrix(top_parties[, c("Degree_norm", "Betweenness_norm",
                                          "Closeness_norm", "Eigenvector_norm",
                                          "PageRank_norm")])
rownames(cent_matrix) <- top_parties$Party

# Set margins: bottom, left, top, right (extra space on bottom and top)
par(mar = c(11, 5, 6, 2))

barplot(t(cent_matrix),
        beside = TRUE,
        main = "Normalized Centrality Measures - Top 15 Parties",
        xlab = "",
        ylab = "Normalized Centrality (0-1)",
        col = c("steelblue", "coral", "gold", "lightgreen", "purple"),
        las = 2,
        cex.names = 0.75,
        cex.axis = 1.0,
        cex.lab = 1.1,
        ylim = c(0, 1.15))

# Add legend in top margin area
par(xpd = TRUE)
legend(x = "top",
       inset = c(0, -0.12),
       legend = c("Degree", "Betweenness", "Closeness", "Eigenvector", "PageRank"),
       fill = c("steelblue", "coral", "gold", "lightgreen", "purple"),
       horiz = TRUE,
       cex = 0.85,
       bty = "n")
par(xpd = FALSE)

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

# --- Plot 3: Closeness Centrality ---
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

# Color by degree using light pink → light lavender → light blue gradient (same as clustering)
node_colors_deg <- colorRampPalette(c("#FFC0CB", "#E6E6FA", "#B0E0E6"), alpha = TRUE)(100)[
                   ceiling(deg_norm * 99) + 1]

# Variable label distance: only very small nodes get more distance
# Large/medium nodes stay close (0.1), only very small nodes get pushed out
label_dist_deg <- ifelse(deg_norm > 0.3,  # If node is medium or large
                         0,              # Keep label close
                         (1 - deg_norm) * 0.9)  # Otherwise push out (range 0.56-0.8)

plot(g_viz,
     layout = common_layout,
     vertex.size = 10,
     vertex.color = node_colors_deg,
     vertex.label = V(g_viz)$name,      # Show ALL labels
     vertex.label.cex = 0.5,             # Smaller font for readability
     vertex.label.color = "black",
     vertex.label.dist = 0, # Variable distance based on node size
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Party Network - Degree Centrality\n(Size and Color by Degree)")

legend("topright",
       legend = c("High Degree", "Medium Degree", "Low Degree"),
       col = colorRampPalette(c("#B0E0E6", "#E6E6FA", "#FFC0CB"), alpha = TRUE)(3),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 08_network_degree_centrality.pdf\n")

# --- Plot 2: Betweenness Centrality ---
pdf(file.path(output_dir, "09_network_betweenness_centrality.pdf"), width = 14, height = 10)

# Get betweenness values from full network but only for top 50
top_50_betw <- centrality$Betweenness[match(V(g_viz)$name, centrality$Party)]

# Log transform the betweenness values to better handle the large range
betw_log <- log1p(top_50_betw)  # log1p to handle zeros
betw_norm <- (betw_log - min(betw_log)) / (max(betw_log) - min(betw_log))

# Adjust node sizes to show more variation
node_size_betw <- betw_norm * 15 + 2  # Range from 2 to 17

# Use same color scheme as clustering coefficient (light pink → lavender → light blue)
node_colors_betw <- colorRampPalette(c("#FFC0CB", "#E6E6FA", "#B0E0E6"), alpha = TRUE)(100)[
                     ceiling(betw_norm * 99) + 1]

plot(g_viz,
     layout = common_layout,
     vertex.size = 10,
     vertex.color = node_colors_betw,
     vertex.label = V(g_viz)$name,  # Show ALL labels
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Party Network - Betweenness Centrality\n(Size and Color by Betweenness, Log-scaled)")

legend("topright",
       legend = c("High Betweenness", "Medium Betweenness", "Low Betweenness"),
       col = colorRampPalette(c("#B0E0E6", "#E6E6FA", "#FFC0CB"), alpha = TRUE)(3),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()
cat("✓ Saved: 09_network_betweenness_centrality.pdf\n")

# --- Plot 3: Closeness Centrality ---
pdf(file.path(output_dir, "10_network_closeness_centrality.pdf"), width = 14, height = 10)

# Get closeness values from full network
top_50_clos <- centrality$Closeness[match(V(g_viz)$name, centrality$Party)]
clos_log <- log1p(top_50_clos)
clos_norm <- (clos_log - min(clos_log)) / (max(clos_log) - min(clos_log))
node_size_clos <- clos_norm * 15 + 2

# Green color scheme
node_colors_clos <- colorRampPalette(c("#E5F5E0", "#74C476", "#006D2C"))(100)[
                    ceiling(clos_norm * 99) + 1]

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_clos,
     vertex.color = node_colors_clos,
     vertex.label = ifelse(top_50_clos > quantile(top_50_clos, 0.70), V(g_viz)$name, NA),
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Party Network - Closeness Centrality\n(Size and Color by Closeness, Log-scaled)")

legend("topright",
       legend = c("High Closeness", "Medium Closeness", "Low Closeness"),
       col = colorRampPalette(c("#006D2C", "#74C476", "#E5F5E0"))(3),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()

# --- Plot 4: Eigenvector Centrality ---
pdf(file.path(output_dir, "11_network_eigenvector_centrality.pdf"), width = 14, height = 10)

top_50_eigen <- centrality$Eigenvector[match(V(g_viz)$name, centrality$Party)]
eigen_log <- log1p(top_50_eigen)
eigen_norm <- (eigen_log - min(eigen_log)) / (max(eigen_log) - min(eigen_log))
node_size_eigen <- eigen_norm * 15 + 2

# Purple color scheme
node_colors_eigen <- colorRampPalette(c("#F2F0F7", "#9E9AC8", "#54278F"))(100)[
                     ceiling(eigen_norm * 99) + 1]

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_eigen,
     vertex.color = node_colors_eigen,
     vertex.label = ifelse(top_50_eigen > quantile(top_50_eigen, 0.70), V(g_viz)$name, NA),
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Party Network - Eigenvector Centrality\n(Size and Color by Eigenvector, Log-scaled)")

legend("topright",
       legend = c("High Eigenvector", "Medium Eigenvector", "Low Eigenvector"),
       col = colorRampPalette(c("#54278F", "#9E9AC8", "#F2F0F7"))(3),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()

# --- Plot 5: PageRank ---
pdf(file.path(output_dir, "12_network_pagerank_centrality.pdf"), width = 14, height = 10)

top_50_pr <- centrality$PageRank[match(V(g_viz)$name, centrality$Party)]
pr_log <- log1p(top_50_pr)
pr_norm <- (pr_log - min(pr_log)) / (max(pr_log) - min(pr_log))
node_size_pr <- pr_norm * 15 + 2

# Red color scheme
node_colors_pr <- colorRampPalette(c("#FEE5D9", "#FB6A4A", "#A50F15"))(100)[
                  ceiling(pr_norm * 99) + 1]

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_pr,
     vertex.color = node_colors_pr,
     vertex.label = ifelse(top_50_pr > quantile(top_50_pr, 0.70), V(g_viz)$name, NA),
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Party Network - PageRank\n(Size and Color by PageRank, Log-scaled)")

legend("topright",
       legend = c("High PageRank", "Medium PageRank", "Low PageRank"),
       col = colorRampPalette(c("#A50F15", "#FB6A4A", "#FEE5D9"))(3),
       pch = 16, pt.cex = 2, cex = 0.9, bg = "white")

dev.off()

# --- Plot 6: Eccentricity (IMPROVED VERSION) ---
pdf(file.path(output_dir, "13_network_eccentricity.pdf"), width = 14, height = 10)

top_50_ecc <- centrality$Eccentricity[match(V(g_viz)$name, centrality$Party)]

# Print diagnostic information
cat("\n=== Eccentricity Distribution in Top 50 ===\n")
cat(sprintf("Range: %d to %d\n", min(top_50_ecc), max(top_50_ecc)))
cat("Value counts:\n")
print(table(top_50_ecc))
cat("\n")

# Get unique values and create a discrete color scheme
unique_ecc <- sort(unique(top_50_ecc))
n_levels <- length(unique_ecc)

# Create a color palette from DARK (for 0) to LIGHT (for max)
# Using blue-purple gradient for better visibility
if (n_levels <= 2) {
  # If only 2 levels, use very distinct colors
  color_palette <- c("#08519C", "#C6DBEF")
} else if (n_levels <= 5) {
  # For 3-5 levels, create evenly spaced colors
  color_palette <- colorRampPalette(c("#08519C", "#3182BD", "#6BAED6", "#9ECAE1", "#C6DBEF", "#EFF3FF"))(n_levels)
} else {
  color_palette <- colorRampPalette(c("#08519C", "#3182BD", "#EFF3FF"))(n_levels)
}

# Map each eccentricity value to its color
ecc_colors <- color_palette[match(top_50_ecc, unique_ecc)]

# Size nodes: LARGER for LOWER eccentricity (more central)
# Use inverse scaling so 0 is largest
max_ecc <- max(top_50_ecc)
if (max_ecc > 0) {
  size_scale <- 1 - (top_50_ecc / max_ecc)
} else {
  size_scale <- rep(1, length(top_50_ecc))
}
node_size_ecc <- size_scale * 15 + 5  # Range from 5 to 20

# Label strategy: Show ALL party names but make them readable
# Adjust label size based on importance (lower eccentricity = larger label)
label_sizes <- size_scale * 0.4 + 0.4  # Range from 0.4 to 0.8

plot(g_viz,
     layout = common_layout,
     vertex.size = node_size_ecc,
     vertex.color = ecc_colors,
     vertex.label = V(g_viz)$name,  # Show ALL labels
     vertex.label.cex = label_sizes,  # Variable label sizes
     vertex.label.color = "black",
     vertex.label.dist = 0,  # Slight offset for readability
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.1),  # More transparent edges
     main = "Party Network - Eccentricity\n(Darker/Larger = Lower Eccentricity = More Central)")

# Create a better legend showing actual values
legend_labels <- paste0("Ecc = ", unique_ecc)
if (unique_ecc[1] == 0) {
  legend_labels[1] <- "Ecc = 0 (Most Central)"
}
if (length(unique_ecc) > 1) {
  legend_labels[length(legend_labels)] <- paste0("Ecc = ", unique_ecc[length(unique_ecc)], " (Peripheral)")
}

legend("topright",
       legend = legend_labels,
       col = color_palette,
       pch = 16,
       pt.cex = 2.5,
       cex = 0.9,
       bg = "white",
       title = "Eccentricity Values")

dev.off()
cat("✓ Saved: 13_network_eccentricity.pdf\n")

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

# Get clustering coefficients from full network for the top 50 parties
top_50_clust <- clustering_df_full$Clustering[match(V(g_clust_viz)$name, clustering_df_full$Party)]

# Normalize clustering for sizing and coloring
clust_norm <- (top_50_clust - min(top_50_clust)) / (max(top_50_clust) - min(top_50_clust))

# Size by clustering (adjusted range)
node_size_clust <- clust_norm * 12 + 3  # smaller multiplier for better size range

# Color by clustering using light pink → light lavender → light blue gradient
node_colors_clust <- colorRampPalette(c("#FFC0CB", "#E6E6FA", "#B0E0E6"), alpha = TRUE)(100)[
                     ceiling(clust_norm * 99) + 1]

# Variable label distance: only very small nodes get more distance
# Large/medium nodes stay close (0.1), only very small nodes get pushed out
label_dist_clust <- ifelse(clust_norm > 0.3,  # If node is medium or large
                           0.1,                # Keep label close
                           (1 - clust_norm) * 0.8)  # Otherwise push out

# Use same layout as other centrality plots
plot(g_clust_viz,
     layout = common_layout,
     vertex.size = 10,
     vertex.color = node_colors_clust,
     vertex.label = V(g_clust_viz)$name,        # Show ALL labels
     vertex.label.cex = 0.5,                     # Smaller font for readability
     vertex.label.color = "black",
     vertex.label.dist = 0,      # Variable distance based on node size
     vertex.frame.color = "white",
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Party Network - Clustering Coefficient\n(Size and Color by Local Clustering)")

legend("topright",
       legend = c("High Clustering", "Medium Clustering", "Low Clustering"),
       col = colorRampPalette(c("#B0E0E6", "#E6E6FA", "#FFC0CB"), alpha = TRUE)(3),
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

# Visualize Louvain communities - ALL Parties with IMPROVED LAYOUT
pdf(file.path(output_dir, "15_louvain_communities_all.pdf"), width = 20, height = 16)

# Use the full network Louvain communities
set.seed(123)

# IMPROVED LAYOUT: Place each community in separate circles
# Get community memberships from full network
memberships <- membership(louvain_comm)
num_communities <- length(unique(memberships))
community_colors <- rainbow(num_communities, alpha = 0.8)

# Create custom layout with communities in separate circles
layout_louvain <- matrix(0, nrow = vcount(g_party), ncol = 2)

# Arrange communities in a circle of circles
outer_radius <- 4  # Radius of the big circle that holds all community circles

for (comm_id in 1:num_communities) {
  # Get nodes in this community
  nodes_in_comm <- which(memberships == comm_id)
  n_nodes <- length(nodes_in_comm)

  # Position for this community's center on the outer circle
  angle_outer <- 2 * pi * (comm_id - 1) / num_communities
  center_x <- outer_radius * cos(angle_outer)
  center_y <- outer_radius * sin(angle_outer)

  # Inner radius for nodes within this community (scaled by community size)
  inner_radius <- 0.5 + (n_nodes / max(table(memberships))) * 1.5

  # Arrange nodes in this community in a circle around the community center
  for (i in 1:n_nodes) {
    angle_inner <- 2 * pi * (i - 1) / n_nodes
    layout_louvain[nodes_in_comm[i], 1] <- center_x + inner_radius * cos(angle_inner)
    layout_louvain[nodes_in_comm[i], 2] <- center_y + inner_radius * sin(angle_inner)
  }
}

node_colors_louvain <- community_colors[memberships]

node_size_louvain <- degree(g_party)
node_size_louvain <- (node_size_louvain - min(node_size_louvain)) /
                     (max(node_size_louvain) - min(node_size_louvain)) * 10 + 3

# Shorten long party name for better visualization
labels_louvain <- V(g_party)$name
labels_louvain[labels_louvain == "Sindh Taraqi Passand Party (STP)"] <- "STP"
labels_louvain[labels_louvain == "Sindh Dost Ittehad (SDI) Party"] <- "SDI"

# Get edge weights and normalize for edge thickness
edge_weights <- E(g_party)$weight
if(is.null(edge_weights)) {
  edge_weights <- rep(1, ecount(g_party))
}
# Normalize weights to more visible edge widths (0.1 to 4)
edge_widths <- (edge_weights - min(edge_weights)) / (max(edge_weights) - min(edge_weights))
edge_widths <- edge_widths * 3.9 + 0.1

plot(g_party,
     layout = layout_louvain,
     vertex.size = node_size_louvain,
     vertex.color = node_colors_louvain,
     vertex.label = labels_louvain,
     vertex.label.cex = 0.8,
     vertex.label.color = "black",
     vertex.label.dist = 0.1,
     vertex.frame.color = "white",
     edge.width = edge_widths,
     edge.color = rgb(0, 0, 0, 0.15),
     main = sprintf("Louvain Communities - All Parties (Circular Layout)\n%d communities, Modularity = %.3f",
                   length(louvain_comm), modularity(louvain_comm)))

# Add legend with community sizes
comm_sizes <- table(memberships)
legend_text <- paste0("Community ", 1:num_communities, " (n=", comm_sizes, ")")
legend("bottomright",
       legend = legend_text,
       col = community_colors,
       pch = 16,
       pt.cex = 2,
       cex = 1.0,
       bg = "white",
       title = "Communities",
       ncol = 2)

# Add note about edge thickness
text(x = par("usr")[1], y = par("usr")[3],
     labels = "Note: Edge thickness represents connection weight (number of shared candidates)",
     adj = c(0, -0.5),
     cex = 0.9,
     col = "black",
     font = 3)

dev.off()
cat("✓ Saved: 15_louvain_communities_all.pdf (with circular community layout for all parties)\n\n")

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

# Visualize Walktrap communities - Top 50 Parties with IMPROVED LAYOUT
pdf(file.path(output_dir, "16_walktrap_communities_top50.pdf"), width = 16, height = 12)

walktrap_top <- cluster_walktrap(g_top_comm)

set.seed(456)

# IMPROVED LAYOUT: Place each community in separate circles
# Get community memberships for walktrap
memberships_wt <- membership(walktrap_top)
num_communities_wt_top <- length(unique(memberships_wt))
community_colors_wt_top <- rainbow(num_communities_wt_top, alpha = 0.8)

# Create custom layout with communities in separate circles
layout_top_wt <- matrix(0, nrow = vcount(g_top_comm), ncol = 2)

# Arrange communities in a circle of circles
outer_radius_wt <- 3

for (comm_id in 1:num_communities_wt_top) {
  # Get nodes in this community
  nodes_in_comm <- which(memberships_wt == comm_id)
  n_nodes <- length(nodes_in_comm)

  # Position for this community's center on the outer circle
  angle_outer <- 2 * pi * (comm_id - 1) / num_communities_wt_top
  center_x <- outer_radius_wt * cos(angle_outer)
  center_y <- outer_radius_wt * sin(angle_outer)

  # Inner radius for nodes within this community (scaled by community size)
  inner_radius <- 0.5 + (n_nodes / 50) * 0.5

  # Arrange nodes in this community in a circle around the community center
  for (i in 1:n_nodes) {
    angle_inner <- 2 * pi * (i - 1) / n_nodes
    layout_top_wt[nodes_in_comm[i], 1] <- center_x + inner_radius * cos(angle_inner)
    layout_top_wt[nodes_in_comm[i], 2] <- center_y + inner_radius * sin(angle_inner)
  }
}

node_colors_top_wt <- community_colors_wt_top[memberships_wt]

plot(g_top_comm,
     layout = layout_top_wt,
     vertex.size = node_size_top_comm,
     vertex.color = node_colors_top_wt,
     vertex.label = V(g_top_comm)$name,
     vertex.label.cex = 0.55,
     vertex.label.color = "black",
     vertex.label.dist = 0.2,
     vertex.frame.color = "white",
     edge.width = 0.5,
     edge.color = rgb(0, 0, 0, 0.15),
     main = sprintf("Walktrap Communities - Top 50 Parties (Circular Layout)\n%d communities, Modularity = %.3f",
                   length(walktrap_top), modularity(walktrap_top)))

# Add legend with community sizes
comm_sizes_wt <- table(memberships_wt)
legend_text_wt <- paste0("Community ", 1:num_communities_wt_top, " (n=", comm_sizes_wt, ")")
legend("bottomright",
       legend = legend_text_wt,
       col = community_colors_wt_top,
       pch = 16,
       pt.cex = 1.5,
       cex = 0.7,
       bg = "white",
       title = "Communities")

dev.off()
cat("✓ Saved: 16_walktrap_communities_top50.pdf (with circular community layout)\n\n")

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
# 15. COMPLETE UNIPARTITE NETWORK VISUALIZATION
# ==============================================================================

cat("================================================================================\n")
cat("CREATING COMPLETE UNIPARTITE PARTY-PARTY NETWORK VISUALIZATION\n")
cat("================================================================================\n\n")

pdf(file.path(output_dir, "06_complete_unipartite_network.pdf"), width = 20, height = 20)

# Use the complete party-party network (all nodes)
cat(sprintf("Visualizing complete network: %d nodes, %d edges\n",
            vcount(g_party), ecount(g_party)))

# Calculate layout for complete network with better spacing
set.seed(123)
cat("Calculating layout (this may take a moment)...\n")
# Use graphopt layout for better spacing and fewer overlaps
complete_layout <- layout_with_graphopt(g_party,
                                        niter = 1000,
                                        charge = 0.02,
                                        mass = 30,
                                        spring.length = 1.5,
                                        spring.constant = 1)

cat("Creating visualization...\n")
plot(g_party,
     layout = complete_layout,
     vertex.size = 3,
     vertex.color = "lightblue",
     vertex.label = V(g_party)$name,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.label.font = 1,
     vertex.frame.color = "gray30",
     vertex.frame.width = 0.5,
     edge.width = 0.4,
     edge.color = rgb(0.3, 0.3, 0.3, 0.3),
     edge.curved = 0.1,
     main = sprintf("Complete Party-Party Unipartite Network\n%d Parties, %d Connections",
                   vcount(g_party), ecount(g_party)))

# Add network statistics
legend("topright",
       legend = c(
         "Network Statistics:",
         sprintf("Nodes: %d", vcount(g_party)),
         sprintf("Edges: %d", ecount(g_party)),
         sprintf("Density: %.4f", edge_density(g_party)),
         sprintf("Avg Degree: %.2f", mean(degree(g_party))),
         sprintf("Components: %d", components(g_party)$no),
         sprintf("Avg Path Length: %.2f", mean_distance(g_party)),
         sprintf("Clustering Coef: %.4f", transitivity(g_party, type = "global"))
       ),
       bty = "o",
       bg = "white",
       cex = 0.7,
       box.col = "gray50")

dev.off()

cat("✓ Saved: 06_complete_unipartite_network.pdf\n\n")

# ==============================================================================
# 16. SUMMARY
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
cat("  • 06_complete_unipartite_network.pdf (ALL NODES & EDGES)\n")
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
cat("✓ Total: 18 PDFs + 4 CSVs\n\n")

cat("================================================================================\n")