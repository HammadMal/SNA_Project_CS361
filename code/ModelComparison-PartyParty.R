# ==============================================================================
# Social Network Analysis - Pakistan Elections
# Script: Comparison with Seminal Network Models (ER, WS, BA)
# ==============================================================================
# Authors: Hammad Malik (hm08298) & Mehlab Kashani (mk07950)
# Course: CS/SDP 361/352
# Date: November 22, 2025
# Purpose: Compare Party-Party network with Random, Small-World, and Scale-Free models
# ==============================================================================

# --- Load Required Libraries ---
install.packages("igraph", dependencies=TRUE)
library(igraph)

# ==============================================================================
# 1. CONFIGURATION
# ==============================================================================

cat("================================================================================\n")
cat("SEMINAL NETWORK MODEL COMPARISON\n")
cat("================================================================================\n\n")

# Set your data paths here
resultpath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/results"

# Input files
metrics_dir <- file.path(resultpath, "network_metrics_party_party")
network_file <- file.path(metrics_dir, "party_party_network.rds")

# Output directory
output_dir <- file.path(resultpath, "model_comparison")

# Create output directory if it doesn't exist
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ==============================================================================
# 2. LOAD ACTUAL PARTY-PARTY NETWORK
# ==============================================================================

cat("Loading actual Party-Party network...\n")

# Load network
g_actual <- readRDS(network_file)

# Get network parameters
n_nodes <- vcount(g_actual)
n_edges <- ecount(g_actual)
avg_degree <- mean(degree(g_actual))

cat(sprintf("✓ Loaded Party-Party network:\n"))
cat(sprintf("  • Nodes: %d\n", n_nodes))
cat(sprintf("  • Edges: %d\n", n_edges))
cat(sprintf("  • Average Degree: %.2f\n\n", avg_degree))

# ==============================================================================
# 3. GENERATE SYNTHETIC NETWORKS
# ==============================================================================

cat("================================================================================\n")
cat("GENERATING SYNTHETIC NETWORKS\n")
cat("================================================================================\n\n")

# Set seed for reproducibility
set.seed(123)

# --- 3.1: Random Network (Erdős-Rényi) ---
cat("Generating Random Network (Erdős-Rényi)...\n")
g_random <- erdos.renyi.game(n = n_nodes, p.or.m = n_edges, type = "gnm")
cat(sprintf("✓ ER Random: %d nodes, %d edges\n\n", vcount(g_random), ecount(g_random)))

# --- 3.2: Small-World Network (Watts-Strogatz) ---
cat("Generating Small-World Network (Watts-Strogatz)...\n")
# nei = average degree / 2 (neighbors on each side)
# p = rewiring probability (0.1 is standard)
nei_ws <- max(1, round(avg_degree / 2))
g_smallworld <- watts.strogatz.game(dim = 1, size = n_nodes, nei = nei_ws, p = 0.1)
cat(sprintf("✓ WS Small-World: %d nodes, %d edges (nei=%d, p=0.1)\n\n",
            vcount(g_smallworld), ecount(g_smallworld), nei_ws))

# --- 3.3: Scale-Free Network (Barabási-Albert) ---
cat("Generating Scale-Free Network (Barabási-Albert)...\n")
# m = edges to add per new node (half of average degree is typical)
m_ba <- max(1, round(avg_degree / 2))
g_scalefree <- barabasi.game(n = n_nodes, m = m_ba, directed = FALSE)
cat(sprintf("✓ BA Scale-Free: %d nodes, %d edges (m=%d)\n\n",
            vcount(g_scalefree), ecount(g_scalefree), m_ba))

# ==============================================================================
# 4. CALCULATE METRICS FOR ALL NETWORKS
# ==============================================================================

cat("================================================================================\n")
cat("CALCULATING NETWORK METRICS\n")
cat("================================================================================\n\n")

# Function to calculate all metrics for a network
calculate_metrics <- function(g, network_name) {
  cat(sprintf("Calculating metrics for %s...\n", network_name))

  # Basic metrics
  n <- vcount(g)
  m <- ecount(g)

  # Average Path Length (only for largest component if disconnected)
  if(is.connected(g)) {
    avg_path <- mean_distance(g, directed = FALSE)
  } else {
    # Get largest component
    comp <- components(g)
    largest_comp <- which.max(comp$csize)
    g_comp <- induced_subgraph(g, which(comp$membership == largest_comp))
    avg_path <- mean_distance(g_comp, directed = FALSE)
    cat(sprintf("  Note: Network disconnected, using largest component for path length\n"))
  }

  # Clustering Coefficient (global)
  clustering <- transitivity(g, type = "global")

  # Network Diameter
  if(is.connected(g)) {
    diameter_val <- diameter(g, directed = FALSE)
  } else {
    diameter_val <- diameter(g_comp, directed = FALSE)
  }

  # Degree distribution
  deg <- degree(g)
  avg_deg <- mean(deg)
  max_deg <- max(deg)

  # Check for power-law (scale-free property)
  # Fit power-law: P(k) ~ k^(-gamma)
  # We'll use a simple log-log regression as approximation
  deg_table <- table(deg)
  deg_counts <- as.numeric(deg_table)
  deg_values <- as.numeric(names(deg_table))

  # Remove zeros for log
  valid_idx <- deg_values > 0 & deg_counts > 0
  if(sum(valid_idx) > 3) {
    log_deg <- log(deg_values[valid_idx])
    log_count <- log(deg_counts[valid_idx])

    # Linear regression on log-log
    fit <- lm(log_count ~ log_deg)
    power_law_exp <- abs(coef(fit)[2])  # Slope = -gamma
  } else {
    power_law_exp <- NA
  }

  # Return metrics
  list(
    Network = network_name,
    Nodes = n,
    Edges = m,
    AvgDegree = avg_deg,
    MaxDegree = max_deg,
    AvgPathLength = avg_path,
    ClusteringCoef = clustering,
    Diameter = diameter_val,
    PowerLawExp = power_law_exp
  )
}

# Calculate for all networks
metrics_actual <- calculate_metrics(g_actual, "Party-Party (Actual)")
metrics_random <- calculate_metrics(g_random, "Random (ER)")
metrics_smallworld <- calculate_metrics(g_smallworld, "Small-World (WS)")
metrics_scalefree <- calculate_metrics(g_scalefree, "Scale-Free (BA)")

cat("\n")

# ==============================================================================
# 5. CREATE COMPARISON TABLE
# ==============================================================================

cat("================================================================================\n")
cat("CREATING COMPARISON TABLE\n")
cat("================================================================================\n\n")

# Combine all metrics - only key metrics
comparison_df <- data.frame(
  Metric = c("Average Path Length",
             "Avg Clustering Coefficient",
             "Power Law Coefficient"),

  Party_Party = c(
    round(metrics_actual$AvgPathLength, 3),
    round(metrics_actual$ClusteringCoef, 3),
    round(metrics_actual$PowerLawExp, 3)
  ),

  Random_ER = c(
    round(metrics_random$AvgPathLength, 3),
    round(metrics_random$ClusteringCoef, 3),
    round(metrics_random$PowerLawExp, 3)
  ),

  SmallWorld_WS = c(
    round(metrics_smallworld$AvgPathLength, 3),
    round(metrics_smallworld$ClusteringCoef, 3),
    round(metrics_smallworld$PowerLawExp, 3)
  ),

  ScaleFree_BA = c(
    round(metrics_scalefree$AvgPathLength, 3),
    round(metrics_scalefree$ClusteringCoef, 3),
    round(metrics_scalefree$PowerLawExp, 3)
  ),

  stringsAsFactors = FALSE
)

# Print table
print(comparison_df)
cat("\n")

# Save to CSV
write.csv(comparison_df,
          file.path(output_dir, "model_comparison_table.csv"),
          row.names = FALSE)
cat("✓ Saved: model_comparison_table.csv\n\n")

# ==============================================================================
# 6. CREATE VISUAL COMPARISON TABLE (PDF)
# ==============================================================================

cat("Creating visual comparison table...\n")

pdf(file.path(output_dir, "model_comparison_visual.pdf"), width = 14, height = 7)

par(mar = c(1, 1, 3, 1))
plot.new()

# Title
text(0.5, 0.95, "Network Model Comparison: Party-Party Network",
     cex = 2.0, font = 2)

# Create table layout
n_rows <- nrow(comparison_df)
n_cols <- ncol(comparison_df)

col_width <- 0.18
row_height <- 0.12
start_x <- 0.1
start_y <- 0.75

# Header color - single color for all columns
header_color <- "steelblue"

# Draw headers
for(j in 1:n_cols) {
  x_pos <- start_x + (j - 1) * col_width

  rect(x_pos, start_y, x_pos + col_width, start_y + row_height,
       col = header_color, border = "white", lwd = 2)

  text(x_pos + col_width/2, start_y + row_height/2,
       colnames(comparison_df)[j],
       cex = 1.1, font = 2, col = "white")
}

# Draw data rows
for(i in 1:n_rows) {
  y_pos <- start_y - i * row_height

  for(j in 1:n_cols) {
    x_pos <- start_x + (j - 1) * col_width

    # Alternating row colors
    bg_col <- if(i %% 2 == 0) "#F5F5F5" else "white"
    rect(x_pos, y_pos, x_pos + col_width, y_pos + row_height,
         col = bg_col, border = "gray80", lwd = 0.5)

    # Cell text
    cell_value <- as.character(comparison_df[i, j])
    if(j == 1) {
      # Left-align metric names with padding
      text(x_pos + 0.01, y_pos + row_height/2,
           cell_value,
           cex = 0.95, font = 2, adj = 0)
    } else {
      # Center-align values
      text(x_pos + col_width/2, y_pos + row_height/2,
           cell_value,
           cex = 1.0, font = 1)
    }
  }
}

# Add interpretation notes
text(0.5, 0.12,
     "Interpretation:",
     cex = 1.3, font = 2, pos = 1)

text(0.5, 0.07,
     "• Low Path Length + High Clustering → Small-World characteristics",
     cex = 1.0, pos = 1, col = "gray30")

text(0.5, 0.04,
     "• Power Law Coefficient ~2-3 → Scale-Free characteristics (hub-dominated)",
     cex = 1.0, pos = 1, col = "gray30")

dev.off()

cat("✓ Saved: model_comparison_visual.pdf\n\n")

# ==============================================================================
# 7. DEGREE DISTRIBUTION COMPARISON
# ==============================================================================

cat("Creating degree distribution comparison...\n")

pdf(file.path(output_dir, "degree_distribution_comparison.pdf"), width = 16, height = 12)

par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

# Function to plot degree distribution
plot_degree_dist <- function(g, title, col) {
  deg <- degree(g)
  deg_table <- table(deg)
  deg_counts <- as.numeric(deg_table)
  deg_values <- as.numeric(names(deg_table))

  # Remove zeros
  valid_idx <- deg_values > 0 & deg_counts > 0

  plot(deg_values[valid_idx], deg_counts[valid_idx],
       log = "xy",
       main = title,
       xlab = "Degree (log scale)",
       ylab = "Frequency (log scale)",
       pch = 16,
       col = col,
       cex = 1.5,
       cex.main = 1.5,
       cex.lab = 1.3,
       cex.axis = 1.2)

  # Add power-law fit line
  if(sum(valid_idx) > 3) {
    log_deg <- log(deg_values[valid_idx])
    log_count <- log(deg_counts[valid_idx])
    fit <- lm(log_count ~ log_deg)

    lines(deg_values[valid_idx],
          exp(predict(fit)),
          col = "red", lwd = 2, lty = 2)

    legend("topright",
           legend = sprintf("γ = %.2f", abs(coef(fit)[2])),
           col = "red", lty = 2, lwd = 2,
           cex = 1.2,
           bg = "white")
  }

  grid()
}

# Plot all 4 networks
plot_degree_dist(g_actual, "Party-Party Network (Actual)", "steelblue")
plot_degree_dist(g_random, "Random Network (ER)", "coral")
plot_degree_dist(g_smallworld, "Small-World Network (WS)", "gold")
plot_degree_dist(g_scalefree, "Scale-Free Network (BA)", "lightgreen")

par(mfrow = c(1, 1))
dev.off()

cat("✓ Saved: degree_distribution_comparison.pdf\n\n")

# ==============================================================================
# 8. ANALYSIS SUMMARY
# ==============================================================================

cat("================================================================================\n")
cat("ANALYSIS SUMMARY\n")
cat("================================================================================\n\n")

cat("KEY FINDINGS:\n\n")

# Compare with each model
cat("1. COMPARISON WITH RANDOM NETWORK (ER):\n")
cat(sprintf("   • Clustering: Actual (%.3f) vs Random (%.3f) → ",
            metrics_actual$ClusteringCoef, metrics_random$ClusteringCoef))
if(metrics_actual$ClusteringCoef > metrics_random$ClusteringCoef * 2) {
  cat("MUCH HIGHER (more structured)\n")
} else {
  cat("Similar\n")
}

cat(sprintf("   • Path Length: Actual (%.3f) vs Random (%.3f) → ",
            metrics_actual$AvgPathLength, metrics_random$AvgPathLength))
if(abs(metrics_actual$AvgPathLength - metrics_random$AvgPathLength) < 0.5) {
  cat("Similar\n")
} else {
  cat("Different\n")
}

cat("\n2. COMPARISON WITH SMALL-WORLD (WS):\n")
cat(sprintf("   • Clustering: Actual (%.3f) vs Small-World (%.3f) → ",
            metrics_actual$ClusteringCoef, metrics_smallworld$ClusteringCoef))
if(abs(metrics_actual$ClusteringCoef - metrics_smallworld$ClusteringCoef) < 0.2) {
  cat("SIMILAR (small-world property)\n")
} else {
  cat("Different\n")
}

cat(sprintf("   • Path Length: Actual (%.3f) vs Small-World (%.3f) → ",
            metrics_actual$AvgPathLength, metrics_smallworld$AvgPathLength))
if(abs(metrics_actual$AvgPathLength - metrics_smallworld$AvgPathLength) < 1.0) {
  cat("SIMILAR (small-world property)\n")
} else {
  cat("Different\n")
}

cat("\n3. COMPARISON WITH SCALE-FREE (BA):\n")
cat(sprintf("   • Max Degree: Actual (%d) vs Scale-Free (%d) → ",
            metrics_actual$MaxDegree, metrics_scalefree$MaxDegree))
if(metrics_actual$MaxDegree > n_nodes * 0.1) {
  cat("HUB EXISTS (scale-free property)\n")
} else {
  cat("No dominant hub\n")
}

cat(sprintf("   • Power-Law Exp: Actual (%.3f) vs Scale-Free (%.3f) → ",
            metrics_actual$PowerLawExp, metrics_scalefree$PowerLawExp))
if(!is.na(metrics_actual$PowerLawExp) &&
   metrics_actual$PowerLawExp >= 2 && metrics_actual$PowerLawExp <= 3) {
  cat("FITS POWER-LAW (scale-free property)\n")
} else {
  cat("Not power-law distributed\n")
}

cat("\n================================================================================\n")
cat("CONCLUSION:\n")
cat("================================================================================\n\n")

# Determine which model the network resembles
high_clustering <- metrics_actual$ClusteringCoef > metrics_random$ClusteringCoef * 2
low_path <- metrics_actual$AvgPathLength < n_nodes * 0.1
power_law <- !is.na(metrics_actual$PowerLawExp) &&
             metrics_actual$PowerLawExp >= 2 &&
             metrics_actual$PowerLawExp <= 3

if(high_clustering && low_path && power_law) {
  cat("The Party-Party network exhibits BOTH Small-World AND Scale-Free properties:\n")
  cat("• High clustering coefficient (local cohesion)\n")
  cat("• Short average path length (efficient connectivity)\n")
  cat("• Power-law degree distribution (hub-dominated structure)\n\n")
  cat("This is common in real-world networks, especially social/political networks.\n")
} else if(high_clustering && low_path) {
  cat("The Party-Party network most resembles a SMALL-WORLD network:\n")
  cat("• High clustering coefficient\n")
  cat("• Short average path length\n")
} else if(power_law) {
  cat("The Party-Party network most resembles a SCALE-FREE network:\n")
  cat("• Power-law degree distribution\n")
  cat("• Hub-dominated structure\n")
} else {
  cat("The Party-Party network has unique characteristics not fully captured\n")
  cat("by any single classical model.\n")
}

cat("\n================================================================================\n")
cat("FILES SAVED:\n")
cat("================================================================================\n")
cat("  • model_comparison_table.csv\n")
cat("  • model_comparison_visual.pdf\n")
cat("  • degree_distribution_comparison.pdf\n\n")

cat("✓ Model comparison complete!\n\n")
cat("================================================================================\n")
