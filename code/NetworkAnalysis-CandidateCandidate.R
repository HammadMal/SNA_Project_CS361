# ==============================================================================
# Social Network Analysis - Pakistan Elections
# Script: Candidate-Candidate Network Metrics Analysis
# ==============================================================================
# Authors: Hammad Malik (hm08298) & Mehlab Kashani (mk07950)
# Course: CS/SDP 361/352
# Date: November 2, 2025
# Network Type: Candidate-Candidate Unipartite Projection (Weighted)
# ==============================================================================

# --- Load Required Libraries ---
install.packages("igraph", dependencies=TRUE)
library(igraph)

# ==============================================================================
# 1. CONFIGURATION
# ==============================================================================

cat("================================================================================\n")
cat("CANDIDATE-CANDIDATE NETWORK METRICS ANALYSIS\n")
cat("================================================================================\n\n")

# Set your data paths here
resultpath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/results"

# Input file (from Bipartition-CandidateCandidate.R output)
bipartite_dir <- file.path(resultpath, "Bipartition_CandidateCandidate_output")
input_file <- file.path(bipartite_dir, "candidate_candidate_network.rds")

# Output directory
output_dir <- file.path(resultpath, "network_metrics_candidate_candidate")

# Create output directory if it doesn't exist
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ==============================================================================
# 2. LOAD CANDIDATE-CANDIDATE NETWORK
# ==============================================================================

cat("Loading candidate-candidate network...\n")
g_candidate <- readRDS(input_file)

cat(sprintf("✓ Loaded candidate-candidate network: %d nodes, %d edges\n", 
            vcount(g_candidate), ecount(g_candidate)))

# Check for edge weights
if("weight" %in% edge_attr_names(g_candidate)) {
  cat(sprintf("✓ Edge weights present (range: %d to %d)\n\n", 
              min(E(g_candidate)$weight), max(E(g_candidate)$weight)))
} else {
  cat("⚠ No edge weights found\n\n")
}

# ==============================================================================
# 3. BASIC NETWORK METRICS
# ==============================================================================

cat("================================================================================\n")
cat("BASIC NETWORK METRICS\n")
cat("================================================================================\n\n")

# Number of nodes and edges
n_nodes <- vcount(g_candidate)
n_edges <- ecount(g_candidate)

cat(sprintf("Number of Candidates (Nodes): %d\n", n_nodes))
cat(sprintf("Number of Connections (Edges): %d\n\n", n_edges))

# Network density
density <- edge_density(g_candidate)
cat(sprintf("Network Density: %.6f\n", density))
cat("  → Proportion of actual edges to possible edges\n\n")

# Connected components
components_info <- components(g_candidate)
n_components <- components_info$no
cat(sprintf("Number of Connected Components: %d\n", n_components))

if(n_components > 1) {
  cat(sprintf("Largest Component Size: %d nodes (%.1f%% of network)\n", 
              max(components_info$csize),
              100 * max(components_info$csize) / n_nodes))
  cat(sprintf("Smallest Component Size: %d nodes\n\n", min(components_info$csize)))
  
  # For large networks, work with largest component for path metrics
  cat("NOTE: Working with largest component for path-based metrics...\n\n")
  largest_comp_id <- which.max(components_info$csize)
  g_largest <- induced_subgraph(g_candidate, 
                                 which(components_info$membership == largest_comp_id))
  
  # Average path length
  avg_path <- mean_distance(g_largest, directed = FALSE)
  cat(sprintf("Average Path Length (largest component): %.4f\n", avg_path))
  
  # Network diameter
  diam <- diameter(g_largest, directed = FALSE)
  cat(sprintf("Network Diameter (largest component): %d\n\n", diam))
  
} else {
  cat("  → Network is fully connected\n\n")
  
  # Average path length
  avg_path <- mean_distance(g_candidate, directed = FALSE)
  cat(sprintf("Average Path Length: %.4f\n", avg_path))
  
  # Network diameter
  diam <- diameter(g_candidate, directed = FALSE)
  cat(sprintf("Network Diameter: %d\n\n", diam))
}

# Global clustering coefficient
clustering_global <- transitivity(g_candidate, type = "global")
cat(sprintf("Global Clustering Coefficient: %.4f\n", clustering_global))
cat("  → Tendency of candidates to form triangular relationships\n\n")

# Average local clustering coefficient
clustering_avg <- transitivity(g_candidate, type = "average")
cat(sprintf("Average Local Clustering Coefficient: %.4f\n", clustering_avg))
cat("  → Average clustering over all candidates\n\n")

# ==============================================================================
# 4. CENTRALITY MEASURES
# ==============================================================================

cat("================================================================================\n")
cat("CENTRALITY MEASURES\n")
cat("================================================================================\n\n")

cat("Computing centrality measures...\n")
cat("NOTE: This may take several minutes for large networks...\n\n")

# Get candidate names/IDs
candidate_ids <- V(g_candidate)$name

# --- 4.1 DEGREE CENTRALITY ---
cat("--- DEGREE CENTRALITY ---\n")
cat("Number of direct connections each candidate has\n\n")

deg <- degree(g_candidate)
deg_df <- data.frame(
  Candidate_ID = candidate_ids,
  Degree = deg,
  stringsAsFactors = FALSE
)
deg_df <- deg_df[order(-deg_df$Degree), ]

cat("Top 10 Candidates by Degree Centrality:\n")
print(head(deg_df, 10), row.names = FALSE)
cat("\n")

cat(sprintf("Mean Degree: %.2f\n", mean(deg)))
cat(sprintf("Median Degree: %.2f\n", median(deg)))
cat(sprintf("Max Degree: %d (Candidate: %s)\n", max(deg), deg_df$Candidate_ID[1]))
cat(sprintf("Min Degree: %d\n\n", min(deg)))

# --- 4.2 BETWEENNESS CENTRALITY ---
cat("--- BETWEENNESS CENTRALITY ---\n")
cat("How often a candidate lies on shortest paths between other candidates\n")
cat("NOTE: This computation may take significant time for large networks...\n\n")

betw <- betweenness(g_candidate, directed = FALSE, weights = NA)
betw_df <- data.frame(
  Candidate_ID = candidate_ids,
  Betweenness = betw,
  stringsAsFactors = FALSE
)
betw_df <- betw_df[order(-betw_df$Betweenness), ]

cat("Top 10 Candidates by Betweenness Centrality:\n")
print(head(betw_df, 10), row.names = FALSE)
cat("\n")

cat(sprintf("Mean Betweenness: %.2f\n", mean(betw)))
cat(sprintf("Median Betweenness: %.2f\n", median(betw)))
cat(sprintf("Max Betweenness: %.2f (Candidate: %s)\n", max(betw), betw_df$Candidate_ID[1]))
cat(sprintf("Min Betweenness: %.2f\n\n", min(betw)))

# --- 4.3 CLOSENESS CENTRALITY ---
cat("--- CLOSENESS CENTRALITY ---\n")
cat("How close a candidate is to all other candidates in the network\n\n")

clos <- closeness(g_candidate, mode = "all", normalized = TRUE)
clos_df <- data.frame(
  Candidate_ID = candidate_ids,
  Closeness = clos,
  stringsAsFactors = FALSE
)
clos_df <- clos_df[order(-clos_df$Closeness), ]

cat("Top 10 Candidates by Closeness Centrality:\n")
print(head(clos_df, 10), row.names = FALSE)
cat("\n")

cat(sprintf("Mean Closeness: %.6f\n", mean(clos)))
cat(sprintf("Median Closeness: %.6f\n", median(clos)))
cat(sprintf("Max Closeness: %.6f (Candidate: %s)\n", max(clos), clos_df$Candidate_ID[1]))
cat(sprintf("Min Closeness: %.6f\n\n", min(clos)))

# --- 4.4 EIGENVECTOR CENTRALITY ---
cat("--- EIGENVECTOR CENTRALITY ---\n")
cat("Influence based on connections to other influential candidates\n\n")

eigen <- eigen_centrality(g_candidate, directed = FALSE, weights = NA)$vector
eigen_df <- data.frame(
  Candidate_ID = candidate_ids,
  Eigenvector = eigen,
  stringsAsFactors = FALSE
)
eigen_df <- eigen_df[order(-eigen_df$Eigenvector), ]

cat("Top 10 Candidates by Eigenvector Centrality:\n")
print(head(eigen_df, 10), row.names = FALSE)
cat("\n")

cat(sprintf("Mean Eigenvector: %.6f\n", mean(eigen)))
cat(sprintf("Median Eigenvector: %.6f\n", median(eigen)))
cat(sprintf("Max Eigenvector: %.6f (Candidate: %s)\n", max(eigen), eigen_df$Candidate_ID[1]))
cat(sprintf("Min Eigenvector: %.6f\n\n", min(eigen)))

# --- 4.5 PAGERANK CENTRALITY ---
cat("--- PAGERANK CENTRALITY ---\n")
cat("Google's algorithm - importance considering quality of connections\n\n")

pr <- page_rank(g_candidate, directed = FALSE, weights = NA)$vector
pr_df <- data.frame(
  Candidate_ID = candidate_ids,
  PageRank = pr,
  stringsAsFactors = FALSE
)
pr_df <- pr_df[order(-pr_df$PageRank), ]

cat("Top 10 Candidates by PageRank Centrality:\n")
print(head(pr_df, 10), row.names = FALSE)
cat("\n")

cat(sprintf("Mean PageRank: %.6f\n", mean(pr)))
cat(sprintf("Median PageRank: %.6f\n", median(pr)))
cat(sprintf("Max PageRank: %.6f (Candidate: %s)\n", max(pr), pr_df$PageRank[1]))
cat(sprintf("Min PageRank: %.6f\n\n", min(pr)))

# --- 4.6 ECCENTRICITY ---
cat("--- ECCENTRICITY ---\n")
cat("Maximum distance from each candidate to all other candidates\n\n")

ecc <- eccentricity(g_candidate)
ecc_df <- data.frame(
  Candidate_ID = candidate_ids,
  Eccentricity = ecc,
  stringsAsFactors = FALSE
)
ecc_df <- ecc_df[order(ecc_df$Eccentricity), ]  # Sort ascending (lower is better)

cat("Top 10 Candidates by Eccentricity (lowest values = most central):\n")
print(head(ecc_df, 10), row.names = FALSE)
cat("\n")

cat(sprintf("Mean Eccentricity: %.2f\n", mean(ecc)))
cat(sprintf("Median Eccentricity: %.2f\n", median(ecc)))
cat(sprintf("Max Eccentricity: %d (Most peripheral: %s)\n", 
            max(ecc), ecc_df$Candidate_ID[nrow(ecc_df)]))
cat(sprintf("Min Eccentricity: %d (Most central: %s)\n\n", 
            min(ecc), ecc_df$Candidate_ID[1]))

# ==============================================================================
# 5. DEGREE DISTRIBUTION ANALYSIS
# ==============================================================================

cat("================================================================================\n")
cat("DEGREE DISTRIBUTION ANALYSIS\n")
cat("================================================================================\n\n")

# Degree distribution
deg_dist <- degree_distribution(g_candidate)
deg_table <- table(deg)

cat("Degree Distribution (first 20 values):\n")
print(head(as.data.frame(deg_table), 20))
cat("\n")

cat(sprintf("Mean Degree: %.2f\n", mean(deg)))
cat(sprintf("Median Degree: %.0f\n", median(deg)))
cat(sprintf("Degree Range: %d to %d\n", min(deg), max(deg)))
cat(sprintf("Standard Deviation: %.2f\n\n", sd(deg)))

# ==============================================================================
# 6. SAVE RESULTS
# ==============================================================================

cat("================================================================================\n")
cat("SAVING RESULTS\n")
cat("================================================================================\n\n")

# Save network metrics summary
metrics_summary <- data.frame(
  Metric = c("Number of Candidates", "Number of Edges", "Network Density", 
             "Average Path Length", "Network Diameter", 
             "Global Clustering Coefficient", "Average Local Clustering Coefficient",
             "Number of Components", "Largest Component Size"),
  Value = c(n_nodes, n_edges, density, avg_path, diam, 
            clustering_global, clustering_avg, 
            n_components, max(components_info$csize)),
  stringsAsFactors = FALSE
)

write.csv(metrics_summary, 
          file.path(output_dir, "network_metrics_summary.csv"),
          row.names = FALSE)
cat("✓ Saved network metrics summary\n")

# Combine all centrality measures into one dataframe
centrality_combined <- data.frame(
  Candidate_ID = candidate_ids,
  Degree = deg,
  Betweenness = betw,
  Closeness = clos,
  Eigenvector = eigen,
  PageRank = pr,
  Eccentricity = ecc,
  stringsAsFactors = FALSE
)

write.csv(centrality_combined,
          file.path(output_dir, "centrality_scores_all.csv"),
          row.names = FALSE)
cat("✓ Saved combined centrality scores\n")

# Save individual centrality rankings
write.csv(deg_df, file.path(output_dir, "degree_centrality.csv"), row.names = FALSE)
write.csv(betw_df, file.path(output_dir, "betweenness_centrality.csv"), row.names = FALSE)
write.csv(clos_df, file.path(output_dir, "closeness_centrality.csv"), row.names = FALSE)
write.csv(eigen_df, file.path(output_dir, "eigenvector_centrality.csv"), row.names = FALSE)
write.csv(pr_df, file.path(output_dir, "pagerank_centrality.csv"), row.names = FALSE)
write.csv(ecc_df, file.path(output_dir, "eccentricity.csv"), row.names = FALSE)
cat("✓ Saved individual centrality rankings\n")

# Save degree distribution
deg_dist_df <- data.frame(
  Degree = as.numeric(names(deg_table)),
  Count = as.numeric(deg_table),
  stringsAsFactors = FALSE
)
write.csv(deg_dist_df,
          file.path(output_dir, "degree_distribution.csv"),
          row.names = FALSE)
cat("✓ Saved degree distribution\n")

cat("\n")

# ==============================================================================
# 7. FINAL SUMMARY
# ==============================================================================

cat("================================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("================================================================================\n\n")

cat("KEY FINDINGS:\n\n")

cat(sprintf("• Network has %d candidates with %d connections\n", n_nodes, n_edges))
cat(sprintf("• Network density: %.6f\n", density))
cat(sprintf("• Average path length: %.2f steps\n", avg_path))
cat(sprintf("• Network diameter: %d steps\n", diam))
cat(sprintf("• Clustering coefficient: %.4f\n\n", clustering_global))

cat("MOST CENTRAL CANDIDATES:\n")
cat(sprintf("  Degree: %s (%d connections)\n", deg_df$Candidate_ID[1], deg_df$Degree[1]))
cat(sprintf("  Betweenness: %s (%.2f)\n", betw_df$Candidate_ID[1], betw_df$Betweenness[1]))
cat(sprintf("  Closeness: %s (%.6f)\n", clos_df$Candidate_ID[1], clos_df$Closeness[1]))
cat(sprintf("  Eigenvector: %s (%.6f)\n", eigen_df$Candidate_ID[1], eigen_df$Eigenvector[1]))
cat(sprintf("  PageRank: %s (%.6f)\n", pr_df$Candidate_ID[1], pr_df$PageRank[1]))
cat(sprintf("  Eccentricity: %s (%d steps)\n\n", ecc_df$Candidate_ID[1], ecc_df$Eccentricity[1]))

cat("FILES SAVED TO:", output_dir, "\n")
cat("  • network_metrics_summary.csv\n")
cat("  • centrality_scores_all.csv\n")
cat("  • degree_centrality.csv\n")
cat("  • betweenness_centrality.csv\n")
cat("  • closeness_centrality.csv\n")
cat("  • eigenvector_centrality.csv\n")
cat("  • pagerank_centrality.csv\n")
cat("  • eccentricity.csv\n")
cat("  • degree_distribution.csv\n\n")

cat("✓ Ready for visualization!\n\n")

cat("================================================================================\n")