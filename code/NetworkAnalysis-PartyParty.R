# ==============================================================================
# Social Network Analysis - Pakistan Elections
# Script 03: Party-Party Network Metrics Analysis
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
cat("PARTY-PARTY NETWORK METRICS ANALYSIS\n")
cat("================================================================================\n\n")

# Set your data paths here
datapath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/dataset"
resultpath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/results"

# Input file (from Bipartition.R output)
bipartite_dir <- file.path(resultpath, "Bipartition.R output")
input_file <- file.path(bipartite_dir, "bipartite_network_full.rds")

# Output directory
output_dir <- file.path(resultpath, "network_metrics_party_party")

# Create output directory if it doesn't exist
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ==============================================================================
# 2. LOAD BIPARTITE NETWORK
# ==============================================================================

cat("Loading bipartite network...\n")
g_bipartite <- readRDS(input_file)

cat(sprintf("✓ Loaded bipartite network: %d nodes, %d edges\n", 
            vcount(g_bipartite), ecount(g_bipartite)))
cat(sprintf("✓ Bipartite: %s\n\n", is_bipartite(g_bipartite)))

# ==============================================================================
# 3. PROJECT TO PARTY-PARTY NETWORK
# ==============================================================================

cat("================================================================================\n")
cat("PROJECTING TO PARTY-PARTY NETWORK\n")
cat("================================================================================\n\n")

cat("Creating party-party projection...\n")

# Get the bipartite projection
projection <- bipartite_projection(g_bipartite, multiplicity = TRUE)

# Check which projection has fewer nodes (should be parties ~200)
proj1_nodes <- vcount(projection$proj1)
proj2_nodes <- vcount(projection$proj2)

cat(sprintf("  Projection 1: %d nodes, %d edges\n", 
            proj1_nodes, ecount(projection$proj1)))
cat(sprintf("  Projection 2: %d nodes, %d edges\n", 
            proj2_nodes, ecount(projection$proj2)))

# Parties should be the smaller set (~200 vs ~10,000 candidates)
if(proj1_nodes < proj2_nodes) {
  cat("\n✓ Using Projection 1 (fewer nodes = parties)\n")
  g_party <- projection$proj1
} else {
  cat("\n✓ Using Projection 2 (fewer nodes = parties)\n")
  g_party <- projection$proj2
}

cat(sprintf("\n✓ Party-party network: %d nodes, %d edges\n", 
            vcount(g_party), ecount(g_party)))

# Check if weights were created
if("weight" %in% edge_attr_names(g_party)) {
  cat(sprintf("✓ Edge weights present (range: %d to %d)\n", 
              min(E(g_party)$weight), max(E(g_party)$weight)))
} else {
  cat("⚠ No edge weights found - setting all weights to 1\n")
  E(g_party)$weight <- 1
}

cat("\n")

# ==============================================================================
# 4. BASIC NETWORK METRICS
# ==============================================================================

cat("================================================================================\n")
cat("BASIC NETWORK METRICS\n")
cat("================================================================================\n\n")

# Number of nodes and edges
n_nodes <- vcount(g_party)
n_edges <- ecount(g_party)

cat(sprintf("Number of Parties (Nodes): %d\n", n_nodes))
cat(sprintf("Number of Connections (Edges): %d\n\n", n_edges))

# Network density
density <- edge_density(g_party)
cat(sprintf("Network Density: %.4f\n", density))
cat("  → Proportion of actual edges to possible edges\n\n")

# Average path length
avg_path <- mean_distance(g_party, directed = FALSE)
cat(sprintf("Average Path Length: %.4f\n", avg_path))
cat("  → Average shortest path between all party pairs\n\n")

# Network diameter
diam <- diameter(g_party, directed = FALSE)
cat(sprintf("Network Diameter: %d\n", diam))
cat("  → Longest shortest path in the network\n\n")

# Global clustering coefficient
clustering_global <- transitivity(g_party, type = "global")
cat(sprintf("Global Clustering Coefficient: %.4f\n", clustering_global))
cat("  → Tendency of parties to form triangular relationships\n\n")

# Average local clustering coefficient
clustering_avg <- transitivity(g_party, type = "average")
cat(sprintf("Average Local Clustering Coefficient: %.4f\n", clustering_avg))
cat("  → Average clustering over all parties\n\n")

# Connected components
components_info <- components(g_party)
n_components <- components_info$no
cat(sprintf("Number of Connected Components: %d\n", n_components))

if(n_components > 1) {
  cat(sprintf("Largest Component Size: %d nodes (%.1f%% of network)\n", 
              max(components_info$csize),
              100 * max(components_info$csize) / n_nodes))
  cat(sprintf("Smallest Component Size: %d nodes\n", min(components_info$csize)))
} else {
  cat("  → Network is fully connected\n")
}

cat("\n")

# ==============================================================================
# 5. CENTRALITY MEASURES
# ==============================================================================

cat("================================================================================\n")
cat("CENTRALITY MEASURES\n")
cat("================================================================================\n\n")

# Get party names
party_names <- V(g_party)$name

# --- 5.1 DEGREE CENTRALITY ---
cat("--- DEGREE CENTRALITY ---\n")
cat("Number of direct connections each party has\n\n")

deg <- degree(g_party)
deg_df <- data.frame(
  Party = party_names,
  Degree = deg,
  stringsAsFactors = FALSE
)
deg_df <- deg_df[order(-deg_df$Degree), ]

cat("Top 10 Parties by Degree Centrality:\n")
print(head(deg_df, 10), row.names = FALSE)
cat("\n")

cat(sprintf("Mean Degree: %.2f\n", mean(deg)))
cat(sprintf("Median Degree: %.2f\n", median(deg)))
cat(sprintf("Max Degree: %d (Party: %s)\n", max(deg), deg_df$Party[1]))
cat(sprintf("Min Degree: %d\n\n", min(deg)))

# --- 5.2 BETWEENNESS CENTRALITY ---
cat("--- BETWEENNESS CENTRALITY ---\n")
cat("How often a party lies on shortest paths between other parties\n\n")

betw <- betweenness(g_party, directed = FALSE, weights = NA)
betw_df <- data.frame(
  Party = party_names,
  Betweenness = betw,
  stringsAsFactors = FALSE
)
betw_df <- betw_df[order(-betw_df$Betweenness), ]

cat("Top 10 Parties by Betweenness Centrality:\n")
print(head(betw_df, 10), row.names = FALSE)
cat("\n")

cat(sprintf("Mean Betweenness: %.2f\n", mean(betw)))
cat(sprintf("Median Betweenness: %.2f\n", median(betw)))
cat(sprintf("Max Betweenness: %.2f (Party: %s)\n", max(betw), betw_df$Party[1]))
cat(sprintf("Min Betweenness: %.2f\n\n", min(betw)))

# --- 5.3 CLOSENESS CENTRALITY ---
cat("--- CLOSENESS CENTRALITY ---\n")
cat("How close a party is to all other parties in the network\n\n")

clos <- closeness(g_party, mode = "all", normalized = TRUE)
clos_df <- data.frame(
  Party = party_names,
  Closeness = clos,
  stringsAsFactors = FALSE
)
clos_df <- clos_df[order(-clos_df$Closeness), ]

cat("Top 10 Parties by Closeness Centrality:\n")
print(head(clos_df, 10), row.names = FALSE)
cat("\n")

cat(sprintf("Mean Closeness: %.6f\n", mean(clos)))
cat(sprintf("Median Closeness: %.6f\n", median(clos)))
cat(sprintf("Max Closeness: %.6f (Party: %s)\n", max(clos), clos_df$Party[1]))
cat(sprintf("Min Closeness: %.6f\n\n", min(clos)))

# --- 5.4 EIGENVECTOR CENTRALITY ---
cat("--- EIGENVECTOR CENTRALITY ---\n")
cat("Influence based on connections to other influential parties\n\n")

eigen <- eigen_centrality(g_party, directed = FALSE, weights = NA)$vector
eigen_df <- data.frame(
  Party = party_names,
  Eigenvector = eigen,
  stringsAsFactors = FALSE
)
eigen_df <- eigen_df[order(-eigen_df$Eigenvector), ]

cat("Top 10 Parties by Eigenvector Centrality:\n")
print(head(eigen_df, 10), row.names = FALSE)
cat("\n")

cat(sprintf("Mean Eigenvector: %.6f\n", mean(eigen)))
cat(sprintf("Median Eigenvector: %.6f\n", median(eigen)))
cat(sprintf("Max Eigenvector: %.6f (Party: %s)\n", max(eigen), eigen_df$Party[1]))
cat(sprintf("Min Eigenvector: %.6f\n\n", min(eigen)))

# --- 5.5 PAGERANK CENTRALITY ---
cat("--- PAGERANK CENTRALITY ---\n")
cat("Google's algorithm - importance considering quality of connections\n\n")

pr <- page_rank(g_party, directed = FALSE, weights = NA)$vector
pr_df <- data.frame(
  Party = party_names,
  PageRank = pr,
  stringsAsFactors = FALSE
)
pr_df <- pr_df[order(-pr_df$PageRank), ]

cat("Top 10 Parties by PageRank Centrality:\n")
print(head(pr_df, 10), row.names = FALSE)
cat("\n")

cat(sprintf("Mean PageRank: %.6f\n", mean(pr)))
cat(sprintf("Median PageRank: %.6f\n", median(pr)))
cat(sprintf("Max PageRank: %.6f (Party: %s)\n", max(pr), pr_df$Party[1]))
cat(sprintf("Min PageRank: %.6f\n\n", min(pr)))

# --- 5.6 ECCENTRICITY ---
cat("--- ECCENTRICITY ---\n")
cat("Maximum distance from each party to all other parties\n\n")

ecc <- eccentricity(g_party)
ecc_df <- data.frame(
  Party = party_names,
  Eccentricity = ecc,
  stringsAsFactors = FALSE
)
ecc_df <- ecc_df[order(ecc_df$Eccentricity), ]  # Sort ascending (lower is better)

cat("Top 10 Parties by Eccentricity (lowest values):\n")
print(head(ecc_df, 10), row.names = FALSE)
cat("\n")

cat(sprintf("Mean Eccentricity: %.2f\n", mean(ecc)))
cat(sprintf("Median Eccentricity: %.2f\n", median(ecc)))
cat(sprintf("Max Eccentricity: %d (Most peripheral party: %s)\n", 
            max(ecc), ecc_df$Party[nrow(ecc_df)]))
cat(sprintf("Min Eccentricity: %d (Most central party: %s)\n\n", 
            min(ecc), ecc_df$Party[1]))

# ==============================================================================
# 6. DEGREE DISTRIBUTION ANALYSIS
# ==============================================================================

cat("================================================================================\n")
cat("DEGREE DISTRIBUTION ANALYSIS\n")
cat("================================================================================\n\n")

# Degree distribution
deg_dist <- degree_distribution(g_party)
deg_table <- table(deg)

cat("Degree Distribution:\n")
print(as.data.frame(deg_table))
cat("\n")

cat(sprintf("Mean Degree: %.2f\n", mean(deg)))
cat(sprintf("Median Degree: %.0f\n", median(deg)))
cat(sprintf("Degree Range: %d to %d\n", min(deg), max(deg)))
cat(sprintf("Standard Deviation: %.2f\n\n", sd(deg)))

# ==============================================================================
# 7. SAVE RESULTS
# ==============================================================================

cat("================================================================================\n")
cat("SAVING RESULTS\n")
cat("================================================================================\n\n")

# Save projected network
saveRDS(g_party, file.path(output_dir, "party_party_network.rds"))
write_graph(g_party, file.path(output_dir, "party_party_network.graphml"), 
            format = "graphml")
cat("✓ Saved party-party network (RDS and GraphML)\n")

# Save network metrics summary
metrics_summary <- data.frame(
  Metric = c("Number of Parties", "Number of Edges", "Network Density", 
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
  Party = party_names,
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
# 8. FINAL SUMMARY
# ==============================================================================

cat("================================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("================================================================================\n\n")

cat("KEY FINDINGS:\n\n")

cat(sprintf("• Network has %d parties with %d connections\n", n_nodes, n_edges))
cat(sprintf("• Network density: %.2f%% (%.4f)\n", density * 100, density))
cat(sprintf("• Average path length: %.2f steps\n", avg_path))
cat(sprintf("• Network diameter: %d steps\n", diam))
cat(sprintf("• Clustering coefficient: %.2f\n\n", clustering_global))

cat("MOST CENTRAL PARTIES:\n")
cat(sprintf("  Degree: %s (%d connections)\n", deg_df$Party[1], deg_df$Degree[1]))
cat(sprintf("  Betweenness: %s (%.2f)\n", betw_df$Party[1], betw_df$Betweenness[1]))
cat(sprintf("  Closeness: %s (%.6f)\n", clos_df$Party[1], clos_df$Closeness[1]))
cat(sprintf("  Eigenvector: %s (%.6f)\n", eigen_df$Party[1], eigen_df$Eigenvector[1]))
cat(sprintf("  PageRank: %s (%.6f)\n", pr_df$Party[1], pr_df$PageRank[1]))
cat(sprintf("  Eccentricity: %s (%d steps)\n\n", ecc_df$Party[1], ecc_df$Eccentricity[1]))

cat("FILES SAVED TO:", output_dir, "\n")
cat("  • party_party_network.rds/graphml\n")
cat("  • network_metrics_summary.csv\n")
cat("  • centrality_scores_all.csv\n")
cat("  • degree_centrality.csv\n")
cat("  • betweenness_centrality.csv\n")
cat("  • closeness_centrality.csv\n")
cat("  • eigenvector_centrality.csv\n")
cat("  • pagerank_centrality.csv\n")
cat("  • eccentricity.csv\n")
cat("  • degree_distribution.csv\n\n")

cat("✓ Ready for visualization in Gephi!\n\n")

cat("================================================================================\n")