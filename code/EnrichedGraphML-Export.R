# ==============================================================================
# Social Network Analysis - Pakistan Elections
# Script: Enriched GraphML Export - Separate Files for Each Metric
# ==============================================================================
# Authors: Hammad Malik (hm08298) & Mehlab Kashani (mk07950)
# Course: CS/SDP 361/352
# Date: December 7, 2025
# Description: Creates separate GraphML files for each metric:
#              - Betweenness Centrality
#              - Degree Centrality
#              - Louvain Communities
#              - Clustering Coefficient
# ==============================================================================

# --- Load Required Libraries ---
# library(igraph)

cat("================================================================================\n")
cat("ENRICHED GRAPHML EXPORT\n")
cat("Creating separate GraphML files with individual metrics:\n")
cat("  • Betweenness Centrality\n")
cat("  • Degree Centrality\n")
cat("  • Louvain Communities\n")
cat("  • Clustering Coefficient\n")
cat("================================================================================\n\n")

# ==============================================================================
# 1. CONFIGURATION
# ==============================================================================

# Set your data paths here
resultpath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/results"

# Input files
network_file <- file.path(resultpath, "network_metrics_party_party/party_party_network.rds")
betweenness_file <- file.path(resultpath, "network_metrics_party_party/betweenness_centrality.csv")
degree_file <- file.path(resultpath, "network_metrics_party_party/degree_centrality.csv")
louvain_file <- file.path(resultpath, "visualizations_party_party/louvain_communities.csv")
clustering_file <- file.path(resultpath, "visualizations_party_party/clustering_coefficient.csv")

# Output files
output_dir <- file.path(resultpath, "network_metrics_party_party")
output_betweenness <- file.path(output_dir, "network_with_betweenness.graphml")
output_degree <- file.path(output_dir, "network_with_degree.graphml")
output_louvain <- file.path(output_dir, "network_with_louvain.graphml")
output_clustering <- file.path(output_dir, "network_with_clustering.graphml")

# ==============================================================================
# 2. LOAD NETWORK
# ==============================================================================

cat("Loading party-party network...\n")
g_party <- readRDS(network_file)

cat(sprintf("✓ Loaded network: %d nodes, %d edges\n\n",
            vcount(g_party), ecount(g_party)))

# ==============================================================================
# 3. LOAD METRICS FROM CSV FILES
# ==============================================================================

cat("Loading metrics from CSV files...\n")

# Load betweenness centrality
betweenness_df <- read.csv(betweenness_file, stringsAsFactors = FALSE)
cat(sprintf("✓ Loaded betweenness centrality for %d parties\n", nrow(betweenness_df)))

# Load degree centrality
degree_df <- read.csv(degree_file, stringsAsFactors = FALSE)
cat(sprintf("✓ Loaded degree centrality for %d parties\n", nrow(degree_df)))

# Load Louvain communities
louvain_df <- read.csv(louvain_file, stringsAsFactors = FALSE)
cat(sprintf("✓ Loaded Louvain communities for %d parties\n", nrow(louvain_df)))

# Load clustering coefficient
clustering_df <- read.csv(clustering_file, stringsAsFactors = FALSE)
cat(sprintf("✓ Loaded clustering coefficients for %d parties\n", nrow(clustering_df)))

cat("\n")

# ==============================================================================
# 4. CREATE SEPARATE GRAPHS WITH INDIVIDUAL METRICS
# ==============================================================================

cat("Creating separate network files with individual metrics...\n\n")

# Get party names from the network
party_names <- V(g_party)$name

# --- 4.1 BETWEENNESS CENTRALITY GRAPH ---
cat("1. Creating network with BETWEENNESS CENTRALITY...\n")
g_betweenness <- g_party

betweenness_values <- numeric(vcount(g_betweenness))
for(i in 1:vcount(g_betweenness)) {
  party <- party_names[i]
  match_idx <- match(party, betweenness_df$Party)
  if(!is.na(match_idx)) {
    betweenness_values[i] <- betweenness_df$Betweenness[match_idx]
  }
}
V(g_betweenness)$betweenness <- betweenness_values

cat(sprintf("   ✓ Added betweenness centrality for %d nodes\n", vcount(g_betweenness)))
cat(sprintf("   ✓ Range: %.4f to %.4f\n", min(betweenness_values), max(betweenness_values)))

write_graph(g_betweenness, output_betweenness, format = "graphml")
cat(sprintf("   ✓ Saved: network_with_betweenness.graphml\n\n"))

# --- 4.2 DEGREE CENTRALITY GRAPH ---
cat("2. Creating network with DEGREE CENTRALITY...\n")
g_degree <- g_party

degree_values <- numeric(vcount(g_degree))
for(i in 1:vcount(g_degree)) {
  party <- party_names[i]
  match_idx <- match(party, degree_df$Party)
  if(!is.na(match_idx)) {
    degree_values[i] <- degree_df$Degree[match_idx]
  }
}
V(g_degree)$degree_centrality <- degree_values

cat(sprintf("   ✓ Added degree centrality for %d nodes\n", vcount(g_degree)))
cat(sprintf("   ✓ Range: %d to %d\n", min(degree_values), max(degree_values)))

write_graph(g_degree, output_degree, format = "graphml")
cat(sprintf("   ✓ Saved: network_with_degree.graphml\n\n"))

# --- 4.3 LOUVAIN COMMUNITY GRAPH ---
cat("3. Creating network with LOUVAIN COMMUNITIES...\n")
g_louvain <- g_party

louvain_values <- numeric(vcount(g_louvain))
for(i in 1:vcount(g_louvain)) {
  party <- party_names[i]
  match_idx <- match(party, louvain_df$Party)
  if(!is.na(match_idx)) {
    louvain_values[i] <- louvain_df$Community[match_idx]
  }
}
V(g_louvain)$louvain_community <- louvain_values

cat(sprintf("   ✓ Added Louvain community for %d nodes\n", vcount(g_louvain)))
cat(sprintf("   ✓ Number of communities: %d\n", length(unique(louvain_values))))

write_graph(g_louvain, output_louvain, format = "graphml")
cat(sprintf("   ✓ Saved: network_with_louvain.graphml\n\n"))

# --- 4.4 CLUSTERING COEFFICIENT GRAPH ---
cat("4. Creating network with CLUSTERING COEFFICIENT...\n")
g_clustering <- g_party

clustering_values <- numeric(vcount(g_clustering))
for(i in 1:vcount(g_clustering)) {
  party <- party_names[i]
  match_idx <- match(party, clustering_df$Party)
  if(!is.na(match_idx)) {
    clustering_values[i] <- clustering_df$Clustering[match_idx]
  }
}
V(g_clustering)$clustering_coefficient <- clustering_values

cat(sprintf("   ✓ Added clustering coefficient for %d nodes\n", vcount(g_clustering)))
cat(sprintf("   ✓ Range: %.4f to %.4f\n", min(clustering_values), max(clustering_values)))

write_graph(g_clustering, output_clustering, format = "graphml")
cat(sprintf("   ✓ Saved: network_with_clustering.graphml\n\n"))

# ==============================================================================
# 5. SUMMARY
# ==============================================================================

cat("================================================================================\n")
cat("EXPORT COMPLETE!\n")
cat("================================================================================\n\n")

cat("Created 4 separate GraphML files:\n\n")

cat("1. network_with_betweenness.graphml\n")
cat("   • Contains: betweenness centrality attribute\n")
cat(sprintf("   • Range: %.4f to %.4f\n\n", min(betweenness_values), max(betweenness_values)))

cat("2. network_with_degree.graphml\n")
cat("   • Contains: degree_centrality attribute\n")
cat(sprintf("   • Range: %d to %d\n\n", min(degree_values), max(degree_values)))

cat("3. network_with_louvain.graphml\n")
cat("   • Contains: louvain_community attribute\n")
cat(sprintf("   • Number of communities: %d\n\n", length(unique(louvain_values))))

cat("4. network_with_clustering.graphml\n")
cat("   • Contains: clustering_coefficient attribute\n")
cat(sprintf("   • Range: %.4f to %.4f\n\n", min(clustering_values), max(clustering_values)))

cat("All files also include:\n")
cat("  • Original node attributes (name, Name, Type, label)\n")
cat("  • Edge weights\n\n")

cat("These files can be imported into:\n")
cat("  • Gephi (for visualization and further analysis)\n")
cat("  • Cytoscape (for biological/network analysis)\n")
cat("  • NetworkX (Python library)\n")
cat("  • Any tool that supports GraphML format\n\n")

cat("Output location:\n")
cat(sprintf("  %s/\n", output_dir))
cat("    • network_with_betweenness.graphml\n")
cat("    • network_with_degree.graphml\n")
cat("    • network_with_louvain.graphml\n")
cat("    • network_with_clustering.graphml\n\n")

cat("✓ Ready for visualization and analysis!\n\n")
cat("================================================================================\n")
