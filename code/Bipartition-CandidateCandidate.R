# ==============================================================================
# Social Network Analysis - Pakistan Elections
# Script: Bipartite Network Construction for CANDIDATE-CANDIDATE Projection (FIXED)
# ==============================================================================
# Authors: Hammad Malik (hm08298) & Mehlab Kashani (mk07950)
# Course: CS/SDP 361/352
# Date: November 2, 2025
# Target Years: 2008, 2013, 2024
# Network Type: Candidate-Candidate Unipartite Projection
# FIX: Properly handles Independent candidates
# ==============================================================================

# --- Load Required Libraries ---
install.packages("igraph", dependencies=TRUE)
library(igraph)

# ==============================================================================
# 1. CONFIGURATION
# ==============================================================================

cat("================================================================================\n")
cat("BIPARTITE NETWORK CONSTRUCTION - CANDIDATE-CANDIDATE PROJECTION (FIXED)\n")
cat("================================================================================\n\n")

# Set your data path here
datapath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/dataset"
resultpath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/results"
setwd(datapath)

# Input file (use existing cleaned data)
input_file <- "cleaned_elections_2008_2013_2024.csv"

# Output directory
output_dir <- file.path(resultpath, "Bipartition_CandidateCandidate_output")

# Create output directory if it doesn't exist
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ==============================================================================
# 2. LOAD DATA
# ==============================================================================

cat("Loading data...\n")
elections <- read.csv(input_file, stringsAsFactors = FALSE)
cat(sprintf("✓ Loaded %d records\n\n", nrow(elections)))

# ==============================================================================
# 3. HANDLE INDEPENDENT CANDIDATES
# ==============================================================================

cat("================================================================================\n")
cat("HANDLING INDEPENDENT CANDIDATES\n")
cat("================================================================================\n\n")

cat("Analyzing party distribution...\n")
party_counts <- table(elections$Party)
ind_count <- sum(elections$Party == "IND")
total_count <- nrow(elections)

cat(sprintf("Total records: %d\n", total_count))
cat(sprintf("Independent (IND) records: %d (%.1f%%)\n\n", 
            ind_count, 100 * ind_count / total_count))

cat("IMPORTANT DECISION: How to handle Independent candidates?\n\n")
cat("Option 1: EXCLUDE Independents (recommended for party-based connections)\n")
cat("  → Removes all IND candidates from analysis\n")
cat("  → Network shows connections through actual political parties only\n")
cat("  → More meaningful for party loyalty/switching analysis\n\n")

cat("Option 2: KEEP Independents with unique IDs\n")
cat("  → Each independent gets unique party ID (IND_candidateID)\n")
cat("  → Prevents massive clique of all independents\n")
cat("  → Allows including independents in analysis\n\n")

# CHOOSE YOUR OPTION HERE:
EXCLUDE_INDEPENDENTS <- TRUE  # Set to FALSE if you want to keep them with unique IDs

if(EXCLUDE_INDEPENDENTS) {
  cat(">>> SELECTED: OPTION 1 - Excluding Independent candidates\n\n")
  
  # Filter out independents
  elections_filtered <- elections[elections$Party != "IND", ]
  
  cat(sprintf("Records after filtering: %d (removed %d IND records)\n\n", 
              nrow(elections_filtered), nrow(elections) - nrow(elections_filtered)))
  
  # Use filtered data
  elections_working <- elections_filtered
  
} else {
  cat(">>> SELECTED: OPTION 2 - Keeping Independents with unique IDs\n\n")
  
  # Create unique party IDs for independents
  elections_working <- elections
  
  # For IND candidates, create unique party identifier
  ind_mask <- elections_working$Party == "IND"
  elections_working$Party[ind_mask] <- paste0("IND_", elections_working$Candidate_ID[ind_mask])
  
  cat(sprintf("Converted %d IND entries to unique party IDs\n\n", sum(ind_mask)))
}

# ==============================================================================
# 4. CREATE EDGE LIST WITH WEIGHTS
# ==============================================================================

cat("Creating edge list...\n")

# Create edge data (Candidate -> Party connections)
edge_data <- data.frame(
  Candidate_ID = elections_working$Candidate_ID,
  Candidate_Name = elections_working$Candidate_Name,
  Party = elections_working$Party,
  Year = elections_working$Year,
  stringsAsFactors = FALSE
)

# Calculate edge weights (number of elections candidate contested with each party)
edge_weights <- aggregate(
  Year ~ Candidate_ID + Party,
  data = edge_data,
  FUN = function(x) length(unique(x))
)
colnames(edge_weights)[3] <- "Weight"

# Add candidate names
edge_weights$Candidate_Name <- sapply(edge_weights$Candidate_ID, function(cid) {
  edge_data$Candidate_Name[edge_data$Candidate_ID == cid][1]
})

cat(sprintf("✓ Created %d unique candidate-party edges\n\n", nrow(edge_weights)))

# ==============================================================================
# 5. IDENTIFY PARTY SWITCHERS
# ==============================================================================

cat("Identifying party switchers...\n")

# Count parties per candidate (excluding IND_* unique parties)
if(EXCLUDE_INDEPENDENTS) {
  # Simple count - no independents
  parties_per_candidate <- aggregate(
    Party ~ Candidate_ID,
    data = edge_weights,
    FUN = function(x) length(unique(x))
  )
} else {
  # For unique IND parties, don't count them as different parties
  parties_per_candidate <- aggregate(
    Party ~ Candidate_ID,
    data = edge_weights,
    FUN = function(x) {
      # Count non-IND parties + 1 if any IND party exists
      non_ind <- x[!grepl("^IND_", x)]
      has_ind <- any(grepl("^IND_", x))
      return(length(unique(non_ind)) + as.numeric(has_ind))
    }
  )
}

colnames(parties_per_candidate)[2] <- "Party_Count"

# Identify switchers (candidates who contested for multiple parties)
switchers <- parties_per_candidate$Candidate_ID[parties_per_candidate$Party_Count > 1]

cat(sprintf("✓ Found %d party switchers (%.1f%% of candidates)\n\n",
            length(switchers),
            100 * length(switchers) / length(unique(edge_weights$Candidate_ID))))

# ==============================================================================
# 6. CREATE NODE LISTS
# ==============================================================================

cat("Creating node lists...\n")

# Candidate nodes
candidate_nodes <- data.frame(
  ID = unique(edge_weights$Candidate_ID),
  Name = sapply(unique(edge_weights$Candidate_ID), function(cid) {
    edge_weights$Candidate_Name[edge_weights$Candidate_ID == cid][1]
  }),
  Type = "Candidate",
  Num_Parties = sapply(unique(edge_weights$Candidate_ID), function(cid) {
    sum(edge_weights$Candidate_ID == cid)
  }),
  Is_Switcher = unique(edge_weights$Candidate_ID) %in% switchers,
  stringsAsFactors = FALSE
)

# Party nodes
party_nodes <- data.frame(
  ID = unique(edge_weights$Party),
  Name = unique(edge_weights$Party),
  Type = "Party",
  Num_Candidates = sapply(unique(edge_weights$Party), function(pid) {
    sum(edge_weights$Party == pid)
  }),
  stringsAsFactors = FALSE
)

cat(sprintf("✓ Created %d candidate nodes\n", nrow(candidate_nodes)))
cat(sprintf("✓ Created %d party nodes\n\n", nrow(party_nodes)))

# ==============================================================================
# 7. BUILD BIPARTITE GRAPH
# ==============================================================================

cat("Building bipartite graph...\n")

# Prepare edge list
edges_for_graph <- data.frame(
  from = edge_weights$Candidate_ID,
  to = edge_weights$Party,
  weight = edge_weights$Weight,
  stringsAsFactors = FALSE
)

# Combine nodes
all_nodes <- rbind(
  data.frame(
    ID = candidate_nodes$ID,
    Name = candidate_nodes$Name,
    Type = candidate_nodes$Type,
    stringsAsFactors = FALSE
  ),
  data.frame(
    ID = party_nodes$ID,
    Name = party_nodes$Name,
    Type = party_nodes$Type,
    stringsAsFactors = FALSE
  )
)

# Create graph
g_bipartite <- graph_from_data_frame(
  d = edges_for_graph,
  vertices = all_nodes,
  directed = FALSE
)

# Set bipartite attribute
V(g_bipartite)$type <- V(g_bipartite)$Type == "Candidate"
V(g_bipartite)$label <- V(g_bipartite)$Name

cat(sprintf("✓ Bipartite graph created: %d nodes, %d edges\n", 
            vcount(g_bipartite), ecount(g_bipartite)))
cat(sprintf("✓ Bipartite: %s\n\n", is_bipartite(g_bipartite)))

# ==============================================================================
# 8. PROJECT TO CANDIDATE-CANDIDATE NETWORK
# ==============================================================================

cat("================================================================================\n")
cat("PROJECTING TO CANDIDATE-CANDIDATE NETWORK\n")
cat("================================================================================\n\n")

cat("Creating candidate-candidate projection...\n")
cat("NOTE: This may take several minutes for large networks...\n\n")

# Get the bipartite projection
projection <- bipartite_projection(g_bipartite, multiplicity = TRUE)

# Check which projection has more nodes (should be candidates)
proj1_nodes <- vcount(projection$proj1)
proj2_nodes <- vcount(projection$proj2)

cat(sprintf("  Projection 1: %d nodes, %d edges\n", 
            proj1_nodes, ecount(projection$proj1)))
cat(sprintf("  Projection 2: %d nodes, %d edges\n", 
            proj2_nodes, ecount(projection$proj2)))

# Candidates should be the larger set
if(proj1_nodes > proj2_nodes) {
  cat("\n✓ Using Projection 1 (more nodes = candidates)\n")
  g_candidate <- projection$proj1
} else {
  cat("\n✓ Using Projection 2 (more nodes = candidates)\n")
  g_candidate <- projection$proj2
}

cat(sprintf("\n✓ Candidate-candidate network: %d nodes, %d edges\n", 
            vcount(g_candidate), ecount(g_candidate)))

# Check if weights were created
if("weight" %in% edge_attr_names(g_candidate)) {
  cat(sprintf("✓ Edge weights present (range: %d to %d)\n", 
              min(E(g_candidate)$weight), max(E(g_candidate)$weight)))
  cat("  → Weight = number of shared parties between candidates\n")
} else {
  cat("⚠ No edge weights found - setting all weights to 1\n")
  E(g_candidate)$weight <- 1
}

cat("\n")

# ==============================================================================
# 9. CALCULATE BASIC NETWORK METRICS
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
cat(sprintf("Network Density: %.6f (%.2f%%)\n", density, density * 100))
cat("  → Proportion of actual edges to possible edges\n\n")

# Connected components
components_info <- components(g_candidate)
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

# For very large networks, calculate metrics on largest component only
if(n_components > 1) {
  cat("NOTE: Network has multiple components.\n")
  cat("For path length and diameter calculations, using largest component only.\n\n")
  
  # Extract largest component
  largest_comp_id <- which.max(components_info$csize)
  g_largest <- induced_subgraph(g_candidate, 
                                 which(components_info$membership == largest_comp_id))
  
  # Average path length (on largest component)
  avg_path <- mean_distance(g_largest, directed = FALSE)
  cat(sprintf("Average Path Length (largest component): %.4f\n", avg_path))
  
  # Network diameter (on largest component)
  diam <- diameter(g_largest, directed = FALSE)
  cat(sprintf("Network Diameter (largest component): %d\n", diam))
} else {
  # Average path length
  avg_path <- mean_distance(g_candidate, directed = FALSE)
  cat(sprintf("Average Path Length: %.4f\n", avg_path))
  
  # Network diameter
  diam <- diameter(g_candidate, directed = FALSE)
  cat(sprintf("Network Diameter: %d\n", diam))
}

cat("\n")

# Global clustering coefficient
clustering_global <- transitivity(g_candidate, type = "global")
cat(sprintf("Global Clustering Coefficient: %.4f\n", clustering_global))
cat("  → Tendency of candidates to form triangular relationships\n\n")

# ==============================================================================
# 10. SAVE OUTPUTS
# ==============================================================================

cat("================================================================================\n")
cat("SAVING OUTPUTS\n")
cat("================================================================================\n\n")

cat("Saving candidate-candidate network...\n")

# Save igraph object
saveRDS(g_candidate, file.path(output_dir, "candidate_candidate_network.rds"))
cat("✓ Saved: candidate_candidate_network.rds\n")

# Save GraphML for Gephi
write_graph(g_candidate, 
            file.path(output_dir, "candidate_candidate_network.graphml"), 
            format = "graphml")
cat("✓ Saved: candidate_candidate_network.graphml\n")

# Save bipartite graph as well
saveRDS(g_bipartite, file.path(output_dir, "bipartite_network_full.rds"))
cat("✓ Saved: bipartite_network_full.rds\n")

write_graph(g_bipartite, 
            file.path(output_dir, "bipartite_network_full.graphml"), 
            format = "graphml")
cat("✓ Saved: bipartite_network_full.graphml\n\n")

# Save node information
candidate_info <- data.frame(
  Candidate_ID = V(g_candidate)$name,
  Candidate_Name = sapply(V(g_candidate)$name, function(cid) {
    idx <- which(candidate_nodes$ID == cid)
    if(length(idx) > 0) candidate_nodes$Name[idx[1]] else cid
  }),
  Degree = degree(g_candidate),
  Is_Switcher = V(g_candidate)$name %in% switchers,
  stringsAsFactors = FALSE
)

write.csv(candidate_info,
          file.path(output_dir, "candidate_nodes_info.csv"),
          row.names = FALSE)
cat("✓ Saved: candidate_nodes_info.csv\n")

# Save party switchers list
switchers_df <- data.frame(
  Candidate_ID = switchers,
  Candidate_Name = sapply(switchers, function(cid) {
    candidate_nodes$Name[candidate_nodes$ID == cid][1]
  }),
  Num_Parties = sapply(switchers, function(cid) {
    sum(edge_weights$Candidate_ID == cid)
  }),
  stringsAsFactors = FALSE
)
switchers_df <- switchers_df[order(-switchers_df$Num_Parties), ]

write.csv(switchers_df,
          file.path(output_dir, "party_switchers.csv"),
          row.names = FALSE)
cat("✓ Saved: party_switchers.csv\n")

# Save network summary
network_summary <- data.frame(
  Metric = c("Number of Candidates", "Number of Edges", "Network Density",
             "Average Path Length", "Network Diameter", 
             "Global Clustering Coefficient",
             "Number of Components", "Largest Component Size",
             "Number of Party Switchers", "Percentage of Switchers",
             "Independents Handling"),
  Value = c(n_nodes, n_edges, sprintf("%.6f (%.2f%%)", density, density * 100),
            sprintf("%.4f", avg_path), diam,
            sprintf("%.4f", clustering_global),
            n_components, max(components_info$csize),
            length(switchers), 
            sprintf("%.1f%%", 100 * length(switchers) / n_nodes),
            ifelse(EXCLUDE_INDEPENDENTS, "Excluded", "Unique IDs")),
  stringsAsFactors = FALSE
)

write.csv(network_summary,
          file.path(output_dir, "network_summary.csv"),
          row.names = FALSE)
cat("✓ Saved: network_summary.csv\n\n")

# ==============================================================================
# 11. SUMMARY
# ==============================================================================

cat("================================================================================\n")
cat("COMPLETE!\n")
cat("================================================================================\n\n")

cat("CANDIDATE-CANDIDATE NETWORK SUMMARY:\n\n")

cat(sprintf("• Network has %d candidates with %d connections\n", n_nodes, n_edges))
cat(sprintf("• Network density: %.6f (%.2f%%)\n", density, density * 100))
cat(sprintf("• Average path length: %.2f steps\n", avg_path))
cat(sprintf("• Network diameter: %d steps\n", diam))
cat(sprintf("• Clustering coefficient: %.4f\n\n", clustering_global))

cat("PARTY SWITCHING:\n")
cat(sprintf("  %d candidates switched parties (%.1f%%)\n\n",
            length(switchers),
            100 * length(switchers) / n_nodes))

cat("EDGE INTERPRETATION:\n")
cat("  → Two candidates are connected if they contested under the same party\n")
cat("  → Edge weight = number of parties they shared\n")
cat("  → Higher weight = more political alignment\n\n")

if(EXCLUDE_INDEPENDENTS) {
  cat("NOTE: Independent (IND) candidates were EXCLUDED from this analysis\n")
  cat("      to focus on party-based connections.\n\n")
} else {
  cat("NOTE: Independent (IND) candidates were given unique party IDs\n")
  cat("      to prevent artificial connections between all independents.\n\n")
}

cat("FILES SAVED TO:", output_dir, "\n")
cat("  - candidate_candidate_network.rds/graphml\n")
cat("  - bipartite_network_full.rds/graphml\n")
cat("  - candidate_nodes_info.csv\n")
cat("  - party_switchers.csv\n")
cat("  - network_summary.csv\n\n")

cat("✓ Ready for centrality analysis!\n\n")

cat("================================================================================\n")