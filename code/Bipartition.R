# ==============================================================================
# Social Network Analysis - Pakistan Elections
# Script 02: Bipartite Network Construction (SIMPLE VERSION)
# ==============================================================================
# Authors: Hammad Malik (hm08298) & Mehlab Kashani (mk07950)
# Course: CS/SDP 361/352
# Date: October 31, 2025
# Target Years: 2008, 2013, 2024
# ==============================================================================

# --- Load Required Libraries ---
library(igraph)

# ==============================================================================
# 1. CONFIGURATION
# ==============================================================================

cat("================================================================================\n")
cat("BIPARTITE NETWORK CONSTRUCTION\n")
cat("================================================================================\n\n")

# Set your data path here
datapath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/dataset"
resultpath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/results"
setwd(datapath)

# Input and output files
input_file <- "cleaned_elections_2008_2013_2024.csv"
output_dir <- file.path(resultpath, "Bipartition.R output")

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
# 3. CREATE EDGE LIST WITH WEIGHTS
# ==============================================================================

cat("Creating edge list...\n")

# Create edge data
edge_data <- data.frame(
  Candidate_ID = elections$Candidate_ID,
  Candidate_Name = elections$Candidate_Name,
  Party = elections$Party,
  Year = elections$Year,
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
# 4. IDENTIFY PARTY SWITCHERS
# ==============================================================================

cat("Identifying party switchers...\n")

# Count parties per candidate
parties_per_candidate <- aggregate(
  Party ~ Candidate_ID,
  data = edge_weights,
  FUN = function(x) length(unique(x))
)
colnames(parties_per_candidate)[2] <- "Party_Count"

# Identify switchers
switchers <- parties_per_candidate$Candidate_ID[parties_per_candidate$Party_Count > 1]

cat(sprintf("✓ Found %d party switchers (%.1f%% of candidates)\n\n",
            length(switchers),
            100 * length(switchers) / length(unique(edge_weights$Candidate_ID))))

# ==============================================================================
# 5. CREATE NODE LISTS
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
# 6. BUILD BIPARTITE GRAPH
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
g <- graph_from_data_frame(
  d = edges_for_graph,
  vertices = all_nodes,
  directed = FALSE
)

# Set bipartite attribute
V(g)$type <- V(g)$Type == "Candidate"
V(g)$label <- V(g)$Name

cat(sprintf("✓ Graph created: %d nodes, %d edges\n", vcount(g), ecount(g)))
cat(sprintf("✓ Bipartite: %s\n\n", is_bipartite(g)))

# ==============================================================================
# 7. CREATE TEMPORAL NETWORKS
# ==============================================================================

cat("Creating temporal networks...\n")

create_year_network <- function(year, edge_data) {
  year_edges <- edge_data[edge_data$Year == year, ]
  
  year_edge_list <- unique(data.frame(
    from = year_edges$Candidate_ID,
    to = year_edges$Party,
    stringsAsFactors = FALSE
  ))
  
  year_nodes <- data.frame(
    ID = c(unique(year_edges$Candidate_ID), unique(year_edges$Party)),
    Name = c(
      sapply(unique(year_edges$Candidate_ID), 
             function(x) year_edges$Candidate_Name[year_edges$Candidate_ID == x][1]),
      unique(year_edges$Party)
    ),
    Type = c(
      rep("Candidate", length(unique(year_edges$Candidate_ID))),
      rep("Party", length(unique(year_edges$Party)))
    ),
    stringsAsFactors = FALSE
  )
  
  g_year <- graph_from_data_frame(d = year_edge_list, vertices = year_nodes, directed = FALSE)
  V(g_year)$type <- V(g_year)$Type == "Candidate"
  V(g_year)$label <- V(g_year)$Name
  
  return(g_year)
}

g_2008 <- create_year_network(2008, edge_data)
g_2013 <- create_year_network(2013, edge_data)
g_2024 <- create_year_network(2024, edge_data)

cat(sprintf("✓ 2008: %d nodes, %d edges\n", vcount(g_2008), ecount(g_2008)))
cat(sprintf("✓ 2013: %d nodes, %d edges\n", vcount(g_2013), ecount(g_2013)))
cat(sprintf("✓ 2024: %d nodes, %d edges\n\n", vcount(g_2024), ecount(g_2024)))

# ==============================================================================
# 8. SAVE OUTPUTS
# ==============================================================================

cat("Saving outputs...\n")

# Save igraph objects
saveRDS(g, file.path(output_dir, "bipartite_network_full.rds"))
saveRDS(g_2008, file.path(output_dir, "bipartite_network_2008.rds"))
saveRDS(g_2013, file.path(output_dir, "bipartite_network_2013.rds"))
saveRDS(g_2024, file.path(output_dir, "bipartite_network_2024.rds"))

# Save edge list
write.csv(edge_weights, 
          file.path(output_dir, "edge_list_weighted.csv"),
          row.names = FALSE)

# Save node lists
write.csv(candidate_nodes, 
          file.path(output_dir, "candidate_nodes.csv"),
          row.names = FALSE)
write.csv(party_nodes, 
          file.path(output_dir, "party_nodes.csv"),
          row.names = FALSE)

# Save GraphML for Gephi
write_graph(g, file.path(output_dir, "bipartite_network_full.graphml"), format = "graphml")
write_graph(g_2008, file.path(output_dir, "bipartite_network_2008.graphml"), format = "graphml")
write_graph(g_2013, file.path(output_dir, "bipartite_network_2013.graphml"), format = "graphml")
write_graph(g_2024, file.path(output_dir, "bipartite_network_2024.graphml"), format = "graphml")

cat("✓ All files saved\n\n")

# ==============================================================================
# 9. SUMMARY
# ==============================================================================

cat("================================================================================\n")
cat("COMPLETE!\n")
cat("================================================================================\n\n")

cat("PARTY SWITCHERS:\n")
cat(sprintf("  %d candidates switched parties (%.1f%%)\n\n",
            length(switchers),
            100 * length(switchers) / nrow(candidate_nodes)))

cat("FILES SAVED TO:", output_dir, "\n")
cat("  - bipartite_network_full.rds/graphml\n")
cat("  - bipartite_network_2008.rds/graphml\n")
cat("  - bipartite_network_2013.rds/graphml\n")
cat("  - bipartite_network_2024.rds/graphml\n")
cat("  - edge_list_weighted.csv\n")
cat("  - candidate_nodes.csv\n")
cat("  - party_nodes.csv\n\n")

cat("✓ Ready for analysis!\n\n")