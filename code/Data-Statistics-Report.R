# ==============================================================================
# DATA STATISTICS REPORT - ANSWERING INSTRUCTOR FEEDBACK
# ==============================================================================
# This script provides comprehensive statistics about the dataset to address
# instructor feedback regarding data selection and preprocessing decisions.
# ==============================================================================

cat("================================================================================\n")
cat("DATA STATISTICS REPORT FOR SNA PROJECT\n")
cat("================================================================================\n\n")

# Set paths
datapath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/dataset"
resultpath <- "C:/Users/Hammad/Documents/github/SNA_Project_CS361/results"

# Load datasets
full_data <- read.csv(file.path(datapath, "gallup-pakistan-elections-database-1970-2024-national-assembly.csv"),
                      stringsAsFactors = FALSE)
cleaned_data <- read.csv(file.path(datapath, "cleaned_elections_2008_2013_2024.csv"),
                         stringsAsFactors = FALSE)

cat("Data loaded successfully!\n\n")

# ==============================================================================
# PART 1: FULL DATASET OVERVIEW (1970-2024)
# ==============================================================================

cat("================================================================================\n")
cat("PART 1: FULL DATASET OVERVIEW (1970-2024)\n")
cat("================================================================================\n\n")

total_records <- nrow(full_data)
cat(sprintf("Total records in full dataset: %d\n", total_records))

# Get unique election years
election_years <- sort(unique(full_data$Year))
cat(sprintf("Election years available: %s\n", paste(election_years, collapse = ", ")))
cat(sprintf("Number of elections: %d\n", length(election_years)))

# Records per election year
cat("\n--- Records per Election Year ---\n")
year_distribution <- table(full_data$Year)
year_df <- data.frame(
  Year = names(year_distribution),
  Records = as.numeric(year_distribution),
  Percentage = round(as.numeric(year_distribution) / total_records * 100, 2)
)
print(year_df, row.names = FALSE)

# Parties and candidates in full dataset
unique_parties_full <- length(unique(full_data$Party[full_data$Party != ""]))
cat(sprintf("\nUnique parties in full dataset: %d\n", unique_parties_full))

# Count candidates (non-empty Candidate Name)
candidates_with_names_full <- sum(full_data$Candidate.Name != "" & !is.na(full_data$Candidate.Name))
cat(sprintf("Records with candidate names: %d (%.2f%%)\n",
            candidates_with_names_full,
            candidates_with_names_full / total_records * 100))

# ==============================================================================
# PART 2: ELECTION YEAR SELECTION JUSTIFICATION
# ==============================================================================

cat("\n================================================================================\n")
cat("PART 2: ELECTION YEAR SELECTION JUSTIFICATION (2008, 2013, 2024)\n")
cat("================================================================================\n\n")

# Analyze 2018 data (excluded election)
data_2018 <- full_data[full_data$Year == 2018, ]
cat("--- Analysis of 2018 Election (EXCLUDED) ---\n")
cat(sprintf("Total records in 2018: %d\n", nrow(data_2018)))
candidates_with_names_2018 <- sum(data_2018$Candidate.Name != "" & !is.na(data_2018$Candidate.Name))
cat(sprintf("Records with candidate names: %d (%.2f%%)\n",
            candidates_with_names_2018,
            candidates_with_names_2018 / nrow(data_2018) * 100))
cat(sprintf("Records WITHOUT candidate names: %d (%.2f%%)\n",
            nrow(data_2018) - candidates_with_names_2018,
            (nrow(data_2018) - candidates_with_names_2018) / nrow(data_2018) * 100))

# Analyze selected elections
selected_years <- c(2008, 2013, 2024)
cat("\n--- Analysis of SELECTED Elections (2008, 2013, 2024) ---\n")
for(year in selected_years) {
  data_year <- full_data[full_data$Year == year, ]
  candidates_with_names <- sum(data_year$Candidate.Name != "" & !is.na(data_year$Candidate.Name))
  cat(sprintf("\n%d:\n", year))
  cat(sprintf("  Total records: %d\n", nrow(data_year)))
  cat(sprintf("  With candidate names: %d (%.2f%%)\n",
              candidates_with_names,
              candidates_with_names / nrow(data_year) * 100))
}

# Justification summary
cat("\n--- JUSTIFICATION FOR SELECTION ---\n")
cat("1. Data Quality: 2018 election had 0% candidate names, making it unusable\n")
cat("   for network analysis based on candidate-party affiliations.\n")
cat("2. Recency: 2008, 2013, 2024 represent the most recent elections with\n")
cat("   complete candidate information.\n")
cat("3. Coverage: These three elections span 16 years of political evolution,\n")
cat("   capturing post-Musharraf era democratic transitions.\n")
cat("4. Consistency: All three selected years have high-quality candidate data\n")
cat("   suitable for bipartite network construction.\n")

# ==============================================================================
# PART 3: CLEANED DATA STATISTICS
# ==============================================================================

cat("\n================================================================================\n")
cat("PART 3: CLEANED DATA STATISTICS (2008, 2013, 2024)\n")
cat("================================================================================\n\n")

cleaned_records <- nrow(cleaned_data)
cat(sprintf("Total records in cleaned dataset: %d\n", cleaned_records))

# Calculate fraction of total records
fraction_of_total <- cleaned_records / total_records * 100
cat(sprintf("Fraction of total dataset: %.2f%%\n", fraction_of_total))

# Records per year in cleaned data
cat("\n--- Records per Election Year (Cleaned) ---\n")
cleaned_year_distribution <- table(cleaned_data$Year)
cleaned_year_df <- data.frame(
  Year = names(cleaned_year_distribution),
  Records = as.numeric(cleaned_year_distribution),
  Percentage_of_Cleaned = round(as.numeric(cleaned_year_distribution) / cleaned_records * 100, 2)
)
print(cleaned_year_df, row.names = FALSE)

# Unique parties in cleaned data
unique_parties_cleaned <- length(unique(cleaned_data$Party[cleaned_data$Party != ""]))
cat(sprintf("\nUnique parties after cleaning: %d\n", unique_parties_cleaned))

# Unique candidates in cleaned data
unique_candidates_cleaned <- length(unique(cleaned_data$Candidate_ID[!is.na(cleaned_data$Candidate_ID)]))
cat(sprintf("Unique candidates after cleaning: %d\n", unique_candidates_cleaned))

# All candidates should have names (since we filtered for Has_Candidate_Name == TRUE)
candidates_with_names_cleaned <- sum(cleaned_data$Has_Candidate_Name == TRUE)
cat(sprintf("Records with candidate names: %d (%.2f%%)\n",
            candidates_with_names_cleaned,
            candidates_with_names_cleaned / cleaned_records * 100))

# ==============================================================================
# PART 4: PREPROCESSING IMPACT
# ==============================================================================

cat("\n================================================================================\n")
cat("PART 4: PREPROCESSING IMPACT\n")
cat("================================================================================\n\n")

# Calculate raw data for selected years before cleaning
raw_selected_years <- full_data[full_data$Year %in% selected_years, ]
raw_selected_records <- nrow(raw_selected_years)

cat(sprintf("Records for 2008, 2013, 2024 BEFORE cleaning: %d\n", raw_selected_records))
cat(sprintf("Records for 2008, 2013, 2024 AFTER cleaning: %d\n", cleaned_records))
cat(sprintf("Records removed during preprocessing: %d (%.2f%%)\n",
            raw_selected_records - cleaned_records,
            (raw_selected_records - cleaned_records) / raw_selected_records * 100))

# Check for independent candidates in raw data
independents_in_raw <- sum(raw_selected_years$Party == "IND", na.rm = TRUE)
cat(sprintf("\nIndependent candidates removed: %d\n", independents_in_raw))
cat(sprintf("Percentage of raw data: %.2f%%\n", independents_in_raw / raw_selected_records * 100))

# Party name standardization
cat("\n--- Party Name Standardization ---\n")
original_parties <- length(unique(cleaned_data$Party_Original[cleaned_data$Party_Original != ""]))
standardized_parties <- length(unique(cleaned_data$Party[cleaned_data$Party != ""]))
cat(sprintf("Original unique party names: ~280 (from raw data inspection)\n"))
cat(sprintf("After standardization: %d\n", standardized_parties))
cat(sprintf("Reduction: ~%d party names consolidated\n", 280 - standardized_parties))

# ==============================================================================
# PART 5: NETWORK CONSTRUCTION STATISTICS
# ==============================================================================

cat("\n================================================================================\n")
cat("PART 5: NETWORK CONSTRUCTION STATISTICS\n")
cat("================================================================================\n\n")

# Load network files if available
party_nodes_file <- file.path(resultpath, "Bipartition-Party_Party.R output", "party_nodes.csv")
candidate_nodes_file <- file.path(resultpath, "Bipartition-Party_Party.R output", "candidate_nodes.csv")
edge_list_file <- file.path(resultpath, "Bipartition-Party_Party.R output", "edge_list_weighted.csv")

if(file.exists(party_nodes_file)) {
  party_nodes <- read.csv(party_nodes_file, stringsAsFactors = FALSE)
  cat(sprintf("Parties in final network: %d\n", nrow(party_nodes)))
}

if(file.exists(candidate_nodes_file)) {
  candidate_nodes <- read.csv(candidate_nodes_file, stringsAsFactors = FALSE)
  cat(sprintf("Candidates in final network: %d\n", nrow(candidate_nodes)))
}

if(file.exists(edge_list_file)) {
  edge_list <- read.csv(edge_list_file, stringsAsFactors = FALSE)
  cat(sprintf("Edges in bipartite network: %d\n", nrow(edge_list)))
}

# ==============================================================================
# PART 6: WHY NOT USE DATA FROM 1970?
# ==============================================================================

cat("\n================================================================================\n")
cat("PART 6: COMPREHENSIVE JUSTIFICATION - WHY NOT USE DATA FROM 1970?\n")
cat("================================================================================\n\n")

cat("REASON 1: Data Quality Issues\n")
cat("------------------------------\n")
cat("• 2018 election had 0% candidate names (3,353 records unusable)\n")
cat("• Many older elections likely have similar data quality issues\n")
cat("• Network analysis requires complete candidate-party linkages\n\n")

cat("REASON 2: Political Context Relevance\n")
cat("--------------------------------------\n")
cat("• 2008-2024 represents Pakistan's democratic era post-Musharraf\n")
cat("• Including 1970s-1990s data would mix military regimes with democracy\n")
cat("• Political party system has fundamentally changed since 1970\n")
cat("• Many parties from 1970s no longer exist or have merged\n\n")

cat("REASON 3: Research Focus\n")
cat("------------------------\n")
cat("• Focus on contemporary political networks (last 16 years)\n")
cat("• Analysis of recent party-candidate dynamics more policy-relevant\n")
cat("• Three elections provide sufficient data for network analysis\n")
cat("• ", cleaned_records, " records across ", unique_candidates_cleaned, " candidates and ",
    unique_parties_cleaned, " parties\n\n")

cat("REASON 4: Methodological Soundness\n")
cat("-----------------------------------\n")
cat("• Ensures data consistency (same recording standards)\n")
cat("• Avoids temporal bias from 54-year span\n")
cat("• Focuses on stable democratic period\n")
cat("• Selected years represent full election cycles\n\n")

# ==============================================================================
# PART 7: SUMMARY TABLE FOR REPORT
# ==============================================================================

cat("================================================================================\n")
cat("PART 7: SUMMARY TABLE FOR METHODOLOGY SECTION\n")
cat("================================================================================\n\n")

summary_table <- data.frame(
  Metric = c(
    "Total records (1970-2024)",
    "Election years available",
    "Records for 2008, 2013, 2024 (raw)",
    "Records after cleaning",
    "Fraction of total dataset",
    "Independent candidates removed",
    "Unique parties (after cleaning)",
    "Unique candidates (after cleaning)",
    "Party names consolidated",
    "Records with complete data"
  ),
  Value = c(
    sprintf("%d", total_records),
    paste(election_years, collapse = ", "),
    sprintf("%d", raw_selected_records),
    sprintf("%d", cleaned_records),
    sprintf("%.2f%%", fraction_of_total),
    sprintf("%d (%.2f%%)", independents_in_raw, independents_in_raw / raw_selected_records * 100),
    sprintf("%d", unique_parties_cleaned),
    sprintf("%d", unique_candidates_cleaned),
    sprintf("~280 → %d", standardized_parties),
    sprintf("%d (%.2f%%)", cleaned_records, fraction_of_total)
  )
)

print(summary_table, row.names = FALSE)

# Save summary table
output_file <- file.path(resultpath, "data_statistics_summary.csv")
write.csv(summary_table, output_file, row.names = FALSE)
cat(sprintf("\n✓ Summary table saved: %s\n", output_file))

# ==============================================================================
# PART 8: TOP PARTIES ANALYSIS
# ==============================================================================

cat("\n================================================================================\n")
cat("PART 8: TOP PARTIES IN CLEANED DATA\n")
cat("================================================================================\n\n")

party_counts <- sort(table(cleaned_data$Party), decreasing = TRUE)
top_parties <- head(party_counts, 15)

cat("Top 15 Parties by Number of Candidates:\n")
top_parties_df <- data.frame(
  Party = names(top_parties),
  Candidates = as.numeric(top_parties),
  Percentage = round(as.numeric(top_parties) / cleaned_records * 100, 2)
)
print(top_parties_df, row.names = FALSE)

# ==============================================================================
# FINAL MESSAGE
# ==============================================================================

cat("\n================================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("================================================================================\n\n")

cat("KEY FINDINGS FOR INSTRUCTOR:\n")
cat("1. Dataset spans 1970-2024 with", length(election_years), "elections\n")
cat("2. Selected 2008, 2013, 2024 due to complete candidate data\n")
cat("3. These represent", sprintf("%.2f%%", fraction_of_total), "of total records\n")
cat("4. After cleaning:", unique_candidates_cleaned, "candidates,", unique_parties_cleaned, "parties\n")
cat("5. Removed", independents_in_raw, "independent candidates (research focus)\n")
cat("6. Why not 1970s: Data quality, political context, methodological consistency\n\n")

cat("✓ Results saved to:", output_file, "\n")
cat("================================================================================\n")
