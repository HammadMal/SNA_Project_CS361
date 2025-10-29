# ==============================================================================
# Social Network Analysis - Pakistan Elections
# Script 01: Data Cleaning and Filtering (Base R Version)
# ==============================================================================
# Authors: Hammad Malik (hm08298) & Mehlab Kashani (mk07950)
# Course: CS/SDP 361/352
# Date: October 29, 2025
# ==============================================================================

# ==============================================================================
# 1. DATA IMPORT
# ==============================================================================


getwd()
datapath = "C:/Users/Hammad/Documents/github/SNA_Project_CS361/dataset/gallup-pakistan-elections-database-1970-2024-national-assembly.csv"


cat("Reading dataset...\n")
elections_raw <- read.csv(
  datapath,
  stringsAsFactors = FALSE,
  strip.white = TRUE
)

# Display dataset dimensions
cat(sprintf("Original dataset: %d rows, %d columns\n", 
            nrow(elections_raw), ncol(elections_raw)))

# Display column names
cat("\nColumn names:\n")
print(colnames(elections_raw))

# Display first few rows
cat("\nFirst few rows:\n")
print(head(elections_raw, 3))

# ==============================================================================
# 2. FILTER DATA FOR TARGET YEARS (2013, 2018, 2024)
# ==============================================================================

cat("\n", paste(rep("=", 80), collapse=""), "\n", sep = "")
cat("FILTERING DATA FOR 2013, 2018, AND 2024\n")
cat(paste(rep("=", 80), collapse=""), "\n", sep = "")

# Fix column name (remove BOM if present)
if(grepl("^X", colnames(elections_raw)[1])) {
  colnames(elections_raw)[1] <- "Year"
}

# Fix other column names with dots and spaces
colnames(elections_raw) <- gsub("\\.", "_", colnames(elections_raw))
colnames(elections_raw)[colnames(elections_raw) == "NA_"] <- "NA_Code"  # NA is reserved in R
colnames(elections_raw)[colnames(elections_raw) == "Candidate_Name"] <- "Candidate_Name_Raw"

# Filter for target election years
elections_filtered <- elections_raw[elections_raw$Year %in% c(2013, 2018, 2024), ]

# Display filtering results
cat(sprintf("\nFiltered dataset: %d rows (%.1f%% of original)\n", 
            nrow(elections_filtered), 
            100 * nrow(elections_filtered) / nrow(elections_raw)))

# Show breakdown by year
year_counts <- table(elections_filtered$Year)
cat("\nRecords per election year:\n")
print(year_counts)

# ==============================================================================
# 3. DATA QUALITY CHECKS
# ==============================================================================

cat("\n", paste(rep("=", 80), collapse=""), "\n", sep = "")
cat("DATA QUALITY CHECKS\n")
cat(paste(rep("=", 80), collapse=""), "\n", sep = "")

# Check for missing values in critical columns
critical_columns <- c("Year", "Constituency", "NA_Code", "Province", 
                      "Party", "Candidate_Name_Raw", "Votes")

cat("\nMissing values in critical columns:\n")
for(col in critical_columns) {
  if(col %in% colnames(elections_filtered)) {
    na_count <- sum(is.na(elections_filtered[[col]]))
    cat(sprintf("%s: %d missing values\n", col, na_count))
  }
}

# Check for empty strings
cat("\nEmpty strings in critical text fields:\n")
if("Candidate_Name_Raw" %in% colnames(elections_filtered)) {
  empty_candidates <- sum(elections_filtered$Candidate_Name_Raw == "", na.rm = TRUE)
  cat(sprintf("Candidate Name: %d empty strings\n", empty_candidates))
}
if("Party" %in% colnames(elections_filtered)) {
  empty_parties <- sum(elections_filtered$Party == "", na.rm = TRUE)
  cat(sprintf("Party: %d empty strings\n", empty_parties))
}

# ==============================================================================
# 4. DATA CLEANING
# ==============================================================================

cat("\n", paste(rep("=", 80), collapse=""), "\n", sep = "")
cat("DATA CLEANING\n")
cat(paste(rep("=", 80), collapse=""), "\n", sep = "")

# Start with filtered data
elections_clean <- elections_filtered

# Step 1: Remove records with missing critical fields
cat("\n1. Removing records with missing critical fields...\n")
before_rows <- nrow(elections_clean)

# For 2018, candidate names are missing in the dataset, so we'll keep those records
# but create a placeholder candidate name
elections_clean$Has_Candidate_Name <- elections_clean$Candidate_Name_Raw != "" & 
                                      !is.na(elections_clean$Candidate_Name_Raw)

# Create filter - only require Year, Party, and Constituency
complete_filter <- !is.na(elections_clean$Year) & 
                   !is.na(elections_clean$Party) &
                   !is.na(elections_clean$Constituency) &
                   elections_clean$Party != ""

elections_clean <- elections_clean[complete_filter, ]

# For records without candidate names, create a placeholder
elections_clean$Candidate_Name_Raw[!elections_clean$Has_Candidate_Name] <- 
  paste("Unknown_Candidate", seq_len(sum(!elections_clean$Has_Candidate_Name)), sep = "_")

after_rows <- nrow(elections_clean)
cat(sprintf("   Removed %d rows (%.1f%%)\n", 
            before_rows - after_rows, 
            100 * (before_rows - after_rows) / before_rows))
cat(sprintf("   Records without candidate names: %d (%.1f%%)\n",
            sum(!elections_clean$Has_Candidate_Name),
            100 * sum(!elections_clean$Has_Candidate_Name) / after_rows))

# Step 2: Standardize party names
cat("\n2. Standardizing party names...\n")

# Store original party names
elections_clean$Party_Original <- elections_clean$Party

# Count unique parties before
unique_before <- length(unique(elections_clean$Party))
cat(sprintf("   Unique parties before: %d\n", unique_before))

# Standardize party names
elections_clean$Party <- sapply(elections_clean$Party, function(x) {
  x <- trimws(x)
  
  # PML variations
  if(grepl("PML-N|PMLN", x, ignore.case = TRUE)) return("PML-N")
  if(grepl("PML-Q|PMLQ", x, ignore.case = TRUE)) return("PML-Q")
  if(grepl("PML-F|PMLF", x, ignore.case = TRUE)) return("PML-F")
  if(grepl("^PML$", x, ignore.case = TRUE)) return("PML")
  
  # PPP variations
  if(grepl("PPP|Pakistan Peoples Party", x, ignore.case = TRUE)) return("PPP")
  
  # PTI variations
  if(grepl("PTI|Pakistan Tehreek", x, ignore.case = TRUE)) return("PTI")
  
  # MQM variations
  if(grepl("MQM", x, ignore.case = TRUE)) return("MQM")
  
  # JUI variations
  if(grepl("JUI-F", x, ignore.case = TRUE)) return("JUI-F")
  if(grepl("JUI", x, ignore.case = TRUE)) return("JUI")
  
  # ANP
  if(grepl("ANP", x, ignore.case = TRUE)) return("ANP")
  
  # Independent candidates
  if(grepl("IND|Independent", x, ignore.case = TRUE)) return("IND")
  
  # Return original if no match
  return(x)
})

unique_after <- length(unique(elections_clean$Party))
cat(sprintf("   Unique parties after: %d\n", unique_after))

# Step 3: Standardize candidate names
cat("\n3. Standardizing candidate names...\n")

# Store original names
elections_clean$Candidate_Original <- elections_clean$Candidate_Name_Raw

# Standardize candidate names
elections_clean$Candidate_Name <- sapply(elections_clean$Candidate_Name_Raw, function(x) {
  if(is.na(x)) return(NA)
  x <- trimws(x)  # Remove whitespace
  x <- tools::toTitleCase(tolower(x))  # Title case
  x <- gsub("\\s+", " ", x)  # Replace multiple spaces
  x <- gsub("Mohammed", "Muhammad", x)  # Standardize Muhammad
  return(x)
})

# Step 4: Create unique candidate identifier
cat("\n4. Creating unique candidate-constituency identifiers...\n")

elections_clean$Candidate_ID <- paste(
  elections_clean$Candidate_Name, 
  elections_clean$NA_Code, 
  sep = "_"
)
elections_clean$Candidate_ID <- gsub("[^A-Za-z0-9_-]", "_", elections_clean$Candidate_ID)

# Step 5: Handle duplicate entries
cat("\n5. Checking for duplicate entries...\n")

# Create a composite key
elections_clean$dup_key <- paste(
  elections_clean$Year,
  elections_clean$Candidate_Name,
  elections_clean$Party,
  elections_clean$NA_Code,
  sep = "|||"
)

# Find duplicates
duplicated_rows <- duplicated(elections_clean$dup_key)
n_duplicates <- sum(duplicated_rows)

cat(sprintf("   Found %d duplicate candidate-party-constituency-year combinations\n", 
            n_duplicates))

if(n_duplicates > 0) {
  cat("   Removing duplicate entries...\n")
  elections_clean <- elections_clean[!duplicated_rows, ]
}

# Remove temporary column
elections_clean$dup_key <- NULL

# ==============================================================================
# 5. FINAL DATASET SUMMARY
# ==============================================================================

cat("\n", paste(rep("=", 80), collapse=""), "\n", sep = "")
cat("FINAL CLEANED DATASET SUMMARY\n")
cat(paste(rep("=", 80), collapse=""), "\n", sep = "")

cat(sprintf("\nFinal dataset: %d rows, %d columns\n", 
            nrow(elections_clean), ncol(elections_clean)))

# Summary by year
cat("\nRecords per year:\n")
print(table(elections_clean$Year))

# Summary by party
cat("\nTop 10 parties by candidate count:\n")
party_counts <- sort(table(elections_clean$Party), decreasing = TRUE)
print(head(party_counts, 10))

# Summary by province
cat("\nRecords per province:\n")
print(sort(table(elections_clean$Province), decreasing = TRUE))

# ==============================================================================
# 6. SAVE CLEANED DATA
# ==============================================================================

cat("\n", paste(rep("=", 80), collapse=""), "\n", sep = "")
cat("SAVING CLEANED DATA\n")
cat(paste(rep("=", 80), collapse=""), "\n", sep = "")

# Save cleaned dataset to same folder as 'datapath'
output_file <- "cleaned_elections_2013_2018_2024.csv"
output_dir <- dirname(datapath)                          # <- use datapath directory
output_path <- file.path(output_dir, output_file)

write.csv(elections_clean, output_path, row.names = FALSE)
cat(sprintf("\n✓ Saved cleaned data to: %s\n", output_path))

# Save summary statistics in same folder
summary_stats <- list(
  total_records = nrow(elections_clean),
  years = unique(elections_clean$Year),
  unique_candidates = length(unique(elections_clean$Candidate_Name)),
  unique_parties = length(unique(elections_clean$Party)),
  unique_constituencies = length(unique(elections_clean$NA_Code)),
  records_by_year = as.data.frame(table(elections_clean$Year)),
  top_parties = as.data.frame(head(sort(table(elections_clean$Party), decreasing = TRUE), 10)),
  provinces = as.data.frame(table(elections_clean$Province))
)

summary_path <- file.path(output_dir, "data_summary.rds")
saveRDS(summary_stats, summary_path)
cat(sprintf("✓ Saved summary statistics to: %s\n", summary_path))

cat("\n", paste(rep("=", 80), collapse=""), "\n", sep = "")
cat("DATA CLEANING COMPLETE!\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")
