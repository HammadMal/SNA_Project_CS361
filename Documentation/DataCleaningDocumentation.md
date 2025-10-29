# Data Cleaning Summary Report
## Pakistan National Assembly Elections (2013, 2018, 2024)

**Project:** Social Network Analysis of Candidate-Party Affiliations  
**Authors:** Hammad Malik (hm08298) & Mehlab Kashani (mk07950)  
**Date:** October 29, 2025

---

## 1. Overview

This document summarizes the data cleaning and filtering process for the Pakistan National Assembly Elections dataset, focusing on the 2013, 2018, and 2024 electoral cycles.

---

## 2. Original Dataset

**Source:** Gallup Pakistan Elections Database (1970-2024)

| Metric | Value |
|--------|-------|
| Total Records | 24,585 |
| Election Years | 11 (1970, 1977, 1988, 1990, 1993, 1997, 2002, 2008, 2013, 2018, 2024) |
| Columns | 17 |

---

## 3. Filtering Process

### 3.1 Year Selection
We filtered the dataset to include only three target election years:
- **2013:** Post-democratic transition period (4,454 records)
- **2018:** Critical power transfer (3,353 records)  
- **2024:** Most recent election (5,177 records)

**Filtered Dataset:** 12,984 rows (52.8% of original data)

---

## 4. Data Quality Issues Identified

### 4.1 Missing Values
- **Votes:** 18 missing values across all years
- **Candidate Names (2018 only):** All 3,353 records from 2018 had empty candidate names in the source data

### 4.2 Data Quality Decisions
Since the 2018 election is critical for temporal analysis, we **retained** the 2018 records despite missing candidate names. We created placeholder names: `Unknown_Candidate_1`, `Unknown_Candidate_2`, etc.

**Note:** This limitation means that for 2018, our network analysis will focus on party-level patterns rather than individual candidate movements.

### 4.3 Empty Strings
- **Party field:** 1 empty string (removed)
- **Candidate Name:** 3,353 empty strings (2018 data - handled with placeholders)

---

## 5. Data Cleaning Steps

### 5.1 Column Standardization
- Renamed columns to use underscores instead of spaces/dots
- Changed `NA.` to `NA_Code` (to avoid conflict with R's NA keyword)
- Renamed `Candidate.Name` to `Candidate_Name_Raw`

### 5.2 Party Name Standardization
**Before:** 286 unique parties  
**After:** 256 unique parties

Key standardizations:
- `PML-N`, `PMLN` → `PML-N`
- `PML-Q`, `PMLQ` → `PML-Q`  
- `PTI`, `Pakistan Tehreek` → `PTI`
- `IND`, `Independent` → `IND`
- `MQM` (all variations) → `MQM`
- `JUI-F` → `JUI-F`
- `PPP`, `Pakistan Peoples Party` → `PPP`

### 5.3 Candidate Name Standardization
For 2013 and 2024 data:
- Removed leading/trailing whitespace
- Standardized capitalization (Title Case)
- Replaced multiple spaces with single space
- Standardized "Mohammed" → "Muhammad"

### 5.4 Unique Identifier Creation
Created `Candidate_ID` field: `{Candidate_Name}_{NA_Code}`
Example: `Alhaj_Ghulam_Ahmad_Bilour_NA-1`

### 5.5 Duplicate Removal
- **Duplicates found:** 78 records
- **Action:** Removed duplicate candidate-party-constituency-year combinations

---

## 6. Final Cleaned Dataset

| Metric | Value |
|--------|-------|
| **Total Records** | 12,905 |
| **Columns** | 22 |
| **Records Removed** | 79 (0.6%) |

### 6.1 Records by Year
| Year | Records | Percentage |
|------|---------|------------|
| 2013 | 4,454 | 34.5% |
| 2018 | 3,353 | 26.0% |
| 2024 | 5,098 | 39.5% |

### 6.2 Records by Province
| Province | Records | Percentage |
|----------|---------|------------|
| Punjab | 6,597 | 51.1% |
| Sindh | 3,035 | 23.5% |
| KPK | 2,160 | 16.7% |
| Balochistan | 966 | 7.5% |
| ICT | 113 | 0.9% |
| (Other) | 34 | 0.3% |

### 6.3 Top 10 Parties by Candidate Count
| Rank | Party | Candidates | Percentage |
|------|-------|------------|------------|
| 1 | IND (Independents) | 6,961 | 53.9% |
| 2 | PPP | 770 | 6.0% |
| 3 | PTI | 766 | 5.9% |
| 4 | PML-N | 641 | 5.0% |
| 5 | TLP | 393 | 3.0% |
| 6 | MQM | 351 | 2.7% |
| 7 | JUI-F | 238 | 1.8% |
| 8 | JI-P | 232 | 1.8% |
| 9 | MMA | 191 | 1.5% |
| 10 | ANP | 176 | 1.4% |

**Note:** The high number of independents (53.9%) reflects Pakistan's political landscape and includes the 2018 data where specific candidate names were unavailable.

---

## 7. Data Quality Flags

### 7.1 New Fields Added
1. **Has_Candidate_Name** (Boolean)
   - TRUE: Original dataset contained candidate name (2013, 2024)
   - FALSE: Candidate name was missing and placeholder was used (2018)

2. **Party_Original**: Preserved original party name before standardization

3. **Candidate_Original**: Preserved original candidate name

4. **Candidate_Name**: Cleaned and standardized candidate name

5. **Candidate_ID**: Unique identifier for each candidate-constituency combination

---

## 8. Network Analysis Implications

### 8.1 Strengths
- Complete coverage of all three target election years
- High-quality data for 2013 and 2024
- Standardized party names enable accurate affiliation tracking
- Unique candidate IDs support longitudinal analysis

### 8.2 Limitations
1. **2018 Candidate-Level Analysis:** Cannot track individual candidates in 2018 due to missing names
2. **Party-Switching Detection:** Limited to 2013→2024 comparisons for individual candidates
3. **2018 Network Contribution:** Will focus on party-level patterns and aggregate statistics

### 8.3 Recommended Approach
For network construction:
- **Bipartite Network (2013 + 2024):** Full candidate-party network with named individuals
- **Party-Level Network (All years):** Aggregate analysis including 2018 for temporal trends
- **Temporal Comparison:** Focus on 2013 vs 2024 for candidate loyalty/switching patterns

---

## 9. Files Generated

| File | Description |
|------|-------------|
| `cleaned_elections_2013_2018_2024.csv` | Main cleaned dataset (12,905 rows) |
| `data_summary.rds` | R object with summary statistics |
| `01_data_cleaning_base.R` | Complete R script for reproducibility |

---

## 10. Next Steps

1. **Data Exploration:** Examine party distribution across years and provinces
2. **Network Construction:** Build bipartite network with candidates and parties as nodes
3. **Edge Creation:** Connect candidates to parties based on election year
4. **Temporal Analysis:** Track changes in affiliations between 2013 and 2024
5. **Centrality Measures:** Calculate degree, betweenness, closeness, and eigenvector centrality

---

## 11. Quality Assurance

✅ All three target years (2013, 2018, 2024) are present in final dataset  
✅ Party names standardized to reduce duplicates  
✅ Unique identifiers created for all records  
✅ Duplicates removed  
✅ Original data preserved in separate columns  
✅ Data quality flags added for transparency  
✅ Missing data handled appropriately (2018 candidate names)

---

**End of Report**
