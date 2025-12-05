# Data Cleaning Summary Report

## Pakistan National Assembly Elections (2008, 2013, 2024)

**Project:** Social Network Analysis of Candidate-Party Affiliations  
**Authors:** Hammad Malik (hm08298) & Mehlab Kashani (mk07950)  
**Date:** October 31, 2025

---

## 1. Overview

This document summarizes the data cleaning and filtering process for the Pakistan National Assembly Elections dataset, focusing on the 2008, 2013, and 2024 electoral cycles.

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

- **2008:** Post-Musharraf democratic transition
- **2013:** First democratic power transfer in Pakistan's history
- **2024:** Most recent election

**Rationale for excluding 2018:** The 2018 election data in the Gallup dataset contains missing candidate names for all 3,353 records, making it unsuitable for individual candidate-level network analysis. We selected 2008 instead, which provides complete candidate information while still capturing the critical democratic transition period.

---

## 4. Data Quality Issues Identified

### 4.1 Missing Values Analysis

We examined all three selected years for data completeness:

- **2008:** Complete candidate names ✓
- **2013:** Complete candidate names ✓
- **2024:** Complete candidate names ✓
- **Votes:** Minimal missing values (<1%)

### 4.2 Data Quality Decisions

All three selected election years (2008, 2013, 2024) contain complete candidate name information, enabling:

- Full individual candidate tracking across elections
- Accurate party-switching analysis
- Complete bipartite network construction with named individuals
- Longitudinal analysis spanning 16 years

### 4.3 Empty Strings

- **Party field:** 1 empty string (removed)
- **Candidate Name:** No empty strings in selected years (2008, 2013, 2024)

---

## 5. Data Cleaning Steps

### 5.1 Column Standardization

- Renamed columns to use underscores instead of spaces/dots
- Changed `NA.` to `NA_Code` (to avoid conflict with R's NA keyword)
- Renamed `Candidate.Name` to `Candidate_Name_Raw`

### 5.2 Party Name Standardization and Abbreviations

**Before:** ~280 unique parties
**After:** 175 unique parties

**Rationale:** Many party names in the original dataset are very long (e.g., "Pakistan Sunni Tehreek", "Muttahidda Majlis-e-Amal Pakistan", "Awami Himayat Tehreek Pakistan"), making them difficult to display clearly in network visualizations. Additionally, the same party often appeared with multiple spelling variations and formats. To improve readability and accuracy, we implemented a comprehensive abbreviation and consolidation system while preserving original names for reference.

**How Consolidation Was Achieved:**

The reduction from ~280 to 175 parties was accomplished through a two-step process:

1. **Abbreviation Mapping**: Created a dictionary of 112 long party names mapped to short abbreviations
2. **Variation Consolidation**: Used case-insensitive pattern matching to merge spelling variations and different formats of the same party

**Example of Consolidation:**

Original variations found in raw data → Standardized to:

- `"PPP"`, `"Pakistan Peoples Party (Shaheed Bhutto)"` → **`PPP`**
- `"MQM"`, `"MQM-P"` → **`MQM`**

This process eliminated approximately 105 party name variations (37.5% reduction), while the `Party_Original` column preserves all original names for traceability.

#### 5.2.1 Major Party Standardizations

- `PML-N`, `PMLN` → `PML-N`
- `PML-Q`, `PMLQ` → `PML-Q`
- `PTI`, `Pakistan Tehreek` → `PTI`
- `MQM` (all variations) → `MQM`
- `JUI-F` → `JUI-F`
- `PPP`, `Pakistan Peoples Party` → `PPP`
- `ANP` → `ANP`

#### 5.2.2 Long Party Name Abbreviations

To enhance visualization clarity, we created abbreviations for 80+ parties with long names:

**Religious/Islamic Parties:**

- `Muttahidda Majlis-e-Amal Pakistan` → `MMA`
- `Pakistan Sunni Tehreek` → `PST`
- `Sunni Ittehad Council` → `SIC`
- `Sunni Tehreek` → `ST`
- `Jamiat Ulma-e-Pakistan (Noorani)` → `JUP-N`
- `Jamiat Ulma-e-Pakistan (Niazi)` → `JUP-NZ`
- `Jamiat Ulama-e-Islam (S)` → `JUI-S`
- `Jamiat Ulama-e-Islam Nazryati Pakistan` → `JUI-NP`
- `Markazi Jamiat Ulema-e-Pakistan (FK)` → `MJUP-FK`
- `Majlis-e-Wahdat-e-Muslimeen Pakistan` → `MWMP`

**PML Factions:**

- `Pakistan Muslim League (J)` → `PML-J`
- `Pakistan Muslim League (F)` → `PML-F`
- `Pakistan Muslim League(Z)` → `PML-Z`
- `Pakistan Muslim League (Safdar)` → `PML-S`
- `Pakistan Muslim League-Muttahida` → `PML-M`
- `Pakistan Muslim League (Zehri Group)` → `PML-Z`
- `Pakistan Muslim League "H" Haqiqi` → `PML-H`
- `Pakistan Muslim League Council` → `PML-C`
- `All Pakistan Muslim League` → `APML`

**Regional/Ethnic Parties:**

- `Awami Himayat Tehreek Pakistan` → `AHTP`
- `Pukhtoonkhwa Milli Awami Party` → `PKMAP`
- `Qaumi Watan Party (Sherpao)` → `QWP-S`
- `Qaumi Watan Party` → `QWP`
- `Bahawalpur National Awami Party` → `BNAP`
- `Mutahida Baloch Movement Pakistan` → `MBMP`
- `Seraiki Sooba Movement Pakistan` → `SSMP`

**Tehreek/Movement Parties:**

- `Tehreek-e-Tahaffuze Pakistan` → `TTP`
- `Tehreek-e-Istehkaam Pakistan` → `TIP`
- `Istehkaam-e-Pakistan Movement` → `IPM`
- `Pakistan Tehrek-e-Inqalab` → `PTI-I`
- `Tehreek-e-Suba Hazara` → `TSH`
- `Tehreek Tabdili Nizam Pakistan` → `TTNP`

**Other Parties:**

- `National Peoples Party` → `NPP`
- `National Party` → `NP`
- `Jamhoori Wattan Party` → `JWP`
- `Pakistan Conservative Party` → `PCP`
- `Communist Party of Pakistan` → `CPP`
- `Christian Progressive Movement` → `CPM`
- And 50+ additional parties...

**Complete List:** Over 80 parties have been abbreviated for improved visualization. The full mapping is available in the data cleaning script (`Datacleaning.R`, lines 165-278).

#### 5.2.3 Preservation of Original Names

All original party names are preserved in the `Party_Original` column, ensuring:

- Traceability back to source data
- Ability to verify abbreviation accuracy
- Option to use full names in text descriptions
- Data integrity and transparency

### 5.3 Candidate Name Standardization

For all three years (2008, 2013, 2024):

- Removed leading/trailing whitespace
- Standardized capitalization (Title Case)
- Replaced multiple spaces with single space
- Standardized "Mohammed" → "Muhammad"

### 5.4 Unique Identifier Creation

Created `Candidate_ID` field by sanitizing the standardized candidate name (replacing special characters with underscores)
Example: `Muhammad_Ali_Khan`

Note: Constituency codes are not included in the identifier to allow tracking candidates across different constituencies over time.

### 5.5 Removal of Independent (IND) Candidates

**Rationale:** Independent candidates were removed from the dataset to focus the network analysis on party-based political structures and affiliations.

- **Action:** Filtered out all records where `Party == "IND"`
- **Impact:** Removed a significant portion of candidates, as independents represent a large share of contestants in Pakistani elections
- **Justification:**
   - **Network Focus:** The primary goal of this analysis is to study party-party networks and candidate-party affiliations
   - **Analytical Clarity:** Independent candidates have no party affiliation, making them less relevant for party network analysis
   - **Visualization Quality:** Removing independents reduces network complexity and improves interpretability
   - **Party Dynamics:** Focus on party-affiliated candidates enables clearer analysis of party strength, evolution, and inter-party relationships

__Note:__ While independents play an important role in Pakistani politics (especially in 2024), their inclusion would not contribute meaningfully to a party-centric network analysis. For studies focused on independent candidates or electoral competition, the original dataset retains this information in the `Party_Original` column before this filtering step.

### 5.6 Duplicate Removal

- **Duplicates found:** ~75-80 records
- **Action:** Removed duplicate candidate-party-constituency-year combinations

---

## 6. Final Cleaned Dataset

| Metric | Value |
|--------|-------|
| **Total Records** | 5,223 (after removing independents) |
| **Unique Candidates** | 4,593 |
| **Unique Parties** | 175 |
| **Columns** | 22 |
| **Independent Candidates Removed** | 6,613 records (55.88% of raw selected years) |
| **Raw Selected Years (2008+2013+2024)** | 11,836 records (48.14% of full dataset) |
| **Fraction of Full Dataset (After Cleaning)** | 21.25% |
| **Other Records Removed** | <1% (duplicates, missing data) |

### 6.1 Records by Year

| Year | Description | Records (Party-Affiliated Only) | Percentage |
|------|-------------|---------------------------------|------------|
| 2008 | Post-Musharraf transition | 1,038 | 19.88% |
| 2013 | First democratic transfer | 2,082 | 39.86% |
| 2024 | Most recent election | 2,103 | 40.26% |
| **Total** | **All Three Years** | **5,223** | **100%** |

**Note:** Record counts reflect only party-affiliated candidates after removal of independents.

### 6.2 Records by Province

| Province | Expected Percentage |
|----------|---------------------|
| Punjab | ~50-52% |
| Sindh | ~23-25% |
| KPK | ~16-18% |
| Balochistan | ~7-8% |
| ICT | ~1% |

### 6.3 Top Parties by Candidate Count

Major parties across all three years (after removing independents):

| Rank | Party | Full Name | Candidates | % of Data |
|------|-------|-----------|-----------|-----------|
| 1 | PPP | Pakistan Peoples Party | 710 | 13.59% |
| 2 | PML-N | Pakistan Muslim League (Nawaz) | 567 | 10.85% |
| 3 | PTI | Pakistan Tehreek-e-Insaf | 488 | 9.34% |
| 4 | MQM | Muttahida Qaumi Movement | 405 | 7.75% |
| 5 | Pakistan Muslim League | Various PML factions | 245 | 4.69% |
| 6 | JUI-F | Jamiat Ulema-e-Islam (Fazl) | 230 | 4.40% |
| 7 | JI-P | Jamaat-e-Islami Pakistan | 225 | 4.31% |
| 8 | TLP | Tehreek-e-Labbaik Pakistan | 213 | 4.08% |
| 9 | JI | Jamaat-e-Islami | 162 | 3.10% |
| 10 | ANP | Awami National Party | 149 | 2.85% |

**Key Observations:**

- **PPP** maintains major presence across all three elections (13.59% of records)
- **PML-N** demonstrates strong support base, especially in Punjab (10.85%)
- **PTI** shows remarkable growth: minimal in 2008, major force by 2013 and 2024 (9.34%)
- **MQM** concentrated in urban Sindh with consistent presence (7.75%)
- **Religious parties** (JUI-F, JI-P, JI, TLP) collectively represent significant portion (17.89%)
- **175 total parties** demonstrate Pakistan's diverse political landscape

**Note:** Independent candidates (previously the largest category) have been excluded from this analysis to focus on party-based networks.

---

## 7. Data Quality Flags

### 7.1 New Fields Added

1. __Has_Candidate_Name__ (Boolean)

   - TRUE for all records in 2008, 2013, and 2024
   - This field is maintained for consistency but will be TRUE for all selected years

2. __Party_Original__: Preserved original party name before standardization and abbreviation

   - Enables verification of abbreviation accuracy
   - Maintains data traceability

3. **Party**: Standardized and abbreviated party name

   - Used for all visualizations and network analysis
   - Significantly improves readability in network diagrams

4. __Candidate_Original__: Preserved original candidate name
5. __Candidate_Name__: Cleaned and standardized candidate name
6. __Candidate_ID__: Unique identifier for each candidate

---

## 8. Network Analysis Implications

### 8.1 Strengths

- Complete coverage of all three target election years
- **Complete candidate names for all years** - enables full individual-level analysis
- **Party-focused dataset** - removal of independents sharpens focus on party networks and affiliations
- **Abbreviated party names** - dramatically improves visualization readability while preserving original data
- Standardized party names enable accurate affiliation tracking
- Unique candidate IDs support tracking candidates across constituencies and elections
- 16-year span (2008-2024) captures major political transitions:
   - Democratic restoration (2008)
   - First democratic transfer (2013)
   - Recent political landscape (2024)

- Simplified identifier system allows comprehensive mobility analysis
- Original party names preserved for reference and verification
- Cleaner network structure by focusing exclusively on party-affiliated candidates

### 8.2 Advantages Over Previous Selection

1. **Full Candidate Tracking:** Unlike the previous selection that included 2018 (with missing names), all three years now have complete candidate information
2. **Longitudinal Analysis:** Can track individual candidates across all three elections
3. **Party-Switching Detection:** Enables complete analysis of candidate movements between parties across 2008→2013→2024
4. **Historical Significance:** Captures the critical post-Musharraf democratic transition (2008)

### 8.3 Recommended Approach

For network construction:

- **Party-Party Network:** Focus on connections between parties through shared constituencies and electoral competition
- **Candidate-Party Bipartite Network:** Full candidate-party network with named party-affiliated individuals across 2008, 2013, and 2024
- **Temporal Comparison:**
   - 2008 vs 2013: Early democratic transition patterns
   - 2013 vs 2024: Modern political evolution
   - 2008 vs 2024: Long-term transformation

- **Party Evolution Analysis:** Track party strength changes across 16 years (excluding independent fluctuations)
- **Candidate Loyalty Analysis:** Identify stable vs mobile candidates across three elections within the party system
- **Inter-Party Competition:** Analyze party-to-party relationships without the noise of independent candidates

---

## 9. Files Generated

| File | Description |
|------|-------------|
| `cleaned_elections_2008_2013_2024.csv` | Main cleaned dataset (5,223 rows, party-affiliated only) |
| `data_summary.rds` | R object with summary statistics |
| `Datacleaning.R` | Complete R script for reproducibility |

**Important Note:** The cleaned dataset contains only party-affiliated candidates. Independent candidates have been removed to focus the analysis on party-based political structures.

---

## 10. Next Steps

1. **Data Exploration:** Examine party distribution across years and provinces
2. **Network Construction:** Build bipartite network with candidates and parties as nodes
3. **Edge Creation:** Connect candidates to parties based on election year
4. **Temporal Analysis:** Track changes in affiliations across 2008, 2013, and 2024
5. **Party Evolution:** Analyze party strength changes (especially PTI's rise)
6. **Candidate Mobility:** Identify patterns in party-switching behavior
7. **Centrality Measures:** Calculate degree, betweenness, closeness, and eigenvector centrality

---

## 11. Political Context

### 11.1 2008 Elections

- Held after Musharraf's resignation
- PPP-led coalition victory
- Return to democratic governance
- PTI minimal presence

### 11.2 2013 Elections

- First democratic power transfer in Pakistan's history
- PML-N victory
- PTI emerged as third major force
- Significant milestone for democracy

### 11.3 2024 Elections

- Recent electoral landscape
- Continued democratic process
- Current political configuration

---

## 12. Quality Assurance

✅ All three target years (2008, 2013, 2024) present in final dataset
✅ __Complete candidate names for all selected years__
✅ __Independent (IND) candidates removed to focus on party networks__
✅ Party names standardized to reduce duplicates
✅ __80+ long party names abbreviated for visualization clarity__
✅ __Original party names preserved in `Party_Original` column__
✅ Unique identifiers created for all records
✅ Duplicates removed
✅ Original data preserved in separate columns
✅ Data quality flags added for transparency
✅ __No missing candidate data issues__ (resolved by excluding 2018)
✅ 16-year time span enables comprehensive longitudinal analysis
✅ Abbreviation system enhances network visualization readability
✅ Dataset optimized for party-centric network analysis

---

**End of Report**