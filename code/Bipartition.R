# =====================================================
# Bipartite Network Construction in R
# Project: SNA of Candidate–Party Affiliations (Pakistan Elections 2013–2024)
# Author: Hammad Malik
# Dataset: cleaned_elections_2013_2018_2024.csv (fully cleaned)
# =====================================================

# --- Load Required Libraries ---
library(tidyverse)
library(igraph)

# --- Step 1: Load the Cleaned Dataset ---
getwd()
setwd("C:/Users/Hammad/Documents/github/SNA_Project_CS361/dataset")
data <- read.csv("cleaned_elections_2013_2018_2024.csv")
