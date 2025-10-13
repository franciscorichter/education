#!/usr/bin/env Rscript
# ==============================================================================
# Data Preprocessing for Educational Network Analysis
# ==============================================================================
# This script loads and preprocesses the ENLA 2024 data for subsequent analysis
# including EDA, PC algorithm, and Graphical Lasso.
#
# Author: Education Analysis Team
# Date: 2024
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
})

# ==============================================================================
# Configuration
# ==============================================================================

# Set paths relative to project root
ROOT_DIR <- here::here()
DATA_DIR <- file.path(ROOT_DIR, "data")
OUTPUT_DIR <- file.path(ROOT_DIR, "outputs")
ANALYSIS_DIR <- file.path(ROOT_DIR, "analysis")

# Create output directories if they don't exist
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUTPUT_DIR, "preprocessed"), showWarnings = FALSE, recursive = TRUE)

# RDS file path
RDS_PATH <- file.path(DATA_DIR, "enla_processed_data.rds")

message("="*80)
message("Data Preprocessing Pipeline")
message("="*80)

# ==============================================================================
# Load Raw Data
# ==============================================================================

message("\n[1/5] Loading raw data...")
if (!file.exists(RDS_PATH)) {
  stop("RDS file not found at: ", RDS_PATH)
}

obj <- readRDS(RDS_PATH)
if (is.null(obj$em_data)) {
  stop("RDS does not contain em_data")
}

em <- obj$em_data
message("  - Loaded ", nrow(em), " observations")
message("  - Variables: ", ncol(em))

# ==============================================================================
# Data Cleaning and Type Conversion
# ==============================================================================

message("\n[2/5] Cleaning and converting data types...")

# Convert performance measures to numeric
if ("medida500_L" %in% names(em)) {
  em$medida500_L <- suppressWarnings(as.numeric(em$medida500_L))
}
if ("medida500_M" %in% names(em)) {
  em$medida500_M <- suppressWarnings(as.numeric(em$medida500_M))
}

# Remove rows with missing performance measures
em_clean <- em %>%
  filter(!is.na(medida500_L) & !is.na(medida500_M))

message("  - Removed ", nrow(em) - nrow(em_clean), " rows with missing performance data")
message("  - Remaining: ", nrow(em_clean), " observations")

# ==============================================================================
# Standardize Categorical Variables
# ==============================================================================

message("\n[3/5] Standardizing categorical variables...")

# Helper function to standardize group variables
std_group <- function(vec, type) {
  x <- trimws(tolower(as.character(vec)))
  
  if (type == "gender") {
    boy_tokens  <- c("m", "masculino", "male", "nino", "niño", "hombre", "1", "varon", "varón")
    girl_tokens <- c("f", "femenino", "female", "nina", "niña", "mujer", "2")
    return(ifelse(x %in% boy_tokens, "boy", 
                  ifelse(x %in% girl_tokens, "girl", NA_character_)))
  } else if (type == "language") {
    spanish_tokens <- c("castellano", "espanol", "español", "espaniol", "castellana")
    return(ifelse(grepl(paste(spanish_tokens, collapse = "|"), x), "spanish", "other"))
  } else if (type == "school") {
    public_tokens  <- c("publico", "pública", "publica", "estatal", "nacional")
    private_tokens <- c("privado", "particular", "parroquial", "no estatal")
    return(ifelse(grepl(paste(public_tokens, collapse = "|"), x), "public",
                  ifelse(grepl(paste(private_tokens, collapse = "|"), x), "private", NA_character_)))
  } else if (type == "area") {
    rural_tokens <- c("rural")
    urban_tokens <- c("urbana", "urbano", "urban")
    return(ifelse(grepl(paste(rural_tokens, collapse = "|"), x), "rural",
                  ifelse(grepl(paste(urban_tokens, collapse = "|"), x), "urban", NA_character_)))
  }
  rep(NA_character_, length(x))
}

# Detect and standardize group columns
nm <- names(em_clean)

# Gender
gender_col <- if ("SEXO" %in% nm) "SEXO" else 
              if ("sexo" %in% nm) "sexo" else NA_character_
if (!is.na(gender_col)) {
  em_clean$gender <- std_group(em_clean[[gender_col]], "gender")
  message("  - Standardized gender: ", sum(!is.na(em_clean$gender)), " valid values")
}

# School type
school_col <- if ("gestion2" %in% nm) "gestion2" else 
              if ("GESTION" %in% nm) "GESTION" else 
              if ("gestion" %in% nm) "gestion" else NA_character_
if (!is.na(school_col)) {
  em_clean$school_type <- std_group(em_clean[[school_col]], "school")
  message("  - Standardized school type: ", sum(!is.na(em_clean$school_type)), " valid values")
}

# Language
language_col <- if ("lengua_materna" %in% nm) "lengua_materna" else 
                if ("idioma_materno" %in% nm) "idioma_materno" else NA_character_
if (!is.na(language_col)) {
  em_clean$language <- std_group(em_clean[[language_col]], "language")
  message("  - Standardized language: ", sum(!is.na(em_clean$language)), " valid values")
}

# Area (rural/urban)
area_col <- if ("area" %in% nm) "area" else 
            if ("área" %in% nm) "área" else 
            if ("ambito" %in% nm) "ambito" else NA_character_
if (!is.na(area_col)) {
  em_clean$area <- std_group(em_clean[[area_col]], "area")
  message("  - Standardized area: ", sum(!is.na(em_clean$area)), " valid values")
}

# ==============================================================================
# Select and Prepare Analysis Variables
# ==============================================================================

message("\n[4/5] Selecting variables for analysis...")

# Identify numeric variables for network analysis
numeric_vars <- em_clean %>%
  select_if(is.numeric) %>%
  names()

# Remove ID and weight variables
exclude_patterns <- c("^id", "^ID", "^peso", "^PESO", "^weight", "^WEIGHT", 
                      "^factor", "^FACTOR", "^estrato", "^ESTRATO")
analysis_vars <- numeric_vars[!grepl(paste(exclude_patterns, collapse = "|"), 
                                     numeric_vars, ignore.case = TRUE)]

message("  - Identified ", length(analysis_vars), " numeric variables for analysis")

# Create analysis dataset with complete cases
em_analysis <- em_clean %>%
  select(all_of(c("medida500_L", "medida500_M", 
                  if(exists("gender", em_clean)) "gender" else NULL,
                  if(exists("school_type", em_clean)) "school_type" else NULL,
                  if(exists("language", em_clean)) "language" else NULL,
                  if(exists("area", em_clean)) "area" else NULL,
                  analysis_vars))) %>%
  distinct()

# Calculate missingness
missingness <- em_analysis %>%
  summarise(across(everything(), ~sum(is.na(.))/n()*100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  arrange(desc(pct_missing))

message("  - Variables with >20% missing:")
high_missing <- missingness %>% filter(pct_missing > 20)
if (nrow(high_missing) > 0) {
  print(high_missing, n = 20)
} else {
  message("    None")
}

# ==============================================================================
# Save Preprocessed Data
# ==============================================================================

message("\n[5/5] Saving preprocessed data...")

# Save full cleaned dataset
output_file <- file.path(OUTPUT_DIR, "preprocessed", "enla_clean.rds")
saveRDS(list(
  data = em_clean,
  analysis_data = em_analysis,
  analysis_vars = analysis_vars,
  missingness = missingness,
  metadata = list(
    n_obs = nrow(em_clean),
    n_vars = ncol(em_clean),
    n_analysis_vars = length(analysis_vars),
    processing_date = Sys.time()
  )
), output_file)

message("  - Saved to: ", output_file)

# Save CSV for external tools
csv_file <- file.path(OUTPUT_DIR, "preprocessed", "enla_analysis.csv")
write_csv(em_analysis, csv_file)
message("  - Saved CSV to: ", csv_file)

# Print summary statistics
message("\n" %+% "="*80)
message("Preprocessing Complete!")
message("="*80)
message("\nSummary Statistics:")
message("  - Total observations: ", nrow(em_clean))
message("  - Analysis observations: ", nrow(em_analysis))
message("  - Analysis variables: ", length(analysis_vars))
message("  - Mean Math score: ", round(mean(em_analysis$medida500_M, na.rm = TRUE), 2))
message("  - Mean Language score: ", round(mean(em_analysis$medida500_L, na.rm = TRUE), 2))
message("  - Correlation (Math-Language): ", 
        round(cor(em_analysis$medida500_M, em_analysis$medida500_L, 
                  use = "complete.obs"), 3))

if ("gender" %in% names(em_analysis)) {
  message("\nGender distribution:")
  print(table(em_analysis$gender, useNA = "ifany"))
}

if ("area" %in% names(em_analysis)) {
  message("\nArea distribution:")
  print(table(em_analysis$area, useNA = "ifany"))
}

message("\n✓ Data ready for analysis!")
message("  Next steps:")
message("    1. Run 01_eda_analysis.R for exploratory data analysis")
message("    2. Run 02_pc_algorithm.R for causal discovery")
message("    3. Run 03_graphical_lasso.R for network estimation")
