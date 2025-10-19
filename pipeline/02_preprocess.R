#!/usr/bin/env Rscript
# ==============================================================================
# Pipeline Step 02: Preprocess and Merge Data
# ==============================================================================
# Loads the four Excel datasets, merges them appropriately, and creates
# analysis-ready datasets for EDA, PCA, PC algorithm, and Graphical Lasso.
#
# Input: Four Excel files loaded in Step 01
# Output: Cleaned and merged datasets ready for analysis
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(stringr)
  library(purrr)
})

message_line <- function(...) cat(paste0(..., "\n"))
rule <- function(char = "=") paste0(rep(char, 80), collapse = "")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
files <- list(
  alumnos = "/Users/pancho/Library/CloudStorage/Dropbox/25 - CSG/04 - Education/education-main/data/xlsx/EM_6P_2024_alumnos_innominados.xlsx",
  base_hse = "/Users/pancho/Library/CloudStorage/Dropbox/25 - CSG/04 - Education/education-main/data/xlsx/base_web2_HSE_ENLA_2024.xlsx",
  estudiante = "/Users/pancho/Library/CloudStorage/Dropbox/25 - CSG/04 - Education/others/EM 6P 2024 FFAA/__report_6P/data/ENLA2024_6Pestudiante_EBRD1.xlsx",
  familia = "/Users/pancho/Library/CloudStorage/Dropbox/25 - CSG/04 - Education/education-main/data/xlsx/ENLA2024_6Pfamilia_EBR.xlsx"
)

output_dir <- "outputs"
preprocessed_dir <- file.path(output_dir, "preprocessed")
dir.create(preprocessed_dir, showWarnings = FALSE, recursive = TRUE)

message_line(rule())
message_line("PIPELINE 02 - PREPROCESS AND MERGE DATA")
message_line(rule())

# ------------------------------------------------------------------------------
# Load individual datasets
# ------------------------------------------------------------------------------
load_dataset <- function(name, path, sheet = NULL) {
  message_line("Loading ", name, "...")
  tryCatch({
    if (name == "alumnos" && is.null(sheet)) {
      sheet <- "BD"  # Use BD sheet for alumnos
    } else if (is.null(sheet)) {
      sheet <- 1  # Default to first sheet
    }

    df <- readxl::read_excel(path, sheet = sheet, .name_repair = "unique")
    message_line("  ✓ Loaded: ", nrow(df), " rows × ", ncol(df), " columns")

    # Clean column names
    names(df) <- tolower(names(df))
    names(df) <- str_replace_all(names(df), "[^a-z0-9_]", "_")
    names(df) <- str_replace_all(names(df), "_+", "_")
    names(df) <- str_trim(names(df))

    # Remove empty rows/columns
    df <- df %>%
      filter(if_any(everything(), ~ !is.na(.) & . != "")) %>%
      select_if(~ !all(is.na(.) | . == ""))

    message_line("  ✓ Cleaned: ", nrow(df), " rows × ", ncol(df), " columns")
    return(df)
  }, error = function(e) {
    message_line("  ✗ Error loading ", name, ": ", e$message)
    return(NULL)
  })
}

# Load all datasets
datasets <- list()

# Load alumnos (student performance data)
datasets$alumnos <- load_dataset("alumnos", files$alumnos)

# Load HSE base data
datasets$base_hse <- load_dataset("base_hse", files$base_hse, sheet = "Base de datos")

# Load estudiante data
datasets$estudiante <- load_dataset("estudiante", files$estudiante, sheet = "base")

# Load familia data
datasets$familia <- load_dataset("familia", files$familia, sheet = "base")

# Check if all loaded successfully
failed <- names(datasets)[sapply(datasets, is.null)]
if (length(failed) > 0) {
  message_line("Failed to load: ", paste(failed, collapse = ", "))
  stop("Cannot proceed with missing datasets")
}

# ------------------------------------------------------------------------------
# Examine datasets and identify merge keys
# ------------------------------------------------------------------------------
message_line("\nDataset overview:")
for (name in names(datasets)) {
  df <- datasets[[name]]
  message_line("  ", name, ": ", nrow(df), " × ", ncol(df))
  if (ncol(df) <= 10) {
    message_line("    Columns: ", paste(names(df), collapse = ", "))
  } else {
    message_line("    First 5 columns: ", paste(names(df)[1:5], collapse = ", "))
  }
}

# Identify common ID columns for merging
id_cols <- list(
  alumnos = c("id_estudiante", "id_alumno", "id", "cod_mod7", "cod_mod"),
  estudiante = c("id_estudiante", "id_alumno", "id", "cod_mod7", "cod_mod"),
  familia = c("id_estudiante", "id_familia", "id", "cod_mod7", "cod_mod"),
  base_hse = c("id_estudiante", "id_alumno", "id", "cod_mod7", "cod_mod")
)

# Find actual ID columns in each dataset
actual_ids <- list()
for (name in names(datasets)) {
  df <- datasets[[name]]
  candidates <- id_cols[[name]]

  # Look for columns that might be IDs (contain 'id', 'cod', or 'codigo')
  id_like <- grep("^(id|cod|codigo)", names(df), value = TRUE)

  # Find best matches
  matches <- candidates[candidates %in% names(df)]
  if (length(matches) > 0) {
    actual_ids[[name]] <- matches[1]  # Use first match
  } else if (length(id_like) > 0) {
    actual_ids[[name]] <- id_like[1]  # Use first ID-like column
  } else {
    actual_ids[[name]] <- NA  # No clear ID found
  }
}

message_line("\nIdentified ID columns:")
for (name in names(actual_ids)) {
  message_line("  ", name, ": ", actual_ids[[name]] %||% "NONE")
}

# ------------------------------------------------------------------------------
# Merge datasets
# ------------------------------------------------------------------------------
message_line("\nMerging datasets...")

# Start with alumnos as base (contains performance measures)
merged_data <- datasets$alumnos

preferred_key <- "id_estudiante"

# Deduplicate each dataset by preferred key when available
for (nm in names(datasets)) {
  if (!is.null(datasets[[nm]]) && preferred_key %in% names(datasets[[nm]])) {
    dup_n <- sum(duplicated(datasets[[nm]][[preferred_key]]), na.rm = TRUE)
    if (dup_n > 0) message_line("  ⚠ ", nm, ": ", dup_n, " duplicate ", preferred_key, " values. Keeping first occurrence.")
    datasets[[nm]] <- datasets[[nm]] %>%
      arrange(!!!rlang::syms(intersect(c(preferred_key), names(.)))) %>%
      distinct(!!rlang::sym(preferred_key), .keep_all = TRUE)
  }
}

# Merge by preferred key when present; otherwise fall back to detected id
merge_step <- function(x, y, yname) {
  if (preferred_key %in% names(x) && preferred_key %in% names(y)) {
    suppressWarnings(left_join(x, y, by = preferred_key, relationship = "one-to-one"))
  } else {
    key <- actual_ids[[yname]]
    if (!is.na(key) && key %in% names(x) && key %in% names(y)) {
      left_join(x, y, by = key)
    } else {
      message_line("  ⚠ Skipping merge for ", yname, ": no common key found")
      x
    }
  }
}

# Apply merges
merged_data <- merge_step(merged_data, datasets$base_hse, "base_hse")
message_line("  ✓ Merged base_hse data")
merged_data <- merge_step(merged_data, datasets$estudiante, "estudiante")
message_line("  ✓ Merged estudiante data")
merged_data <- merge_step(merged_data, datasets$familia, "familia")
message_line("  ✓ Merged familia data")

message_line("\nFinal merged dataset:")
message_line("  Dimensions: ", nrow(merged_data), " × ", ncol(merged_data))
uniq_students <- if (preferred_key %in% names(merged_data)) n_distinct(merged_data[[preferred_key]]) else NA_integer_
message_line("  Unique students: ", uniq_students)

# ------------------------------------------------------------------------------
# Data cleaning and type conversion
# ------------------------------------------------------------------------------
message_line("\nCleaning and converting data types...")

# Identify performance measures (columns with 'medida' or scores)
performance_cols <- grep("medida|score|puntaje", names(merged_data), value = TRUE)
if (length(performance_cols) > 0) {
  message_line("  Found performance columns: ", paste(performance_cols, collapse = ", "))
}

# Convert performance measures to numeric
for (col in performance_cols) {
  if (col %in% names(merged_data)) {
    merged_data[[col]] <- suppressWarnings(as.numeric(merged_data[[col]]))
  }
}

# Identify and standardize demographic columns
demo_cols <- list(
  gender = c("sexo", "genero", "gender"),
  area = c("area", "zona", "urbano_rural"),
  school_type = c("gestion", "tipo_gestion", "sector"),
  grade = c("grado", "nivel", "curso")
)

standardized_cols <- list()
for (demo_type in names(demo_cols)) {
  candidates <- demo_cols[[demo_type]]
  found <- candidates[candidates %in% names(merged_data)]
  if (length(found) > 0) {
    standardized_cols[[demo_type]] <- found[1]
    message_line("  Standardized ", demo_type, " column: ", found[1])
  }
}

# ----------------------------------------------------------------------------
# Define primary analysis variables and weights (after merges/cleaning)
# ----------------------------------------------------------------------------
message_line("\nCreating primary analysis variables and weights...")

nm <- names(merged_data)

# Socioemotional learning (from base_hse): pv1.auto_1 weighted by peso_calib
soc_var <- if ("pv1_auto_1" %in% nm) "pv1_auto_1" else NULL
soc_w   <- if ("peso_calib" %in% nm) "peso_calib" else NULL

if (!is.null(soc_var)) {
  merged_data[[soc_var]] <- suppressWarnings(as.numeric(merged_data[[soc_var]]))
  merged_data$socioemotional_learning <- merged_data[[soc_var]]
}
if (!is.null(soc_w)) {
  merged_data[[soc_w]] <- suppressWarnings(as.numeric(merged_data[[soc_w]]))
  merged_data$weight_socio <- merged_data[[soc_w]]
}

# Academic performance (from alumnos): medida500_l, medida500_m with weights
acad_l <- if ("medida500_l" %in% nm) "medida500_l" else NULL
acad_m <- if ("medida500_m" %in% nm) "medida500_m" else NULL
wl <- if ("peso_l" %in% nm) "peso_l" else NULL
wm <- if ("peso_m" %in% nm) "peso_m" else NULL

if (!is.null(acad_l)) {
  merged_data[[acad_l]] <- suppressWarnings(as.numeric(merged_data[[acad_l]]))
  merged_data$academic_lang <- merged_data[[acad_l]]
}
if (!is.null(acad_m)) {
  merged_data[[acad_m]] <- suppressWarnings(as.numeric(merged_data[[acad_m]]))
  merged_data$academic_math <- merged_data[[acad_m]]
}
if (!is.null(wl)) {
  merged_data[[wl]] <- suppressWarnings(as.numeric(merged_data[[wl]]))
  merged_data$weight_lang <- merged_data[[wl]]
}
if (!is.null(wm)) {
  merged_data[[wm]] <- suppressWarnings(as.numeric(merged_data[[wm]]))
  merged_data$weight_math <- merged_data[[wm]]
}

message_line("  ✓ Variables ready: ", paste(intersect(c("socioemotional_learning","academic_lang","academic_math"), names(merged_data)), collapse = ", "))
message_line("  ✓ Weights found: ", paste(intersect(c("weight_socio","weight_lang","weight_math"), names(merged_data)), collapse = ", "))

# ----------------------------------------------------------------------------
# Standardize key categorical variables across possible suffix variants
# ----------------------------------------------------------------------------
std_pick <- function(df, candidates) {
  present <- intersect(candidates, names(df))
  if (length(present) == 0) return(NULL)
  # Coalesce in order, coercing to character to avoid type conflicts
  out <- as.character(df[[present[1]]])
  if (length(present) > 1) {
    for (i in 2:length(present)) {
      nxt <- as.character(df[[present[i]]])
      # fill NAs in out with nxt
      nas <- is.na(out) | out == ""
      out[nas] <- nxt[nas]
    }
  }
  # Normalize blanks to NA
  out[!nzchar(trimws(out))] <- NA_character_
  out
}

merged_data$sexo_std <- std_pick(merged_data, c("sexo","genero","gender","sexo.x","sexo.y","genero.x","genero.y","gender.x","gender.y"))
merged_data$area_std <- std_pick(merged_data, c("area","zona","area.x","area.y","zona.x","zona.y"))
merged_data$gestion2_std <- std_pick(merged_data, c("gestion2","gestion","tipo_gestion","sector","gestion2.x","gestion2.y","gestion.x","gestion.y","tipo_gestion.x","tipo_gestion.y","sector.x","sector.y"))
merged_data$lengua_materna_std <- std_pick(merged_data, c("lengua_materna","lengua","lengua_materna_est","lengua_materna_estudiante",
                                                         "lengua_materna.x","lengua_materna.y","lengua.x","lengua.y"))

# ------------------------------------------------------------------------------
# Create analysis subsets
# ------------------------------------------------------------------------------
message_line("\nCreating analysis subsets...")

## Minimal analysis-ready dataset focused on primary variables + weights
analysis_vars <- c(
  preferred_key,
  "socioemotional_learning", "weight_socio",
  "academic_lang", "weight_lang",
  "academic_math", "weight_math",
  unlist(standardized_cols),
  # Explicit categorical variables requested for EDA coloring
  "sexo_std", "area_std", "gestion2_std", "lengua_materna_std"
)

analysis_data <- merged_data %>%
  select(any_of(analysis_vars)) %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  drop_na(any_of(c("socioemotional_learning")))
message_line("  Complete cases for performance analysis: ", nrow(analysis_data))

# Save datasets
saveRDS(list(
  raw = datasets,
  merged = merged_data,
  analysis = analysis_data,
  metadata = list(
    merge_date = Sys.time(),
    id_columns = actual_ids,
    performance_columns = performance_cols,
    demographic_columns = standardized_cols,
    n_students = nrow(merged_data),
    n_complete = nrow(analysis_data)
  )
), file.path(preprocessed_dir, "enla_preprocessed.rds"))

message_line("  ✓ Saved preprocessed data to: ", file.path(preprocessed_dir, "enla_preprocessed.rds"))

# Save CSV for external tools
readr::write_csv(analysis_data, file.path(preprocessed_dir, "enla_analysis_ready.csv"))
message_line("  ✓ Saved analysis CSV to: ", file.path(preprocessed_dir, "enla_analysis_ready.csv"))

# ------------------------------------------------------------------------------
# Summary report
# ------------------------------------------------------------------------------
message_line("")
message_line(rule())
message_line("PREPROCESSING COMPLETE")
message_line(rule())

message_line("\nFinal datasets:")
message_line("  - Merged data: ", nrow(merged_data), " students × ", ncol(merged_data), " variables")
message_line("  - Analysis data: ", nrow(analysis_data), " complete cases × ", ncol(analysis_data), " variables")

message_line("\nKey variables identified:")
message_line("  - Performance measures: ", length(performance_cols))
message_line("  - Demographic variables: ", length(standardized_cols))

message_line("\nReady for next pipeline steps:")
message_line("  1. Run 03_eda.R for exploratory data analysis")
message_line("  2. Run 04_pca.R for principal component analysis")
message_line("  3. Run 05_pc_algorithm.R for causal discovery")
message_line("  4. Run 06_graphical_lasso.R for network estimation")

message_line("")
message_line("✓ Preprocessing pipeline complete!")
