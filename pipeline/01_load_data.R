#!/usr/bin/env Rscript
# ==============================================================================
# Pipeline Step 01: Load Raw Excel Datasets
# ==============================================================================
# Loads four Excel datasets and reports load status and dimensions.
#
# Datasets:
# 1) EM_6P_2024_alumnos_innominados.xlsx
# 2) base_web2_HSE_ENLA_2024.xlsx
# 3) ENLA2024_6Pestudiante_EBRD1.xlsx
# 4) ENLA2024_6Pfamilia_EBR.xlsx
#
# Output: Prints per-dataset status, sheet info, and data.frame dimensions
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tibble)
  library(stringr)
})

message_line <- function(...) cat(paste0(..., "\n"))
rule <- function(char = "=") paste0(rep(char, 80), collapse = "")

# ------------------------------------------------------------------------------
# File registry
# ------------------------------------------------------------------------------
files <- list(
  alumnos = "/Users/pancho/Library/CloudStorage/Dropbox/25 - CSG/04 - Education/education-main/data/xlsx/EM_6P_2024_alumnos_innominados.xlsx",
  base_hse = "/Users/pancho/Library/CloudStorage/Dropbox/25 - CSG/04 - Education/education-main/data/xlsx/base_web2_HSE_ENLA_2024.xlsx",
  estudiante = "/Users/pancho/Library/CloudStorage/Dropbox/25 - CSG/04 - Education/others/EM 6P 2024 FFAA/__report_6P/data/ENLA2024_6Pestudiante_EBRD1.xlsx",
  familia = "/Users/pancho/Library/CloudStorage/Dropbox/25 - CSG/04 - Education/education-main/data/xlsx/ENLA2024_6Pfamilia_EBR.xlsx"
)

message_line(rule())
message_line("PIPELINE 01 - LOAD RAW EXCEL DATASETS")
message_line(rule())

# Return a safe list with status and dimensions without throwing
safe_load_excel <- function(path) {
  res <- list(ok = FALSE, sheets = NULL, sheet_used = NA_character_, nrow = NA_integer_, ncol = NA_integer_, error = NULL)
  if (!file.exists(path)) {
    res$error <- paste0("File not found: ", path)
    return(res)
  }
  # Try to list sheets first
  sh <- tryCatch(readxl::excel_sheets(path), error = function(e) NULL)
  res$sheets <- sh
  # For alumnos file, use "BD" sheet; for others use first sheet
  if (basename(path) == "EM_6P_2024_alumnos_innominados.xlsx" && "BD" %in% sh) {
    sheet_to_use <- "BD"
  } else {
    sheet_to_use <- if (!is.null(sh) && length(sh) > 0) sh[1] else 1
  }

  # Special handling for alumnos file - read full file with better strategy
  if (basename(path) == "EM_6P_2024_alumnos_innominados.xlsx") {
    tryCatch({
      # For alumnos, read the full BD sheet but handle potential memory issues
      # First check if we can read a larger sample
      df_large <- suppressWarnings(readxl::read_excel(path, sheet = sheet_to_use, n_max = 10000, .name_repair = "unique"))
      if (nrow(df_large) == 10000) {
        # If we got exactly 10000 rows, there might be more data
        # Try reading even more
        df_full <- suppressWarnings(readxl::read_excel(path, sheet = sheet_to_use, .name_repair = "unique"))
        if (nrow(df_full) > nrow(df_large)) {
          # Success - we got more rows
          res$ok <- TRUE
          res$sheet_used <- as.character(sheet_to_use)
          res$nrow <- nrow(df_full)
          res$ncol <- ncol(df_full)
        } else {
          # Use the large sample
          res$ok <- TRUE
          res$sheet_used <- as.character(sheet_to_use)
          res$nrow <- nrow(df_large)
          res$ncol <- ncol(df_large)
        }
      } else {
        # We got all available rows
        res$ok <- TRUE
        res$sheet_used <- as.character(sheet_to_use)
        res$nrow <- nrow(df_large)
        res$ncol <- ncol(df_large)
      }
    }, error = function(e) {
      res$error <- paste0("Error reading alumnos file - trying alternative approach: ", as.character(e))
      # Fallback: try reading without size limit but with error suppression
      tryCatch({
        df <- suppressWarnings(readxl::read_excel(path, sheet = sheet_to_use, .name_repair = "unique"))
        res$ok <- TRUE
        res$sheet_used <- as.character(sheet_to_use)
        res$nrow <- nrow(df)
        res$ncol <- ncol(df)
      }, error = function(e2) {
        res$error <- paste0("Complete failure reading alumnos file: ", as.character(e2))
      })
    })
  } else {
    # Normal reading for other files
    df <- tryCatch(readxl::read_excel(path, sheet = sheet_to_use, .name_repair = "unique"),
                   error = function(e) e)
    if (inherits(df, "error")) {
      res$error <- df$message
    } else {
      res$ok <- TRUE
      res$sheet_used <- as.character(sheet_to_use)
      res$nrow <- nrow(df)
      res$ncol <- ncol(df)
    }
  }
  res
}

summary_tbl <- tibble(
  dataset = names(files),
  path = unname(unlist(files)),
  exists = file.exists(unname(unlist(files))),
  sheets = NA_character_,
  sheet_used = NA_character_,
  nrow = NA_integer_,
  ncol = NA_integer_,
  status = NA_character_,
  error = NA_character_
)

for (i in seq_len(nrow(summary_tbl))) {
  p <- summary_tbl$path[i]
  res <- safe_load_excel(p)
  summary_tbl$sheets[i] <- if (!is.null(res$sheets)) paste0(res$sheets, collapse = ", ") else NA_character_
  summary_tbl$sheet_used[i] <- res$sheet_used
  summary_tbl$nrow[i] <- res$nrow
  summary_tbl$ncol[i] <- res$ncol
  summary_tbl$status[i] <- if (res$ok) "LOADED" else if (!summary_tbl$exists[i]) "MISSING" else "ERROR"
  summary_tbl$error[i] <- if (is.null(res$error) || length(res$error) == 0) NA_character_ else as.character(res$error)
}

# Pretty print per-file report
for (i in seq_len(nrow(summary_tbl))) {
  message_line("")
  message_line("- ", toupper(summary_tbl$dataset[i]))
  message_line("  Path: ", summary_tbl$path[i])
  message_line("  Exists: ", summary_tbl$exists[i])
  message_line("  Status: ", summary_tbl$status[i])
  if (!is.na(summary_tbl$sheets[i])) message_line("  Sheets: ", str_trunc(summary_tbl$sheets[i], 200))
  if (!is.na(summary_tbl$sheet_used[i])) message_line("  Sheet used: ", summary_tbl$sheet_used[i])
  if (!is.na(summary_tbl$nrow[i])) message_line("  Dimensions: ", summary_tbl$nrow[i], " x ", summary_tbl$ncol[i])
  if (!is.na(summary_tbl$error[i]) && !is.null(summary_tbl$error[i])) {
    error_msg <- summary_tbl$error[i]
    if (length(error_msg) > 0 && error_msg != "") {
      message_line("  Error: ", error_msg)
    }
  }
}

message_line("")
message_line(rule())
message_line("SUMMARY")
message_line(rule())
print(summary_tbl %>% select(dataset, status, nrow, ncol))

# Exit code: non-zero if any load failed (but after printing summary)
failed <- any(summary_tbl$status %in% c("MISSING", "ERROR"))
if (failed) {
  message_line("")
  message_line("One or more datasets failed to load. Please review the paths and errors above.")
  quit(status = 1)
} else {
  message_line("")
  message_line("All datasets loaded successfully.")
}
