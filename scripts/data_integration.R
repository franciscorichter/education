# Complete Data Integration Pipeline for ENLA Platform
# Loads raw Excel files, normalizes columns, performs matching, and saves processed data

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(rlang)
})

# Resolve paths relative to this script's directory
get_script_dir <- function() {
  # Works when sourced()
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  # Fallback for Rscript execution
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("--file=", "", file_arg[1]))))
  }
  # Last resort: current working directory
  return(normalizePath(getwd(), mustWork = TRUE))
}

SCRIPT_DIR <- get_script_dir()

# Configuration (always relative to project layout)
DATA_DIR <- file.path(SCRIPT_DIR, "../data/xlsx")
OUTPUT_DIR <- file.path(SCRIPT_DIR, "../data")
CACHE_VERSION <- 6L

cat("🚀 Starting ENLA Data Integration Pipeline v", CACHE_VERSION, "\n")
cat("===========================================\n\n")

# ===== UTILITY FUNCTIONS =====

# Enhanced column name standardization with comprehensive mapping
standardize_column_names <- function(df) {
  cn <- names(df)
  nml <- tolower(cn)

  cat("  📋 Original columns:", paste(cn, collapse = ", "), "\n")

  # Map ID_ESTUDIANTE variations
  id_patterns <- c("id_estudiante", "idestudiante", "id_est", "estudiante_id", "estudianteid", "id_estudiante")
  idx <- which(nml %in% id_patterns)
  if (length(idx) > 0) {
    old_names <- cn[idx]
    cn[idx] <- "ID_ESTUDIANTE"
    cat("  ✅ Standardized ID_ESTUDIANTE from:", paste(old_names, collapse = ", "), "\n")
  }

  # Map cod_mod7 variations
  cod_patterns <- c("cod_mod7", "codmod7", "codigo_modular", "cod_mod", "mod7", "codmod", "codigo_mod")
  idx <- which(nml %in% cod_patterns)
  if (length(idx) > 0) {
    old_names <- cn[idx]
    cn[idx] <- "cod_mod7"
    cat("  ✅ Standardized cod_mod7 from:", paste(old_names, collapse = ", "), "\n")
  }

  # Map anexo variations
  anexo_patterns <- c("anexo", "anx", "annexo", "anexo_escuela", "anexoescuela")
  idx <- which(nml %in% anexo_patterns)
  if (length(idx) > 0) {
    old_names <- cn[idx]
    cn[idx] <- "anexo"
    cat("  ✅ Standardized anexo from:", paste(old_names, collapse = ", "), "\n")
  }

  # Map ID_seccion variations
  seccion_patterns <- c("id_seccion", "idseccion", "seccion_id", "seccion", "id_sec", "seccionid", "idseccion")
  idx <- which(nml %in% seccion_patterns)
  if (length(idx) > 0) {
    old_names <- cn[idx]
    cn[idx] <- "ID_seccion"
    cat("  ✅ Standardized ID_seccion from:", paste(old_names, collapse = ", "), "\n")
  }

  # Map medida500 variations
  medida_patterns <- c("medida500_l", "medida500l", "lectura", "medida_lectura", "puntaje_lectura", "punt_lect", "lectura500")
  idx <- which(nml %in% medida_patterns)
  if (length(idx) > 0) {
    old_names <- cn[idx]
    cn[idx] <- "medida500_L"
    cat("  ✅ Standardized medida500_L from:", paste(old_names, collapse = ", "), "\n")
  }

  medida_patterns <- c("medida500_m", "medida500m", "matematica", "medida_matematica", "puntaje_matematica", "punt_mat", "matematica500")
  idx <- which(nml %in% medida_patterns)
  if (length(idx) > 0) {
    old_names <- cn[idx]
    cn[idx] <- "medida500_M"
    cat("  ✅ Standardized medida500_M from:", paste(old_names, collapse = ", "), "\n")
  }

  names(df) <- cn
  cat("  📋 Final standardized columns:", paste(names(df), collapse = ", "), "\n")
  return(df)
}

# ===== DATA LOADING FUNCTIONS =====

# Load EM performance data
load_em_data <- function() {
  em_file <- file.path(DATA_DIR, "EM_6P_2024_alumnos_innominados.xlsx")
  if (!file.exists(em_file)) {
    warning("EM performance data not found")
    return(NULL)
  }

  cat("🔍 Loading EM data...\n")

  # Read EM data
  sheets <- tryCatch(readxl::excel_sheets(em_file), error = function(e) character())
  sheet_to_read <- if ("BD" %in% sheets) "BD" else if (length(sheets) > 0) sheets[1] else 1
  em_data <- tryCatch(readxl::read_excel(em_file, sheet = sheet_to_read), error = function(e) NULL)

  if (is.null(em_data)) {
    warning("Could not read EM data")
    return(NULL)
  }

  em_data <- tibble::as_tibble(em_data)

  # Standardize column names
  cat("  🔧 Standardizing EM column names...\n")
  em_data <- standardize_column_names(em_data)

  # Do NOT drop columns here; retain full EM dataset so downstream EDA/grouping can use extra variables
  # (We still standardize names above and coerce L/M to numeric below.)

  # Ensure column names are unique (some sources repeat names like ID_seccion)
  if (any(duplicated(names(em_data)))) {
    dups <- unique(names(em_data)[duplicated(names(em_data))])
    warning("Duplicate column names detected in EM data: ", paste(dups, collapse = ", "), 
            ". Making names unique to proceed.")
    names(em_data) <- make.unique(names(em_data), sep = ".")
  }

  # Coerce scores to numeric
  if ("medida500_L" %in% names(em_data)) em_data$medida500_L <- suppressWarnings(as.numeric(em_data$medida500_L))
  if ("medida500_M" %in% names(em_data)) em_data$medida500_M <- suppressWarnings(as.numeric(em_data$medida500_M))

  cat(sprintf("  ✅ EM data loaded: %d rows, %d columns\n", nrow(em_data), ncol(em_data)))
  cat("  📋 Available columns:", paste(names(em_data), collapse = ", "), "\n")

  return(em_data)
}

# Load questionnaire data
load_questionnaire_data <- function(xlsx_path) {
  if (!file.exists(xlsx_path)) {
    return(list(
      data = NULL,
      columns = character(0),
      error = "File not found",
      success = FALSE
    ))
  }

  base_name <- tools::file_path_sans_ext(basename(xlsx_path))
  cat("🔍 Loading questionnaire:", base_name, "\n")

  # Read questionnaire data
  sheets <- tryCatch(readxl::excel_sheets(xlsx_path), error = function(e) character())
  sheet_to_read <- if ("BD" %in% sheets) "BD" else if (length(sheets) > 0) sheets[1] else "BD"

  df <- tryCatch({
    readxl::read_excel(xlsx_path, sheet = sheet_to_read)
  }, error = function(e) {
    return(list(
      data = NULL,
      columns = character(0),
      error = paste("Failed to read sheet:", e$message),
      success = FALSE
    ))
  })

  if (is.list(df) && !is.null(df$error)) {
    return(df)
  }

  df <- tibble::as_tibble(df)

  # Standardize column names
  cat("  🔧 Standardizing questionnaire column names...\n")
  df <- standardize_column_names(df)

  # Find item columns (pXX pattern)
  col_names <- names(df)
  p_cols <- col_names[grepl("^(p\\d{2})(_\\d{2})?$", col_names, ignore.case = TRUE)]

  if (length(p_cols) < 2) {
    return(list(
      data = NULL,
      columns = character(0),
      error = paste("Insufficient pXX columns found:", length(p_cols)),
      success = FALSE
    ))
  }

  # Process item data
  X <- tryCatch(dplyr::select(df, dplyr::all_of(p_cols)), error = function(e) NULL)
  if (is.null(X)) {
    return(list(
      data = NULL,
      columns = character(0),
      error = "Failed to select item columns",
      success = FALSE
    ))
  }

  # Coerce responses to numeric
  X[] <- lapply(X, function(x) suppressWarnings(as.numeric(x)))

  return(list(
    data = X,
    columns = p_cols,
    full_data = df,
    available_columns = names(df),
    success = TRUE
  ))
}

# ===== INTEGRATION FUNCTIONS =====

# Create EM summaries for different granularities
aggregate_em_data <- function(em_data) {
  # Ensure expected columns exist
  required <- c("cod_mod7", "anexo", "ID_seccion", "ID_ESTUDIANTE", "medida500_L", "medida500_M")
  missing <- setdiff(required, names(em_data))
  if (length(missing) > 0) {
    warning("EM data missing columns: ", paste(missing, collapse = ", "))
  }

  # Student-level (as-is)
  em_student <- em_data

  # Section-level aggregation (for Docentes)
  have_section <- all(c("cod_mod7", "anexo", "ID_seccion") %in% names(em_data))
  if (have_section) {
    em_section <- em_data %>%
      dplyr::group_by(.data$cod_mod7, .data$anexo, .data$ID_seccion) %>%
      dplyr::summarise(
        medida500_L = suppressWarnings(mean(.data$medida500_L, na.rm = TRUE)),
        medida500_M = suppressWarnings(mean(.data$medida500_M, na.rm = TRUE)),
        n_students = dplyr::n(),
        .groups = "drop"
      )
  } else {
    em_section <- tibble::tibble()
  }

  # Site-level aggregation (for Directores)
  have_site <- all(c("cod_mod7", "anexo") %in% names(em_data))
  if (have_site) {
    em_site <- em_data %>%
      dplyr::group_by(.data$cod_mod7, .data$anexo) %>%
      dplyr::summarise(
        medida500_L = suppressWarnings(mean(.data$medida500_L, na.rm = TRUE)),
        medida500_M = suppressWarnings(mean(.data$medida500_M, na.rm = TRUE)),
        n_students = dplyr::n(),
        .groups = "drop"
      )
  } else {
    em_site <- tibble::tibble()
  }

  list(
    em_student = em_student,
    em_section = em_section,
    em_site = em_site
  )
}

# Dynamic integration based on user-selected columns
perform_dynamic_integration <- function(questionnaire_data, em_data, match_columns) {
  if (is.null(questionnaire_data) || is.null(em_data)) {
    return(list(error = "Missing data", success = FALSE))
  }

  cat("🔍 Starting dynamic integration...\n")
  cat("  📋 Questionnaire columns:", paste(names(questionnaire_data), collapse = ", "), "\n")
  cat("  📋 EM columns:", paste(names(em_data), collapse = ", "), "\n")
  cat("  🎯 Requested match columns:", paste(match_columns, collapse = ", "), "\n")

  # Check if match columns exist in both datasets
  missing_in_questionnaire <- match_columns[!match_columns %in% names(questionnaire_data)]
  missing_in_em <- match_columns[!match_columns %in% names(em_data)]

  if (length(missing_in_questionnaire) > 0) {
    cat("  ❌ Missing in questionnaire:", paste(missing_in_questionnaire, collapse = ", "), "\n")
    return(list(
      error = paste("Columns not found in questionnaire:", paste(missing_in_questionnaire, collapse = ", ")),
      success = FALSE
    ))
  }

  if (length(missing_in_em) > 0) {
    cat("  ❌ Missing in EM:", paste(missing_in_em, collapse = ", "), "\n")
    return(list(
      error = paste("Columns not found in EM data:", paste(missing_in_em, collapse = ", ")),
      success = FALSE
    ))
  }

  # Normalize cod_mod7 if present
  if ("cod_mod7" %in% match_columns) {
    cat("  🔧 Normalizing cod_mod7...\n")
    questionnaire_data$cod_mod7 <- as.character(questionnaire_data$cod_mod7)
    em_data$cod_mod7 <- as.character(em_data$cod_mod7)

    questionnaire_data$cod_mod7 <- gsub("[^0-9]", "", questionnaire_data$cod_mod7)
    em_data$cod_mod7 <- gsub("[^0-9]", "", em_data$cod_mod7)

    questionnaire_data$cod_mod7[nchar(questionnaire_data$cod_mod7) > 7] <-
      substr(questionnaire_data$cod_mod7[nchar(questionnaire_data$cod_mod7) > 7],
             nchar(questionnaire_data$cod_mod7[nchar(questionnaire_data$cod_mod7) > 7]) - 6,
             nchar(questionnaire_data$cod_mod7[nchar(questionnaire_data$cod_mod7) > 7]))

    questionnaire_data$cod_mod7 <- sprintf("%07s", questionnaire_data$cod_mod7)
    em_data$cod_mod7 <- sprintf("%07s", em_data$cod_mod7)
  }

  # Create a combined key for joining
  create_join_key <- function(df, keys) {
    cat("  🔑 Creating join keys from:", paste(keys, collapse = ", "), "\n")
    key_parts <- lapply(keys, function(key) {
      if (key %in% names(df)) {
        as.character(df[[key]])
      } else {
        rep(NA_character_, nrow(df))
      }
    })
    do.call(paste, c(key_parts, sep = "|||"))
  }

  # Add join keys to both dataframes
  questionnaire_data$join_key <- create_join_key(questionnaire_data, match_columns)
  em_data$join_key <- create_join_key(em_data, match_columns)

  # Perform the join
  cat("  🔗 Performing left join...\n")
  result <- dplyr::left_join(questionnaire_data, em_data, by = "join_key")

  # Calculate match statistics
  total_rows <- nrow(questionnaire_data)
  measure_cols <- intersect(c("medida500_L", "medida500_M"), names(result))
  if (length(measure_cols) > 0) {
    matched_rows <- sum(rowSums(!is.na(result[measure_cols])) > 0)
  } else {
    matched_rows <- 0L
  }
  match_rate <- if (total_rows > 0) matched_rows / total_rows else 0

  cat(sprintf("  ✅ Integration completed: %d/%d matched (%.1f%%)\n",
              matched_rows, total_rows, match_rate * 100))

  # Clean up the join key column
  result$join_key <- NULL

  return(list(
    integrated_data = result,
    match_rate = match_rate,
    matched_rows = matched_rows,
    total_rows = total_rows,
    match_columns = match_columns,
    success = TRUE
  ))
}

# Perform comprehensive matching for all questionnaires
perform_comprehensive_matching <- function(questionnaire_data, em_summaries) {
  cat("🔄 Performing comprehensive matching for all questionnaires...\n")

  results <- list()

  for (q_name in names(questionnaire_data)) {
    cat(sprintf("  📊 Processing: %s\n", q_name))

    q_data <- questionnaire_data[[q_name]]
    if (!q_data$success) {
      cat("    ❌ Skipping - data not available\n")
      next
    }

    # Determine best matching strategy based on questionnaire type
    q_lower <- tolower(q_name)

    if (grepl("estudiante|familia", q_lower)) {
      # Estudiante/Familia: Use ID_ESTUDIANTE
      match_cols <- "ID_ESTUDIANTE"
      em_for_join <- em_summaries$em_student
      cat("    🎯 Strategy: Estudiante/Familia -> ID_ESTUDIANTE (student-level EM)\n")
    } else if (grepl("docente", q_lower)) {
      # Docente: Use cod_mod7 + anexo + ID_seccion
      match_cols <- c("cod_mod7", "anexo", "ID_seccion")
      em_for_join <- em_summaries$em_section
      cat("    🎯 Strategy: Docente -> cod_mod7 + anexo + ID_seccion (section-level EM mean)\n")
    } else if (grepl("director", q_lower)) {
      # Director: Use cod_mod7 + anexo
      match_cols <- c("cod_mod7", "anexo")
      em_for_join <- em_summaries$em_site
      cat("    🎯 Strategy: Director -> cod_mod7 + anexo (site-level EM mean)\n")
    } else {
      # Default: Try ID_ESTUDIANTE first
      match_cols <- "ID_ESTUDIANTE"
      em_for_join <- em_summaries$em_student
      cat("    🎯 Strategy: Default -> ID_ESTUDIANTE (student-level EM)\n")
    }

    # Check if all required columns exist
    available_cols <- intersect(match_cols, names(q_data$full_data))
    if (length(available_cols) < length(match_cols) && length(match_cols) > 1) {
      # For composite keys, try individual columns if composite fails
      available_cols <- intersect(match_cols, names(q_data$full_data))
      if (length(available_cols) == 0) {
        cat("    ⚠️ No matching columns available, skipping\n")
        next
      }
    }

    # Perform integration
    integration_result <- perform_dynamic_integration(
      q_data$full_data,
      em_for_join,
      available_cols
    )

    if (integration_result$success) {
      results[[q_name]] <- integration_result
      cat(sprintf("    ✅ Success: %d/%d matched (%.1f%%)\n",
                  integration_result$matched_rows,
                  integration_result$total_rows,
                  integration_result$match_rate * 100))
    } else {
      cat("    ❌ Failed:", integration_result$error, "\n")
    }
  }

  return(results)
}

# ===== MAIN PIPELINE =====

# Complete data processing pipeline
run_enla_data_pipeline <- function() {
  cat("🚀 Starting ENLA Data Integration Pipeline\n")
  cat("=========================================\n\n")

  # Step 1: Load EM data
  cat("📥 STEP 1: Loading EM data\n")
  cat("-------------------------\n")
  em_data <- load_em_data()
  if (is.null(em_data)) {
    stop("❌ Failed to load EM data - pipeline cannot continue")
  }
  cat("\n")

  # Step 2: Load questionnaire data
  cat("📥 STEP 2: Loading questionnaire data\n")
  cat("-----------------------------------\n")
  xlsx_files <- list.files(DATA_DIR, pattern = "ENLA.*\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
  xlsx_files <- xlsx_files[!grepl("EM_", basename(xlsx_files), ignore.case = TRUE)]  # Exclude EM file

  cat(sprintf("  📋 Found %d questionnaire files\n", length(xlsx_files)))

  questionnaire_data <- list()
  data_summary <- list()

  for (xlsx_path in xlsx_files) {
    base_name <- tools::file_path_sans_ext(basename(xlsx_path))
    cat(sprintf("  🔍 Loading: %s\n", base_name))

    # Load questionnaire data
    result <- load_questionnaire_data(xlsx_path)

    if (result$success) {
      questionnaire_data[[base_name]] <- result
      data_summary[[base_name]] <- list(
        rows = nrow(result$data),
        item_columns = length(result$columns),
        available_columns = result$available_columns,
        full_column_count = length(result$available_columns)
      )
      cat(sprintf("    ✅ %d rows, %d item columns, %d total columns\n",
                  nrow(result$data), length(result$columns), length(result$available_columns)))
    } else {
      cat(sprintf("    ❌ %s\n", result$error))
      questionnaire_data[[base_name]] <- result
    }
  }
  cat("\n")

  # Step 3: Perform comprehensive matching
  cat("🔗 STEP 3: Performing comprehensive matching\n")
  cat("------------------------------------------\n")
  # Build EM summaries at required granularities
  em_summaries <- aggregate_em_data(em_data)
  integration_results <- perform_comprehensive_matching(questionnaire_data, em_summaries)
  cat("\n")

  # Step 3b: Append EM columns into each questionnaire's full_data and export CSVs
  cat("🧩 STEP 3b: Appending EM averages into questionnaire full_data and exporting CSVs\n")
  cat("--------------------------------------------------------------------------------\n")
  integrated_dir <- file.path(OUTPUT_DIR, "integrated")
  dir.create(integrated_dir, recursive = TRUE, showWarnings = FALSE)

  for (q_name in names(integration_results)) {
    res <- integration_results[[q_name]]
    if (!isTRUE(res$success)) next

    df_joined <- res$integrated_data
    # Overwrite questionnaire full_data with integrated version (now includes EM averages)
    if (!is.null(questionnaire_data[[q_name]]) && !is.null(questionnaire_data[[q_name]]$full_data)) {
      questionnaire_data[[q_name]]$full_data <- df_joined
    }

    # Export CSV with a compact set plus all items
    em_cols <- intersect(c("medida500_L", "medida500_M", "n_students"), names(df_joined))
    id_cols <- intersect(c("ID_ESTUDIANTE", "cod_mod7", "anexo", "ID_seccion"), names(df_joined))
    p_cols <- grep("^(p\\d{2})(_\\d{2})?$", names(df_joined), ignore.case = TRUE, value = TRUE)
    export_cols <- unique(c(id_cols, em_cols, p_cols))

    out_path <- file.path(integrated_dir, paste0(q_name, "_integrated.csv"))
    readr::write_csv(df_joined[, export_cols, drop = FALSE], out_path)

    # Verbose diagnostics
    total_rows <- res$total_rows
    matched_rows <- res$matched_rows
    match_rate <- res$match_rate
    cat(sprintf("  📄 %s: %d rows | matched %d (%.1f%%) | saved -> %s\n",
                q_name, total_rows, matched_rows, match_rate * 100, out_path))

    # Show a short sample of rows without EM (if any) to aid debugging
    measure_cols <- intersect(c("medida500_L", "medida500_M"), names(df_joined))
    if (length(measure_cols) > 0) {
      no_em <- df_joined[rowSums(is.na(df_joined[measure_cols])) == length(measure_cols), ]
      if (nrow(no_em) > 0) {
        cat(sprintf("    ⚠️  %d rows without EM after integration (showing up to 3):\n", nrow(no_em)))
        show_cols <- unique(c(id_cols, measure_cols))
        print(utils::head(no_em[, show_cols, drop = FALSE], 3))
      } else {
        cat("    ✅ All rows have EM values (at least one of L/M)\n")
      }
    }
  }
  cat("\n")

  # Step 4: Create comprehensive results
  cat("📊 STEP 4: Creating comprehensive results\n")
  cat("----------------------------------------\n")

  # Summary statistics
  total_questionnaires <- length(questionnaire_data)
  successful_loads <- sum(sapply(questionnaire_data, function(x) x$success))
  successful_integrations <- length(integration_results)

  cat(sprintf("  📈 Pipeline Summary:\n"))
  cat(sprintf("    - Total questionnaires: %d\n", total_questionnaires))
  cat(sprintf("    - Successfully loaded: %d\n", successful_loads))
  cat(sprintf("    - Successfully integrated: %d\n", successful_integrations))

  if (successful_integrations > 0) {
    match_rates <- sapply(integration_results, function(x) x$match_rate)
    cat(sprintf("    - Match rates: %.1f%% - %.1f%% (avg: %.1f%%)\n",
                min(match_rates) * 100, max(match_rates) * 100, mean(match_rates) * 100))
  }
  cat("\n")

  # Step 5: Save comprehensive results
  cat("💾 STEP 5: Saving comprehensive results\n")
  cat("-------------------------------------\n")

  result <- list(
    em_data = em_data,
    questionnaire_data = questionnaire_data,
    integration_results = integration_results,
    data_summary = data_summary,
    pipeline_stats = list(
      total_questionnaires = total_questionnaires,
      successful_loads = successful_loads,
      successful_integrations = successful_integrations,
      match_rate_summary = if (successful_integrations > 0) {
        list(min = min(match_rates), max = max(match_rates), mean = mean(match_rates))
      } else { NULL }
    ),
    processed_at = Sys.time(),
    version = CACHE_VERSION
  )

  # Ensure output directory exists before saving
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, file.path(OUTPUT_DIR, "enla_processed_data.rds"))
  cat("  💾 Saved comprehensive data to: data/enla_processed_data.rds\n")
  cat("\n")

  cat("🎉 ENLA Data Integration Pipeline completed successfully!\n")
  cat("=======================================================\n")

  return(result)
}

# ===== RUN PIPELINE (manual) =====
# To run from terminal:
#   R -e "source('scripts/data_integration.R'); run_enla_data_pipeline()"
