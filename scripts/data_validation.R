# Data Validation Script for ENLA Platform
# Loads processed data and provides comprehensive validation and reporting

cat("📊 ENLA Data Validation Report\n")
cat("==============================\n\n")

# Load processed data
data_file <- "data/enla_processed_data.rds"
if (!file.exists(data_file)) {
  stop("❌ Processed data file not found. Run data_integration.R first.")
}

cat("📥 Loading processed data...\n")
data <- readRDS(data_file)

cat("✅ Data loaded successfully\n\n")

# ===== COMPREHENSIVE VALIDATION REPORT =====

cat("🔍 COMPREHENSIVE VALIDATION REPORT\n")
cat("==================================\n\n")

# 1. Basic Structure Validation
cat("📋 1. BASIC STRUCTURE VALIDATION\n")
cat("--------------------------------\n")
required_elements <- c("em_data", "questionnaire_data", "integration_results", "data_summary", "pipeline_stats", "processed_at", "version")
actual_elements <- names(data)

missing <- setdiff(required_elements, actual_elements)
extra <- setdiff(actual_elements, required_elements)

if (length(missing) == 0) {
  cat("  ✅ All required elements present\n")
} else {
  cat("  ❌ Missing elements:", paste(missing, collapse = ", "), "\n")
}

if (length(extra) > 0) {
  cat("  ℹ️ Extra elements (may be OK):", paste(extra, collapse = ", "), "\n")
}

cat(sprintf("  📅 Processed at: %s\n", data$processed_at))
cat(sprintf("  🔄 Version: %d\n", data$version))
cat("\n")

# 2. EM Data Validation
cat("📊 2. EM DATA VALIDATION\n")
cat("------------------------\n")
if (!is.null(data$em_data)) {
  cat(sprintf("  ✅ EM data available: %d rows, %d columns\n", nrow(data$em_data), ncol(data$em_data)))
  cat("  📋 Columns:", paste(names(data$em_data), collapse = ", "), "\n")

  # Check for required columns
  required_cols <- c("ID_ESTUDIANTE", "medida500_L", "medida500_M")
  available_cols <- intersect(required_cols, names(data$em_data))
  missing_cols <- setdiff(required_cols, names(data$em_data))

  if (length(missing_cols) == 0) {
    cat("  ✅ All required EM columns present\n")
  } else {
    cat("  ❌ Missing EM columns:", paste(missing_cols, collapse = ", "), "\n")
  }

  # Check for data quality issues
  na_counts <- colSums(is.na(data$em_data))
  na_pct <- na_counts / nrow(data$em_data) * 100

  cat("  📊 Data completeness:\n")
  for (col in names(data$em_data)) {
    pct <- na_pct[col]
    status <- if (pct == 0) "🟢" else if (pct < 5) "🟡" else "🔴"
    cat(sprintf("    %s %s: %.1f%% missing\n", status, col, pct))
  }
} else {
  cat("  ❌ EM data not available\n")
}
cat("\n")

# 3. Questionnaire Data Validation
cat("📝 3. QUESTIONNAIRE DATA VALIDATION\n")
cat("----------------------------------\n")
q_data <- data$questionnaire_data
total_q <- length(q_data)
successful_q <- sum(sapply(q_data, function(x) x$success))

cat(sprintf("  📊 Total questionnaires: %d\n", total_q))
cat(sprintf("  ✅ Successfully loaded: %d\n", successful_q))
cat(sprintf("  ❌ Failed to load: %d\n", total_q - successful_q))

if (successful_q > 0) {
  cat("  📋 Successfully loaded questionnaires:\n")
  for (q_name in names(q_data)) {
    q <- q_data[[q_name]]
    if (q$success) {
      status <- "✅"
      cat(sprintf("    %s %s: %d rows, %d item columns, %d total columns\n",
                  status, q_name, nrow(q$data), length(q$columns), length(q$available_columns)))
    }
  }
}

if (total_q - successful_q > 0) {
  cat("  ❌ Failed questionnaires:\n")
  for (q_name in names(q_data)) {
    q <- q_data[[q_name]]
    if (!q$success) {
      cat(sprintf("    ❌ %s: %s\n", q_name, q$error))
    }
  }
}
cat("\n")

# 4. Integration Results Validation
cat("🔗 4. INTEGRATION RESULTS VALIDATION\n")
cat("-----------------------------------\n")
integration_results <- data$integration_results
total_integrations <- length(integration_results)

cat(sprintf("  🔄 Total integration attempts: %d\n", total_integrations))

if (total_integrations > 0) {
  successful_integrations <- sum(sapply(integration_results, function(x) x$success))
  cat(sprintf("  ✅ Successful integrations: %d\n", successful_integrations))
  cat(sprintf("  ❌ Failed integrations: %d\n", total_integrations - successful_integrations))

  cat("  📊 Integration results by questionnaire:\n")
  for (q_name in names(integration_results)) {
    result <- integration_results[[q_name]]
    if (result$success) {
      status <- "✅"
      cat(sprintf("    %s %s: %d/%d matched (%.1f%%)\n",
                  status, q_name, result$matched_rows, result$total_rows,
                  result$match_rate * 100))
    } else {
      status <- "❌"
      cat(sprintf("    %s %s: %s\n", status, q_name, result$error))
    }
  }

  # Summary statistics
  match_rates <- sapply(integration_results, function(x) x$match_rate)
  cat("  📈 Match rate summary:\n")
  cat(sprintf("    - Minimum: %.1f%%\n", min(match_rates) * 100))
  cat(sprintf("    - Maximum: %.1f%%\n", max(match_rates) * 100))
  cat(sprintf("    - Average: %.1f%%\n", mean(match_rates) * 100))
  cat(sprintf("    - Median: %.1f%%\n", median(match_rates) * 100))

} else {
  cat("  ❌ No integration results available\n")
}
cat("\n")

# 5. Data Summary Validation
cat("📈 5. DATA SUMMARY VALIDATION\n")
cat("----------------------------\n")
data_summary <- data$data_summary

if (length(data_summary) > 0) {
  cat("  📊 Data summary by questionnaire:\n")
  for (q_name in names(data_summary)) {
    summary <- data_summary[[q_name]]
    cat(sprintf("    📋 %s:\n", q_name))
    cat(sprintf("      - Rows: %d\n", summary$rows))
    cat(sprintf("      - Item columns: %d\n", summary$item_columns))
    cat(sprintf("      - Total columns: %d\n", summary$full_column_count))
  }
} else {
  cat("  ❌ No data summary available\n")
}
cat("\n")

# 6. Pipeline Statistics Validation
cat("📊 6. PIPELINE STATISTICS VALIDATION\n")
cat("-----------------------------------\n")
stats <- data$pipeline_stats

if (!is.null(stats)) {
  cat("  📈 Pipeline performance:\n")
  cat(sprintf("    - Total questionnaires: %d\n", stats$total_questionnaires))
  cat(sprintf("    - Successfully loaded: %d\n", stats$successful_loads))
  cat(sprintf("    - Successfully integrated: %d\n", stats$successful_integrations))

  if (!is.null(stats$match_rate_summary)) {
    cat("  📊 Match rate statistics:\n")
    cat(sprintf("    - Min match rate: %.1f%%\n", stats$match_rate_summary$min * 100))
    cat(sprintf("    - Max match rate: %.1f%%\n", stats$match_rate_summary$max * 100))
    cat(sprintf("    - Mean match rate: %.1f%%\n", stats$match_rate_summary$mean * 100))
  }
} else {
  cat("  ❌ Pipeline statistics not available\n")
}
cat("\n")

# 7. Data Quality Assessment
cat("🔍 7. DATA QUALITY ASSESSMENT\n")
cat("----------------------------\n")

# Check for common data quality issues
if (!is.null(data$em_data)) {
  cat("  📊 EM Data Quality:\n")

  # Check for duplicate IDs
  if ("ID_ESTUDIANTE" %in% names(data$em_data)) {
    duplicates <- sum(duplicated(data$em_data$ID_ESTUDIANTE))
    cat(sprintf("    - Duplicate ID_ESTUDIANTE: %d\n", duplicates))
  }

  # Check score ranges
  if ("medida500_L" %in% names(data$em_data)) {
    score_range <- range(data$em_data$medida500_L, na.rm = TRUE)
    cat(sprintf("    - medida500_L range: %.1f to %.1f\n", score_range[1], score_range[2]))
  }

  if ("medida500_M" %in% names(data$em_data)) {
    score_range <- range(data$em_data$medida500_M, na.rm = TRUE)
    cat(sprintf("    - medida500_M range: %.1f to %.1f\n", score_range[1], score_range[2]))
  }
}

# Check questionnaire data quality
cat("  📝 Questionnaire Data Quality:\n")
for (q_name in names(q_data)) {
  q <- q_data[[q_name]]
  if (q$success) {
    # Check for missing values in item data
    missing_pct <- mean(is.na(q$data)) * 100
    cat(sprintf("    - %s: %.1f%% missing values\n", q_name, missing_pct))

    # Check response ranges
    response_range <- range(as.matrix(q$data), na.rm = TRUE)
    cat(sprintf("      Response range: %.1f to %.1f\n", response_range[1], response_range[2]))
  }
}
cat("\n")

# 8. Recommendations
cat("💡 8. RECOMMENDATIONS\n")
cat("-------------------\n")

# Based on validation results, provide recommendations
cat("  📋 General recommendations:\n")

if (length(missing) > 0) {
  cat("    ⚠️  Fix missing data elements\n")
}

if (total_q - successful_q > 0) {
  cat("    ⚠️  Review failed questionnaire loading\n")
}

# Compute a safe value for successful integrations to avoid undefined variable usage
successful_integrations_safe <- if (total_integrations > 0) sum(sapply(integration_results, function(x) x$success)) else 0

if (total_integrations - successful_integrations_safe > 0) {
  cat("    ⚠️  Review failed integrations\n")
}

if (!is.null(data$em_data) && "ID_ESTUDIANTE" %in% names(data$em_data)) {
  duplicates <- sum(duplicated(data$em_data$ID_ESTUDIANTE))
  if (duplicates > 0) {
    cat("    ⚠️  Remove duplicate ID_ESTUDIANTE values\n")
  }
}

if (total_integrations > 0) {
  avg_match_rate <- mean(sapply(integration_results, function(x) x$match_rate))
  if (avg_match_rate < 0.8) {
    cat("    ⚠️  Low average match rate - review matching strategy\n")
  }
}

cat("    ✅ Data validation completed successfully\n")
cat("\n")

cat("🎉 VALIDATION REPORT COMPLETED\n")
cat("==============================\n")
