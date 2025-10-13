#!/usr/bin/env Rscript
# ==============================================================================
# Master Script: Run Complete Analysis Pipeline
# ==============================================================================
# This script runs all analysis steps in sequence:
#   1. Data preprocessing
#   2. Exploratory data analysis
#   3. PC algorithm (causal discovery)
#   4. Graphical Lasso (network estimation)
#
# Usage:
#   Rscript run_all_analyses.R
#
# Or from R console:
#   source("analysis/run_all_analyses.R")
#
# Author: Education Analysis Team
# Date: 2024
# ==============================================================================

# Set working directory to project root
if (require("here", quietly = TRUE)) {
  setwd(here::here())
} else {
  # Fallback: assume script is in analysis/ subdirectory
  script_dir <- dirname(sys.frame(1)$ofile)
  if (basename(script_dir) == "analysis") {
    setwd(dirname(script_dir))
  }
}

message("="*80)
message("EDUCATIONAL NETWORK ANALYSIS - COMPLETE PIPELINE")
message("="*80)
message("\nProject directory: ", getwd())
message("Start time: ", Sys.time())

# ==============================================================================
# Configuration
# ==============================================================================

ANALYSIS_DIR <- file.path(getwd(), "analysis")

# Scripts to run
SCRIPTS <- c(
  "00_data_preprocessing.R",
  "01_eda_analysis.R",
  "02_pc_algorithm.R",
  "03_graphical_lasso.R"
)

# Check that all scripts exist
for (script in SCRIPTS) {
  script_path <- file.path(ANALYSIS_DIR, script)
  if (!file.exists(script_path)) {
    stop("Script not found: ", script_path)
  }
}

# ==============================================================================
# Run Analysis Pipeline
# ==============================================================================

results <- list()
timings <- list()

for (i in seq_along(SCRIPTS)) {
  script <- SCRIPTS[i]
  script_path <- file.path(ANALYSIS_DIR, script)
  
  message("\n\n")
  message("="*80)
  message("STEP ", i, "/", length(SCRIPTS), ": ", script)
  message("="*80)
  message("Time: ", Sys.time())
  
  # Run script and capture timing
  start_time <- Sys.time()
  
  result <- tryCatch({
    source(script_path, local = new.env())
    list(status = "SUCCESS", error = NULL)
  }, error = function(e) {
    list(status = "FAILED", error = e$message)
  }, warning = function(w) {
    list(status = "WARNING", error = w$message)
  })
  
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Store results
  results[[script]] <- result
  timings[[script]] <- elapsed
  
  # Report status
  if (result$status == "SUCCESS") {
    message("\n✓ ", script, " completed successfully")
    message("  Elapsed time: ", round(elapsed, 2), " seconds")
  } else {
    message("\n✗ ", script, " FAILED")
    message("  Error: ", result$error)
    message("  Elapsed time: ", round(elapsed, 2), " seconds")
    
    # Decide whether to continue or stop
    response <- readline(prompt = "Continue with remaining scripts? (y/n): ")
    if (tolower(response) != "y") {
      message("\nPipeline aborted by user.")
      break
    }
  }
}

# ==============================================================================
# Summary Report
# ==============================================================================

message("\n\n")
message("="*80)
message("PIPELINE COMPLETE")
message("="*80)
message("\nEnd time: ", Sys.time())

# Summary table
message("\nExecution Summary:")
message("-"*80)
message(sprintf("%-40s %-15s %-15s", "Script", "Status", "Time (sec)"))
message("-"*80)

for (script in names(results)) {
  status <- results[[script]]$status
  time <- round(timings[[script]], 2)
  message(sprintf("%-40s %-15s %-15.2f", script, status, time))
}

message("-"*80)
total_time <- sum(unlist(timings))
message(sprintf("%-40s %-15s %-15.2f", "TOTAL", "", total_time))
message("-"*80)

# Check for failures
n_failed <- sum(sapply(results, function(x) x$status == "FAILED"))
n_success <- sum(sapply(results, function(x) x$status == "SUCCESS"))

if (n_failed > 0) {
  message("\n⚠ ", n_failed, " script(s) failed")
  message("\nFailed scripts:")
  for (script in names(results)) {
    if (results[[script]]$status == "FAILED") {
      message("  - ", script, ": ", results[[script]]$error)
    }
  }
} else {
  message("\n✓ All scripts completed successfully!")
}

message("\nTotal execution time: ", round(total_time / 60, 2), " minutes")

# ==============================================================================
# Output Summary
# ==============================================================================

message("\n")
message("="*80)
message("OUTPUT SUMMARY")
message("="*80)

OUTPUT_DIR <- file.path(getwd(), "outputs")

if (dir.exists(OUTPUT_DIR)) {
  message("\nGenerated outputs in: ", OUTPUT_DIR)
  
  # List subdirectories
  subdirs <- list.dirs(OUTPUT_DIR, full.names = FALSE, recursive = FALSE)
  
  for (subdir in subdirs) {
    subdir_path <- file.path(OUTPUT_DIR, subdir)
    
    # Count files
    files <- list.files(subdir_path, recursive = TRUE)
    n_csv <- sum(grepl("\\.csv$", files))
    n_rds <- sum(grepl("\\.rds$", files))
    n_png <- sum(grepl("\\.png$", files))
    
    message("\n", subdir, "/")
    if (n_csv > 0) message("  - CSV files: ", n_csv)
    if (n_rds > 0) message("  - RDS files: ", n_rds)
    if (n_png > 0) message("  - PNG figures: ", n_png)
  }
}

# ==============================================================================
# Next Steps
# ==============================================================================

message("\n")
message("="*80)
message("NEXT STEPS")
message("="*80)
message("\n1. Review generated figures in outputs/*/figures/")
message("2. Examine edge lists and centrality measures in CSV files")
message("3. Compare PC algorithm and GLasso results for consistency")
message("4. Integrate key findings into paper")
message("5. Consider sensitivity analyses with different parameters")

message("\n")
message("For detailed documentation, see: analysis/README.md")
message("\n")

# Return results invisibly
invisible(list(results = results, timings = timings))
