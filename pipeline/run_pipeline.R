#!/usr/bin/env Rscript
# ==============================================================================
# Master Script: Run Complete Analysis Pipeline
# ==============================================================================
# Executes the complete educational network analysis pipeline:
#   01. Load raw Excel datasets
#   02. Preprocess and merge data
#   03. Exploratory data analysis (EDA)
#   04. Principal component analysis (PCA)
#   05. PC algorithm (causal discovery)
#   06. Graphical Lasso (network estimation)
#
# Usage:
#   Rscript run_pipeline.R
#
# Author: Francisco Richter
# Date: 2025
# ==============================================================================

message_line <- function(...) cat(paste0(..., "\n"))
rule <- function(char = "=") paste0(rep(char, 80), collapse = "")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
pipeline_dir <- "pipeline"
scripts <- c(
  "01_load_data.R",
  "02_preprocess.R",
  "03_eda.R",
  "04_pca.R",
  "05_pc_algorithm.R",
  "06_graphical_lasso.R"
)

message_line(rule())
message_line("EDUCATIONAL NETWORK ANALYSIS - COMPLETE PIPELINE")
message_line(rule())
message_line("\nPipeline directory: ", pipeline_dir)
message_line("Start time: ", Sys.time())

# ------------------------------------------------------------------------------
# Check script availability
# ------------------------------------------------------------------------------
message_line("\nChecking pipeline scripts...")
missing_scripts <- c()

for (script in scripts) {
  script_path <- file.path(pipeline_dir, script)
  if (file.exists(script_path)) {
    message_line("  ✓ ", script)
  } else {
    message_line("  ✗ ", script, " (MISSING)")
    missing_scripts <- c(missing_scripts, script)
  }
}

if (length(missing_scripts) > 0) {
  message_line("\nMissing scripts: ", paste(missing_scripts, collapse = ", "))
  stop("Cannot run pipeline with missing scripts")
}

# ------------------------------------------------------------------------------
# Run pipeline steps
# ------------------------------------------------------------------------------
results <- list()
timings <- list()

for (i in seq_along(scripts)) {
  script <- scripts[i]
  script_path <- file.path(pipeline_dir, script)

  message_line("\n\n")
  message_line(rule("-"))
  message_line("STEP ", i, "/", length(scripts), ": ", script)
  message_line(rule("-"))
  message_line("Time: ", Sys.time())

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
    message_line("\n✓ ", script, " completed successfully")
    message_line("  Elapsed time: ", round(elapsed, 2), " seconds")
  } else {
    message_line("\n✗ ", script, " FAILED")
    message_line("  Error: ", result$error)
    message_line("  Elapsed time: ", round(elapsed, 2), " seconds")

    # Decide whether to continue or stop
    response <- readline(prompt = "Continue with remaining scripts? (y/n): ")
    if (tolower(response) != "y") {
      message_line("\nPipeline aborted by user.")
      break
    }
  }
}

# ------------------------------------------------------------------------------
# Summary report
# ------------------------------------------------------------------------------
message_line("\n\n")
message_line(rule())
message_line("PIPELINE COMPLETE")
message_line(rule())
message_line("\nEnd time: ", Sys.time())

# Summary table
message_line("\nExecution Summary:")
message_line("-" * 80)
message_line(sprintf("%-40s %-15s %-15s", "Script", "Status", "Time (sec)"))
message_line("-" * 80)

for (script in names(results)) {
  status <- results[[script]]$status
  time <- round(timings[[script]], 2)
  message_line(sprintf("%-40s %-15s %-15.2f", script, status, time))
}

message_line("-" * 80)
total_time <- sum(unlist(timings))
message_line(sprintf("%-40s %-15s %-15.2f", "TOTAL", "", total_time))
message_line("-" * 80)

# Check for failures
n_failed <- sum(sapply(results, function(x) x$status == "FAILED"))
n_success <- sum(sapply(results, function(x) x$status == "SUCCESS"))

if (n_failed > 0) {
  message_line("\n⚠ ", n_failed, " script(s) failed")
  message_line("\nFailed scripts:")
  for (script in names(results)) {
    if (results[[script]]$status == "FAILED") {
      message_line("  - ", script, ": ", results[[script]]$error)
    }
  }
} else {
  message_line("\n✓ All scripts completed successfully!")
}

message_line("\nTotal execution time: ", round(total_time / 60, 2), " minutes")

# ------------------------------------------------------------------------------
# Output summary
# ------------------------------------------------------------------------------
message_line("\n")
message_line(rule())
message_line("OUTPUT SUMMARY")
message_line(rule())

output_dir <- "outputs"

if (dir.exists(output_dir)) {
  message_line("\nGenerated outputs in: ", output_dir)

  # List subdirectories
  subdirs <- list.dirs(output_dir, full.names = FALSE, recursive = FALSE)

  for (subdir in subdirs) {
    subdir_path <- file.path(output_dir, subdir)

    # Count files
    files <- list.files(subdir_path, recursive = TRUE)
    n_csv <- sum(grepl("\\.csv$", files))
    n_rds <- sum(grepl("\\.rds$", files))
    n_png <- sum(grepl("\\.png$", files))

    message_line("\n", subdir, "/")
    if (n_csv > 0) message_line("  - CSV files: ", n_csv)
    if (n_rds > 0) message_line("  - RDS files: ", n_rds)
    if (n_png > 0) message_line("  - PNG figures: ", n_png)
  }
}

# ------------------------------------------------------------------------------
# Next steps
# ------------------------------------------------------------------------------
message_line("\n")
message_line(rule())
message_line("NEXT STEPS")
message_line(rule())
message_line("\n1. Review generated figures in outputs/*/figures/")
message_line("2. Examine edge lists and centrality measures in CSV files")
message_line("3. Compare PC algorithm and GLasso results for consistency")
message_line("4. Integrate key findings into paper")
message_line("5. Consider sensitivity analyses with different parameters")

message_line("\nFor detailed documentation, see: pipeline/README.md")
message_line("\n")

# Return results invisibly
invisible(list(results = results, timings = timings))
