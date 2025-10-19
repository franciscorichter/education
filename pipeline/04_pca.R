#!/usr/bin/env Rscript
# ==============================================================================
# Pipeline Step 04: Principal Component Analysis (PCA)
# ==============================================================================
# Performs PCA on the educational data to identify:
# - Main patterns of variation in performance and demographic variables
# - Component loadings to understand variable relationships
# - Component scores for further analysis
#
# Input: Preprocessed data from Step 02
# Output: PCA results, loadings, and visualizations
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(FactoMineR)  # PCA implementation
  library(factoextra)  # PCA visualization
  library(corrplot)
  library(gridExtra)
  library(rlang)
})

message_line <- function(...) cat(paste0(..., "\n"))
rule <- function(char = "=") paste0(rep(char, 80), collapse = "")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
output_dir <- "outputs"
pca_dir <- file.path(output_dir, "pca")
fig_dir <- file.path(pca_dir, "figures")
preprocessed_dir <- file.path(output_dir, "preprocessed")

dir.create(pca_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# PCA parameters
N_COMPONENTS <- 10  # Number of components to extract

message_line(rule())
message_line("PIPELINE 04 - PRINCIPAL COMPONENT ANALYSIS")
message_line(rule())

# ------------------------------------------------------------------------------
# Load preprocessed data
# ------------------------------------------------------------------------------
data_file <- file.path(preprocessed_dir, "enla_preprocessed.rds")
if (!file.exists(data_file)) {
  stop("Preprocessed data not found. Run 02_preprocess.R first.")
}

obj <- readRDS(data_file)
analysis_data <- obj$analysis

message_line("Loaded analysis data: ", nrow(analysis_data), " × ", ncol(analysis_data))

# ------------------------------------------------------------------------------
# Prepare data for PCA
# ------------------------------------------------------------------------------
message_line("\nPreparing data for PCA...")

# Select numeric variables for PCA
numeric_vars <- analysis_data %>%
  select_if(is.numeric) %>%
  select(-matches("^id|^peso|^weight|^factor", ignore.case = TRUE)) %>%
  names()

pca_data <- analysis_data %>%
  select(all_of(numeric_vars)) %>%
  na.omit()

message_line("  PCA dataset: ", nrow(pca_data), " observations × ", ncol(pca_data), " variables")

# Standardize data (important for PCA)
pca_data_scaled <- scale(pca_data)
attr(pca_data_scaled, "scaled:center") <- NULL
attr(pca_data_scaled, "scaled:scale") <- NULL

# ------------------------------------------------------------------------------
# Perform PCA
# ------------------------------------------------------------------------------
message_line("\nPerforming PCA...")

pca_result <- PCA(pca_data_scaled,
                  ncp = N_COMPONENTS,
                  scale.unit = TRUE,
                  graph = FALSE)

# Extract results
eigenvalues <- pca_result$eig
var_explained <- pca_result$eig[, 2]  # Percentage of variance explained
cum_var_explained <- pca_result$eig[, 3]  # Cumulative percentage

# Component loadings (variable contributions)
loadings <- pca_result$var$coord
contrib <- pca_result$var$contrib

# Component scores (individual observations)
scores <- pca_result$ind$coord

component_cols <- paste0("PC", seq_len(ncol(loadings)))
num_components <- length(component_cols)

colnames(loadings) <- component_cols
colnames(contrib) <- component_cols
colnames(scores) <- component_cols

if (num_components == 0) {
  stop("No PCA components were extracted. Check input data.")
}

message_line("  PCA completed:")
message_line("    - Components extracted: ", num_components)

max_var_summary <- min(3, length(var_explained))

if (max_var_summary > 0) {
  message_line("    - Total variance explained by first ", max_var_summary,
               " components: ",
               round(sum(var_explained[seq_len(max_var_summary)]), 1), "%")
} else {
  message_line("    - Variance explained metrics unavailable")
}

# ------------------------------------------------------------------------------
# Save PCA results
# ------------------------------------------------------------------------------
message_line("\nSaving PCA results...")

# Eigenvalues and variance explained
eigen_df <- data.frame(
  component = paste0("PC", 1:nrow(eigenvalues)),
  eigenvalue = eigenvalues[, 1],
  variance_pct = var_explained,
  cumulative_pct = cum_var_explained
)
write_csv(eigen_df, file.path(pca_dir, "pca_eigenvalues.csv"))

# Component loadings
loadings_df <- as.data.frame(loadings) %>%
  rownames_to_column("variable") %>%
  arrange(desc(abs(!!sym(component_cols[1]))))
write_csv(loadings_df, file.path(pca_dir, "pca_loadings.csv"))

# Component contributions
contrib_df <- as.data.frame(contrib) %>%
  rownames_to_column("variable")
write_csv(contrib_df, file.path(pca_dir, "pca_contributions.csv"))

# Component scores (save first 5 components)
score_cols <- head(component_cols, 5)

scores_df <- as.data.frame(scores) %>%
  select(all_of(score_cols)) %>%
  rownames_to_column("observation_id")
write_csv(scores_df, file.path(pca_dir, "pca_scores.csv"))

message_line("  ✓ Saved PCA results to CSV files")

# ------------------------------------------------------------------------------
# Generate visualizations
# ------------------------------------------------------------------------------
message_line("\nGenerating PCA visualizations...")

# 1. Scree plot
p1 <- fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50)) +
  labs(title = "Scree Plot: Variance Explained by Components") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(fig_dir, "pca_scree_plot.png"), p1, width = 8, height = 6, dpi = 150)
message_line("  ✓ Saved: pca_scree_plot.png")

# 2. Variable loadings (first two components)
p2 <- fviz_pca_var(pca_result, col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE) +
  labs(title = "Variable Loadings: PC1 vs PC2") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(fig_dir, "pca_variable_loadings.png"), p2, width = 10, height = 8, dpi = 150)
message_line("  ✓ Saved: pca_variable_loadings.png")

# 3. Individuals plot (first two components)
p3 <- fviz_pca_ind(pca_result, col.ind = "cos2",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE) +
  labs(title = "Individual Observations: PC1 vs PC2") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(fig_dir, "pca_individuals.png"), p3, width = 10, height = 8, dpi = 150)
message_line("  ✓ Saved: pca_individuals.png")

# 4. Correlation circle
p4 <- fviz_pca_var(pca_result, alpha.var = "contrib",
                   col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(title = "Correlation Circle: Variable Contributions") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(fig_dir, "pca_correlation_circle.png"), p4, width = 8, height = 8, dpi = 150)
message_line("  ✓ Saved: pca_correlation_circle.png")

# 5. Component contributions heatmap
heatmap_cols <- intersect(paste0("PC", 1:5), colnames(contrib_df))

if (length(heatmap_cols) > 0) {
  contrib_subset <- contrib_df %>%
    select(all_of(heatmap_cols)) %>%
    as.matrix()
  rownames(contrib_subset) <- contrib_df$variable

  png(file.path(fig_dir, "pca_contributions_heatmap.png"),
      width = 12, height = 8, units = "in", res = 150)
  corrplot(contrib_subset,
           method = "color",
           is.corr = FALSE,
           tl.col = "black",
           tl.srt = 45,
           tl.cex = 0.6,
           title = "Component Contributions Heatmap (Top 20 Variables)",
           mar = c(0, 0, 2, 0))
  dev.off()
  message_line("  ✓ Saved: pca_contributions_heatmap.png")
} else {
  message_line("  ⚠ Skipped: Not enough components for contributions heatmap")
}

# ------------------------------------------------------------------------------
# Interpret components
# ------------------------------------------------------------------------------
message_line("\nInterpreting principal components...")

# Function to interpret component
interpret_component <- function(comp_num, loadings_df, n_vars = 10) {
  comp_col <- paste0("PC", comp_num)
  if (!comp_col %in% colnames(loadings_df) || comp_num > length(var_explained)) {
    return(invisible(NULL))
  }
  top_vars <- loadings_df %>%
    arrange(desc(abs(!!sym(comp_col)))) %>%
    head(n_vars)

  message_line("  Component ", comp_num, " (", round(var_explained[comp_num], 1), "% variance):")
  for (i in 1:nrow(top_vars)) {
    var <- top_vars$variable[i]
    loading <- top_vars[[comp_col]][i]
    message_line("    ", var, ": ", round(loading, 3))
  }
}

# Interpret first 3 components
for (i in seq_len(min(3, length(component_cols)))) {
  interpret_component(i, loadings_df)
}

# ------------------------------------------------------------------------------
# Save interpretation
# ------------------------------------------------------------------------------
interpretation <- list()
max_interp <- min(5, num_components, length(var_explained))

for (i in seq_len(max_interp)) {
  comp_col <- paste0("PC", i)
  top_vars <- loadings_df %>%
    arrange(desc(abs(!!sym(comp_col)))) %>%
    head(10) %>%
    mutate(component = i,
           variance_explained = var_explained[i])

  interpretation[[paste0("PC", i)]] <- top_vars
}

# Save interpretation
saveRDS(interpretation, file.path(pca_dir, "pca_interpretation.rds"))
message_line("  ✓ Saved component interpretation")

# ------------------------------------------------------------------------------
# Summary report
# ------------------------------------------------------------------------------
message_line("")
message_line(rule())
message_line("PCA COMPLETE")
message_line(rule())

message_line("\nPCA Summary:")
message_line("  - Dataset: ", nrow(pca_data), " observations × ", ncol(pca_data), " variables")
message_line("  - Components extracted: ", num_components)

message_line("\nVariance explained:")
max_var_report <- min(5, length(var_explained))

for (i in seq_len(max_var_report)) {
  message_line("    PC", i, ": ", round(var_explained[i], 1), "%")
}
if (max_var_report > 0) {
  message_line("    Cumulative (PC1-", max_var_report, "): ",
               round(sum(var_explained[seq_len(max_var_report)]), 1), "%")
} else {
  message_line("    Cumulative variance unavailable")
}

message_line("\nGenerated outputs:")
message_line("  - Eigenvalues: ", file.path(pca_dir, "pca_eigenvalues.csv"))
message_line("  - Loadings: ", file.path(pca_dir, "pca_loadings.csv"))
message_line("  - Contributions: ", file.path(pca_dir, "pca_contributions.csv"))
message_line("  - Scores: ", file.path(pca_dir, "pca_scores.csv"))
message_line("  - Interpretation: ", file.path(pca_dir, "pca_interpretation.rds"))
message_line("  - Figures: ", fig_dir)

figs <- list.files(fig_dir, pattern = "\\.png$", full.names = FALSE)
message_line("  - Generated ", length(figs), " figures")

message_line("\nReady for next pipeline steps:")
message_line("  1. Run 05_pc_algorithm.R for causal discovery")
message_line("  2. Run 06_graphical_lasso.R for network estimation")

message_line("")
message_line("✓ PCA pipeline complete!")
