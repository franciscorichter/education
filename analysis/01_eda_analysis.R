#!/usr/bin/env Rscript
# ==============================================================================
# Exploratory Data Analysis (EDA) for Educational Network Study
# ==============================================================================
# This script performs comprehensive EDA including:
# - Distribution analysis by demographic groups
# - Correlation analysis
# - Performance gap analysis
# - Visualization generation for paper
#
# Author: Education Analysis Team
# Date: 2024
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(tibble)
  library(corrplot)
  library(GGally)
  library(gridExtra)
})

# ==============================================================================
# Configuration
# ==============================================================================

ROOT_DIR <- here::here()
OUTPUT_DIR <- file.path(ROOT_DIR, "outputs")
PREPROC_DIR <- file.path(OUTPUT_DIR, "preprocessed")
EDA_DIR <- file.path(OUTPUT_DIR, "eda")
FIG_DIR <- file.path(EDA_DIR, "figures")

# Create directories
dir.create(EDA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

message("="*80)
message("Exploratory Data Analysis")
message("="*80)

# ==============================================================================
# Load Preprocessed Data
# ==============================================================================

message("\n[1/6] Loading preprocessed data...")
data_file <- file.path(PREPROC_DIR, "enla_clean.rds")
if (!file.exists(data_file)) {
  stop("Preprocessed data not found. Run 00_data_preprocessing.R first.")
}

obj <- readRDS(data_file)
em <- obj$data
em_analysis <- obj$analysis_data

message("  - Loaded ", nrow(em), " observations")
message("  - Analysis subset: ", nrow(em_analysis), " observations")

# ==============================================================================
# Descriptive Statistics
# ==============================================================================

message("\n[2/6] Computing descriptive statistics...")

# Overall statistics
desc_stats <- em %>%
  summarise(
    n = n(),
    mean_math = mean(medida500_M, na.rm = TRUE),
    sd_math = sd(medida500_M, na.rm = TRUE),
    median_math = median(medida500_M, na.rm = TRUE),
    mean_lang = mean(medida500_L, na.rm = TRUE),
    sd_lang = sd(medida500_L, na.rm = TRUE),
    median_lang = median(medida500_L, na.rm = TRUE),
    cor_math_lang = cor(medida500_M, medida500_L, use = "complete.obs")
  )

message("  Overall Statistics:")
message("    N: ", desc_stats$n)
message("    Math - Mean: ", round(desc_stats$mean_math, 2), 
        ", SD: ", round(desc_stats$sd_math, 2))
message("    Language - Mean: ", round(desc_stats$mean_lang, 2), 
        ", SD: ", round(desc_stats$sd_lang, 2))
message("    Correlation: ", round(desc_stats$cor_math_lang, 3))

# Statistics by group
compute_group_stats <- function(data, group_var) {
  if (!(group_var %in% names(data))) return(NULL)
  
  data %>%
    filter(!is.na(!!sym(group_var))) %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      n = n(),
      mean_math = mean(medida500_M, na.rm = TRUE),
      sd_math = sd(medida500_M, na.rm = TRUE),
      mean_lang = mean(medida500_L, na.rm = TRUE),
      sd_lang = sd(medida500_L, na.rm = TRUE),
      .groups = "drop"
    )
}

stats_by_gender <- compute_group_stats(em, "gender")
stats_by_area <- compute_group_stats(em, "area")
stats_by_school <- compute_group_stats(em, "school_type")

# Save statistics
write_csv(desc_stats, file.path(EDA_DIR, "descriptive_stats_overall.csv"))
if (!is.null(stats_by_gender)) write_csv(stats_by_gender, file.path(EDA_DIR, "descriptive_stats_gender.csv"))
if (!is.null(stats_by_area)) write_csv(stats_by_area, file.path(EDA_DIR, "descriptive_stats_area.csv"))
if (!is.null(stats_by_school)) write_csv(stats_by_school, file.path(EDA_DIR, "descriptive_stats_school.csv"))

# ==============================================================================
# Violin Plots by Demographic Groups
# ==============================================================================

message("\n[3/6] Generating violin plots...")

plot_violin <- function(data, group_var, title_suffix) {
  if (!(group_var %in% names(data))) return(NULL)
  
  df <- data %>%
    filter(!is.na(!!sym(group_var))) %>%
    select(group = !!sym(group_var), medida500_M, medida500_L) %>%
    filter(!is.na(medida500_M) & !is.na(medida500_L)) %>%
    pivot_longer(cols = c(medida500_M, medida500_L), 
                 names_to = "measure", values_to = "value")
  
  p <- ggplot(df, aes(x = group, y = value, fill = group)) +
    geom_violin(trim = FALSE, alpha = 0.6, color = "grey60") +
    geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.9, fill = "white") +
    facet_wrap(~ measure, scales = "free_y") +
    labs(title = paste0("Distribution by group — ", title_suffix), 
         x = NULL, y = NULL) +
    theme_bw(base_size = 12) + 
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  return(p)
}

# Generate and save violin plots
if ("gender" %in% names(em)) {
  p <- plot_violin(em, "gender", "gender (boy/girl)")
  ggsave(file.path(FIG_DIR, "violin_gender.png"), p, width = 8, height = 6, dpi = 150)
  message("  - Saved violin_gender.png")
}

if ("area" %in% names(em)) {
  p <- plot_violin(em, "area", "area (rural/urban)")
  ggsave(file.path(FIG_DIR, "violin_area.png"), p, width = 8, height = 6, dpi = 150)
  message("  - Saved violin_area.png")
}

if ("school_type" %in% names(em)) {
  p <- plot_violin(em, "school_type", "school (public/private)")
  ggsave(file.path(FIG_DIR, "violin_school.png"), p, width = 8, height = 6, dpi = 150)
  message("  - Saved violin_school.png")
}

if ("language" %in% names(em)) {
  p <- plot_violin(em, "language", "language (spanish/other)")
  ggsave(file.path(FIG_DIR, "violin_language.png"), p, width = 8, height = 6, dpi = 150)
  message("  - Saved violin_language.png")
}

# ==============================================================================
# Scatter Plots with Density Contours
# ==============================================================================

message("\n[4/6] Generating scatter plots...")

plot_scatter <- function(data, group_var, title_suffix) {
  if (!(group_var %in% names(data))) return(NULL)
  
  df <- data %>%
    filter(!is.na(!!sym(group_var))) %>%
    select(group = !!sym(group_var), medida500_M, medida500_L) %>%
    filter(!is.na(medida500_M) & !is.na(medida500_L))
  
  # Sample if too large
  set.seed(123)
  if (nrow(df) > 60000) {
    df <- df[sample.int(nrow(df), 60000), ]
  }
  
  p <- ggplot(df, aes(x = medida500_M, y = medida500_L, color = group)) +
    geom_point(alpha = 0.12, size = 0.5) +
    stat_density_2d(linewidth = 0.8) +
    labs(title = paste0("M vs L — colored by ", title_suffix),
         x = "Math (medida500_M)", 
         y = "Language (medida500_L)", 
         color = "Group") +
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    scale_color_viridis_d(option = "C", end = 0.85)
  
  return(p)
}

# Generate and save scatter plots
if ("gender" %in% names(em)) {
  p <- plot_scatter(em, "gender", "gender (boy/girl)")
  ggsave(file.path(FIG_DIR, "scatter_gender.png"), p, width = 8, height = 6, dpi = 150)
  message("  - Saved scatter_gender.png")
}

if ("area" %in% names(em)) {
  p <- plot_scatter(em, "area", "area (rural/urban)")
  ggsave(file.path(FIG_DIR, "scatter_area.png"), p, width = 8, height = 6, dpi = 150)
  message("  - Saved scatter_area.png")
}

if ("school_type" %in% names(em)) {
  p <- plot_scatter(em, "school_type", "school (public/private)")
  ggsave(file.path(FIG_DIR, "scatter_school.png"), p, width = 8, height = 6, dpi = 150)
  message("  - Saved scatter_school.png")
}

if ("language" %in% names(em)) {
  p <- plot_scatter(em, "language", "language (spanish/other)")
  ggsave(file.path(FIG_DIR, "scatter_language.png"), p, width = 8, height = 6, dpi = 150)
  message("  - Saved scatter_language.png")
}

# ==============================================================================
# Correlation Analysis
# ==============================================================================

message("\n[5/6] Computing correlation matrix...")

# Select numeric variables for correlation
numeric_data <- em_analysis %>%
  select_if(is.numeric) %>%
  select(-matches("^id|^ID|^peso|^PESO|^weight|^factor|^estrato", ignore.case = TRUE))

# Compute correlation matrix (use complete observations)
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Save correlation matrix
write_csv(as.data.frame(cor_matrix) %>% rownames_to_column("variable"), 
          file.path(EDA_DIR, "correlation_matrix.csv"))

# Plot correlation matrix (top variables by correlation with performance)
top_vars <- cor_matrix[, c("medida500_M", "medida500_L")] %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  mutate(abs_cor = abs(medida500_M) + abs(medida500_L)) %>%
  arrange(desc(abs_cor)) %>%
  head(20) %>%
  pull(variable)

cor_subset <- cor_matrix[top_vars, top_vars]

png(file.path(FIG_DIR, "correlation_heatmap.png"), width = 10, height = 10, 
    units = "in", res = 150)
corrplot(cor_subset, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         title = "Correlation Matrix (Top 20 Variables)",
         mar = c(0, 0, 2, 0))
dev.off()
message("  - Saved correlation_heatmap.png")

# ==============================================================================
# Performance Gap Analysis
# ==============================================================================

message("\n[6/6] Analyzing performance gaps...")

compute_gaps <- function(data, group_var) {
  if (!(group_var %in% names(data))) return(NULL)
  
  stats <- data %>%
    filter(!is.na(!!sym(group_var))) %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      mean_math = mean(medida500_M, na.rm = TRUE),
      mean_lang = mean(medida500_L, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(stats) == 2) {
    gap_math <- diff(stats$mean_math)
    gap_lang <- diff(stats$mean_lang)
    
    return(tibble(
      group_var = group_var,
      gap_math = gap_math,
      gap_lang = gap_lang,
      group1 = stats[[group_var]][1],
      group2 = stats[[group_var]][2]
    ))
  }
  return(NULL)
}

gaps <- bind_rows(
  compute_gaps(em, "gender"),
  compute_gaps(em, "area"),
  compute_gaps(em, "school_type")
)

if (nrow(gaps) > 0) {
  write_csv(gaps, file.path(EDA_DIR, "performance_gaps.csv"))
  message("\nPerformance Gaps:")
  print(gaps)
}

# ==============================================================================
# Summary Report
# ==============================================================================

message("\n" %+% "="*80)
message("EDA Complete!")
message("="*80)
message("\nGenerated outputs:")
message("  - Descriptive statistics: ", EDA_DIR)
message("  - Figures: ", FIG_DIR)
message("\nKey findings:")
message("  - Math-Language correlation: ", round(desc_stats$cor_math_lang, 3))
if (!is.null(stats_by_gender)) {
  gender_gap_math <- diff(stats_by_gender$mean_math)
  message("  - Gender gap (Math): ", round(abs(gender_gap_math), 2), " points")
}
if (!is.null(stats_by_area)) {
  area_gap_math <- diff(stats_by_area$mean_math)
  message("  - Urban-Rural gap (Math): ", round(abs(area_gap_math), 2), " points")
}

message("\n✓ Ready for next analysis step!")
message("  Run 02_pc_algorithm.R for causal discovery")
