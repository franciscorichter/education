#!/usr/bin/env Rscript
# ==============================================================================
# Pipeline Step 03: Exploratory Data Analysis (EDA) - Minimal
# ==============================================================================
# Purpose: Generate only the scatterplot with 2D density contours for
# medida500_L vs medida500_M (and by sex if available), as static PNGs.
#
# Input: Preprocessed data from Step 02
# Output: 2 figures under outputs/eda/figures/
#   - scatter_contour_L_vs_M.png
#   - scatter_contour_L_vs_M_by_sex.png (if sexo available)
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tibble)
})

message_line <- function(...) cat(paste0(..., "\n"))
rule <- function(char = "=") paste0(rep(char, 80), collapse = "")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
output_dir <- "outputs"
eda_dir <- file.path(output_dir, "eda")
fig_dir <- file.path(eda_dir, "figures")
preprocessed_dir <- file.path(output_dir, "preprocessed")

dir.create(eda_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

message_line(rule())
message_line("PIPELINE 03 - EDA (Minimal: Scatter + 2D Contours)")
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

score_L <- "medida500_l"
score_M <- "medida500_m"
message_line("\nSample size: ", nrow(analysis_data))

# ------------------------------------------------------------------------------
# Scatter with 2D density contours (Performance L vs M)
# ------------------------------------------------------------------------------
message_line("\nGenerating scatterplots with 2D density contours (Socioemotional vs Academic)...")

soc_var <- "socioemotional_learning"
acad_l  <- "academic_lang"
acad_m  <- "academic_math"
wsoc    <- "weight_socio"
wl      <- "weight_lang"
wm      <- "weight_math"

make_weight <- function(df, w1, w2) {
  if (w1 %in% names(df) && w2 %in% names(df)) {
    w <- suppressWarnings(as.numeric(df[[w1]])) * suppressWarnings(as.numeric(df[[w2]]))
    return(w)
  } else if (w1 %in% names(df)) {
    return(suppressWarnings(as.numeric(df[[w1]])))
  } else if (w2 %in% names(df)) {
    return(suppressWarnings(as.numeric(df[[w2]])))
  }
  return(NULL)
}

plot_pair <- function(df, xvar, yvar, weight_cols = NULL, title, file_stub) {
  if (!all(c(xvar, yvar) %in% names(df))) return(FALSE)
  tmp <- df %>%
    select(any_of(c(xvar, yvar, "sexo", "genero", "gender", weight_cols))) %>%
    drop_na(all_of(c(xvar, yvar)))

  # Derive a single sexo column if any sex variable is present
  sex_candidates <- intersect(c("sexo", "genero", "gender"), names(tmp))
  if (length(sex_candidates) > 0) {
    tmp$sexo <- tmp[[sex_candidates[1]]]
  }

  w <- NULL
  if (!is.null(weight_cols) && length(weight_cols) > 0) {
    w <- make_weight(tmp, weight_cols[1], ifelse(length(weight_cols) > 1, weight_cols[2], NA_character_))
  }

  aes_base <- aes(x = .data[[xvar]], y = .data[[yvar]])
  if (!is.null(w)) aes_base <- modifyList(aes_base, aes(weight = w))

  contour_palette <- c("#F2F0F7", "#CBC9E2", "#9E9AC8", "#756BB1", "#54278F")

  p <- ggplot(tmp, aes_base) +
    geom_point(alpha = 0.005, size = 0.45, color = "#2f2f2f") +
    stat_density_2d(aes(color = after_stat(level)), bins = 10, linewidth = 1) +
    scale_color_gradientn(colors = contour_palette, guide = guide_colorbar(title = "Density")) +
    labs(title = title, x = xvar, y = yvar) +
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(file.path(fig_dir, paste0(file_stub, ".png")), p, width = 8, height = 6, dpi = 150)
  message_line("  ✓ Saved: ", paste0(file_stub, ".png"))

  # By sex
  if ("sexo" %in% names(tmp) && any(!is.na(tmp$sexo))) {
    sexo_palette <- c("#264653", "#E76F51", "#2A9D8F", "#F4A261", "#A47148")

    p_sex <- ggplot(tmp, modifyList(aes_base, aes(color = sexo))) +
      geom_point(alpha = 0.01, size = 0.45) +
      stat_density_2d(aes(linetype = sexo), linewidth = 0.8, bins = 9, show.legend = FALSE) +
      scale_color_manual(values = sexo_palette, na.translate = FALSE) +
      labs(title = paste0(title, " by Sex"), x = xvar, y = yvar, color = "Sex") +
      theme_bw(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggsave(file.path(fig_dir, paste0(file_stub, "_by_sex.png")), p_sex, width = 9, height = 6, dpi = 150)
    message_line("  ✓ Saved: ", paste0(file_stub, "_by_sex.png"))
  }
  return(TRUE)
}

## Base plots (overall)
ok1 <- plot_pair(analysis_data, soc_var, acad_l, c(wsoc, wl),
                 title = "Socioemotional vs Academic (Language)",
                 file_stub = "scatter_contour_SE_vs_AcademicL")
ok2 <- plot_pair(analysis_data, soc_var, acad_m, c(wsoc, wm),
                 title = "Socioemotional vs Academic (Math)",
                 file_stub = "scatter_contour_SE_vs_AcademicM")
if (!ok1 && !ok2) message_line("  ⚠ Skipping: required variables not found (socioemotional_learning, academic_* )")

## Helper to make categorized plots
plot_by_category <- function(df, xvar, yvar, cat_candidates, weight_cols, pretty_name, stub_suffix) {
  present <- intersect(cat_candidates, names(df))
  if (length(present) == 0) {
    message_line("  ⚠ Skipping ", pretty_name, ": no matching columns (", paste(cat_candidates, collapse=", "), ") found")
    return(FALSE)
  }
  cat_col <- present[1]
  tmp <- df %>%
    select(any_of(c(xvar, yvar, cat_col, weight_cols))) %>%
    drop_na(all_of(c(xvar, yvar))) %>%
    mutate(category = .data[[cat_col]])

  if (pretty_name == "Lengua Materna") {
    castellano_vals <- c(
      "castellano", "castellano y multimarca", "castellano y multimarca (bilingue)",
      "multimarca", "castellano-multimarca", "castellano / multimarca"
    )

    normalized <- tolower(trimws(as.character(tmp$category)))
    grouped <- ifelse(
      normalized %in% castellano_vals,
      "Castellano / Multimarca",
      ifelse(!is.na(normalized) & normalized != "", "Otros", NA_character_)
    )
    tmp$category <- grouped
    tmp <- tmp %>% filter(!is.na(category))
  }

  w <- NULL
  if (!is.null(weight_cols) && length(weight_cols) > 0) {
    w <- make_weight(tmp, weight_cols[1], ifelse(length(weight_cols) > 1, weight_cols[2], NA_character_))
  }

  aes_base <- aes(x = .data[[xvar]], y = .data[[yvar]], color = category)
  if (!is.null(w)) aes_base <- modifyList(aes_base, aes(weight = w))

  # Color palette (special handling for Lengua Materna)
  if (pretty_name == "Lengua Materna") {
    palette_manual <- c(
      "Castellano / Multimarca" = "#1F77B4",
      "Otros" = "#FF7F0E"
    )
  } else {
    palette_manual <- c(
      "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
      "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"
    )
  }

  p <- ggplot(tmp, aes_base) +
    geom_point(alpha = 0.05, size = 0.7) +
    stat_density_2d(aes(color = category), bins = 9, linewidth = 0.7) +
    scale_color_manual(values = palette_manual) +
    labs(title = paste0("SE vs Academic (", pretty_name, ")"), x = xvar, y = yvar, color = pretty_name) +
    theme_bw(base_size = 12) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  fname <- paste0("scatter_contour_SE_vs_", stub_suffix, "_by_", gsub("[^a-z0-9]+", "_", tolower(pretty_name)), ".png")
  ggsave(file.path(fig_dir, fname), p, width = 9, height = 6, dpi = 150)
  message_line("  ✓ Saved: ", fname)
  TRUE
}

message_line("\nGenerating categorized plots (gender, area, gestion2, lengua materna)...")

# Gender: prefer standardized then fallbacks
plot_by_category(analysis_data, soc_var, acad_l, c("sexo_std","sexo","genero","gender"), c(wsoc, wl), "Gender", "AcademicL")
plot_by_category(analysis_data, soc_var, acad_m, c("sexo_std","sexo","genero","gender"), c(wsoc, wm), "Gender", "AcademicM")

# Area: prefer standardized then fallbacks
plot_by_category(analysis_data, soc_var, acad_l, c("area_std","area","zona"), c(wsoc, wl), "Area", "AcademicL")
plot_by_category(analysis_data, soc_var, acad_m, c("area_std","area","zona"), c(wsoc, wm), "Area", "AcademicM")

# Gestion2: prefer standardized then fallbacks
plot_by_category(analysis_data, soc_var, acad_l, c("gestion2_std","gestion2","gestion","tipo_gestion","sector"), c(wsoc, wl), "Gestion2", "AcademicL")
plot_by_category(analysis_data, soc_var, acad_m, c("gestion2_std","gestion2","gestion","tipo_gestion","sector"), c(wsoc, wm), "Gestion2", "AcademicM")

# Lengua materna: prefer standardized then fallbacks
plot_by_category(analysis_data, soc_var, acad_l, c("lengua_materna_std","lengua_materna","lengua","lengua_materna_est","lengua_materna_estudiante"), c(wsoc, wl), "Lengua Materna", "AcademicL")
plot_by_category(analysis_data, soc_var, acad_m, c("lengua_materna_std","lengua_materna","lengua","lengua_materna_est","lengua_materna_estudiante"), c(wsoc, wm), "Lengua Materna", "AcademicM")

# ------------------------------------------------------------------------------
# Summary report
# ------------------------------------------------------------------------------
message_line("")
message_line(rule())
message_line("EDA (Minimal) COMPLETE")
message_line(rule())

message_line("\nGenerated outputs:")
message_line("  - Figures: ", fig_dir)

# Count generated figures
figs <- list.files(fig_dir, pattern = "\\.png$", full.names = FALSE)
message_line("  - Generated ", length(figs), " figures")

message_line("\nKey findings:")
message_line("  - Sample size: ", nrow(analysis_data), " students")

message_line("\nReady for next pipeline steps (optional): 04_pca.R, 05_pc_algorithm.R, 06_graphical_lasso.R")

message_line("")
message_line("✓ EDA (Minimal) pipeline complete!")
