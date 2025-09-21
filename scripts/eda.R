#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(stringr)
  library(tibble)
})

# --------- Helpers ---------
message2 <- function(...) cat(paste0(..., "\n"))

# Parse simple command line args of the form --key=value
parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  out <- list()
  for (a in args) {
    if (grepl("^--[a-zA-Z0-9_]+=", a)) {
      kv <- sub("^--", "", a)
      k <- sub("=.*$", "", kv)
      v <- sub("^[^=]*=", "", kv)
      out[[k]] <- v
    }
  }
  out
}

args <- parse_args()
# Assume we're executed from the project root; otherwise, users can pass --rds and --outdir
root <- normalizePath(getwd(), mustWork = TRUE)
app_dir <- normalizePath(file.path(root, "app"), mustWork = FALSE)
# Default RDS path
rds_path <- if (!is.null(args$rds)) args$rds else file.path(root, "data", "enla_processed_data.rds")
# Output directory
out_dir <- if (!is.null(args$outdir)) args$outdir else file.path(root, "outputs", "eda")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
fig_dir <- file.path(out_dir, "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Utility: null coalesce
`%||%` <- function(a,b) if (!is.null(a)) a else b

# --------- Load RDS ---------
if (!file.exists(rds_path)) {
  stop("RDS not found at ", rds_path)
}
message2("Reading RDS: ", rds_path)
obj <- readRDS(rds_path)
if (is.null(obj$em_data)) stop("RDS does not contain em_data")
em <- obj$em_data

# Coerce measures to numeric (robust)
if ("medida500_L" %in% names(em)) em$medida500_L <- suppressWarnings(as.numeric(em$medida500_L))
if ("medida500_M" %in% names(em)) em$medida500_M <- suppressWarnings(as.numeric(em$medida500_M))

# --------- Detect group columns ---------
nm <- names(em)

detect_first <- function(cands) {
  cand <- cands[cands %in% nm]
  if (length(cand) > 0) cand[1] else NA_character_
}

gender_col   <- detect_first(c("SEXO","sexo","genero","genero_alumno","genero_estudiante","gender","sex"))
school_col   <- if ("gestion2" %in% nm) "gestion2" else detect_first(c("GESTION","gestion","gestion_escuela","gestion_colegio","gestion_ie","tipo_gestion","sector","sector_escuela"))
language_col <- detect_first(c("lengua_materna","idioma_materno","lengua_madre","lengua","idioma","lengua_estudiante","lengua_principal"))
area_col     <- detect_first(c("area","área","ambito","ámbito","zona","area_geografica","ambito_geografico"))

message2("Detected columns -> gender:", gender_col, ", school:", school_col, ", language:", language_col, ", area:", area_col)

# Standardize groups into compact labels
std_group <- function(vec, type) {
  x <- trimws(tolower(as.character(vec)))
  if (type == "gender") {
    boy_tokens  <- c("m","masculino","male","nino","niño","hombre","1","varon","varón")
    girl_tokens <- c("f","femenino","female","nina","niña","mujer","2")
    return(ifelse(x %in% boy_tokens, "boy", ifelse(x %in% girl_tokens, "girl", NA_character_)))
  } else if (type == "language") {
    spanish_tokens <- c("castellano","espanol","español","espaniol","castellana")
    return(ifelse(grepl(paste(spanish_tokens, collapse = "|"), x), "spanish", "other"))
  } else if (type == "school") {
    public_tokens  <- c("publico","pública","publica","estatal","nacional")
    private_tokens <- c("privado","particular","parroquial","no estatal")
    return(ifelse(grepl(paste(public_tokens, collapse = "|"), x), "public",
                  ifelse(grepl(paste(private_tokens, collapse = "|"), x), "private", NA_character_)))
  } else if (type == "area") {
    rural_tokens <- c("rural")
    urban_tokens <- c("urbana","urbano","urban")
    return(ifelse(grepl(paste(rural_tokens, collapse = "|"), x), "rural",
                  ifelse(grepl(paste(urban_tokens, collapse = "|"), x), "urban", NA_character_)))
  }
  rep(NA_character_, length(x))
}

# Build a tidy df for plotting for a given group type
build_plot_df <- function(type) {
  grp_col <- switch(type,
                    gender = gender_col,
                    language = language_col,
                    school = school_col,
                    area   = area_col,
                    NA_character_)
  if (is.na(grp_col) || !(grp_col %in% names(em))) return(NULL)
  g <- std_group(em[[grp_col]], type)
  df <- tibble(group = factor(g),
               medida500_M = em$medida500_M,
               medida500_L = em$medida500_L)
  df <- df %>% filter(!is.na(group) & !is.na(medida500_M) & !is.na(medida500_L))
  if (nrow(df) == 0) return(NULL)
  df
}

# --------- Plotters ---------
plot_violin <- function(df, title_suf) {
  long <- pivot_longer(df, cols = c("medida500_M","medida500_L"), names_to = "measure", values_to = "value")
  ggplot(long, aes(x = group, y = value, fill = group)) +
    geom_violin(trim = FALSE, alpha = 0.6, color = "grey60") +
    geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.9, fill = "white") +
    facet_wrap(~ measure, scales = "free_y") +
    labs(title = paste0("Distribution by group — ", title_suf), x = NULL, y = NULL) +
    theme_bw(base_size = 12) + theme(legend.position = "none")
}

plot_scatter <- function(df, title_suf) {
  set.seed(123)
  n_keep <- min(60000, nrow(df))
  if (nrow(df) > n_keep) df <- df[sample.int(nrow(df), n_keep), , drop = FALSE]
  ggplot(df, aes(x = medida500_M, y = medida500_L, color = group)) +
    geom_point(alpha = 0.12, size = 0.5) +
    stat_density_2d(linewidth = 0.8) +
    labs(title = paste0("M vs L — colored by ", title_suf), x = "Math (medida500_M)", y = "Language (medida500_L)", color = "Group") +
    theme_bw(base_size = 12) +
    scale_color_viridis_d(option = "C", end = 0.85)
}

# --------- Generate requested plots ---------
want <- args$group %||% "all"    # gender|language|school|area|all
ptype <- args$plot %||% "both"    # violin|scatter|both

make_for <- function(type, label) {
  df <- build_plot_df(type)
  if (is.null(df)) { message2("Skip ", type, ": missing column or empty after cleaning"); return(invisible()) }
  if (ptype %in% c("violin","both")) {
    p <- plot_violin(df, label)
    fn <- file.path(fig_dir, paste0("violin_", type, ".png"))
    ggsave(fn, p, width = 8, height = 6, dpi = 130)
    message2("Saved ", fn)
  }
  if (ptype %in% c("scatter","both")) {
    p <- plot_scatter(df, label)
    fn <- file.path(fig_dir, paste0("scatter_", type, ".png"))
    ggsave(fn, p, width = 8, height = 6, dpi = 130)
    message2("Saved ", fn)
  }
}

if (want == "all" || want == "gender") make_for("gender", "gender (boy/girl)")
if (want == "all" || want == "language") make_for("language", "language (spanish/other)")
if (want == "all" || want == "school") make_for("school", "school (public/private)")
if (want == "all" || want == "area") make_for("area", "area (rural/urban)")

message2("Done. Figures under: ", fig_dir)
