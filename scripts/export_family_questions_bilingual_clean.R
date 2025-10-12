# Generate a cleaner bilingual (ES–EN) Family questionnaire report (longtable, two columns)
# Usage:
#   R -e "source('github/education/scripts/export_family_questions_bilingual_clean.R'); export_family_questions_bilingual_clean()"

suppressPackageStartupMessages({
  library(readxl)
  library(stringr)
  library(tibble)
  library(dplyr)
})

# Resolve paths
get_script_dir <- function() {
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(dirname(normalizePath(sub("--file=", "", file_arg[1]))))
  normalizePath(getwd(), mustWork = TRUE)
}

SCRIPT_DIR <- get_script_dir()
ROOT_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = TRUE)
DATA_DIR <- file.path(ROOT_DIR, "data")
XLSX_DIR <- file.path(DATA_DIR, "xlsx")
REPORTS_DIR <- file.path(ROOT_DIR, "reports")
if (!dir.exists(REPORTS_DIR)) dir.create(REPORTS_DIR, recursive = TRUE, showWarnings = FALSE)

# Latex escaper
tex_esc <- function(s) {
  s <- as.character(s)
  s[is.na(s)] <- ""
  s <- gsub("\\\\", "\\\\textbackslash{}", s)
  s <- gsub("([#%&_$])", "\\\\\\1", s)
  s <- gsub("~", "\\\\textasciitilde{}", s)
  s <- gsub("\\^", "\\\\textasciicircum{}", s)
  s <- gsub("{", "\\\\{", s, fixed = TRUE)
  s <- gsub("}", "\\\\}", s, fixed = TRUE)
  s
}

# Import translator and override helpers from student script by sourcing it
# This gives us: translate_local_vec(), load_translation_overrides(), apply_overrides_vec(), write_bilingual_longtable()
# Note: the student script auto-runs only when sourced directly (sys.nframe()==0), so sourcing here is safe.
source(file.path(SCRIPT_DIR, "export_student_questions_bilingual_clean.R"))

# Generic dictionary reader based on app/app.R:get_any_dict()
get_any_dict <- function(xlsx_path) {
  if (!file.exists(xlsx_path)) stop("Workbook not found: ", xlsx_path)
  sheets <- tryCatch(readxl::excel_sheets(xlsx_path), error = function(e) character())
  if (length(sheets) < 1) stop("No sheets in workbook: ", xlsx_path)
  # Prefer a sheet named 'Diccionario', else use 2nd sheet
  sheet_name <- if (any(grepl("(?i)^diccionario$", sheets))) sheets[grepl("(?i)^diccionario$", sheets)][1] else sheets[min(2, length(sheets))]
  dict_raw <- tryCatch(readxl::read_excel(xlsx_path, sheet = sheet_name, col_names = TRUE), error = function(e) NULL)
  if (is.null(dict_raw)) stop("Failed to read dictionary sheet from: ", xlsx_path)
  dict_raw <- suppressMessages(tibble::as_tibble(dict_raw))

  nm <- names(dict_raw)
  nm_l <- tolower(nm)

  # detect code column by matching pXX(_YY)
  is_code_match <- function(v) {
    vv <- stringr::str_trim(as.character(v))
    m <- grepl("(?i)^p\\d{1,2}(?:_\\d{1,2})?$", vv)
    mean(m, na.rm = TRUE)
  }
  code_scores <- vapply(dict_raw, is_code_match, numeric(1))
  code_idx <- if (any(code_scores > 0)) which.max(code_scores) else 1

  # pick question and descriptor columns by name hints
  pick_by_tokens <- function(tokens) which.max(ifelse(nm_l %in% nm_l, sapply(nm_l, function(x) max(grepl(paste(tokens, collapse = "|"), x, ignore.case = TRUE))), 0))
  q_idx <- suppressWarnings(pick_by_tokens(c("pregunta","enunciado","preg","stem")))
  if (length(q_idx) == 0 || q_idx < 1 || q_idx > ncol(dict_raw)) q_idx <- min(2, ncol(dict_raw))
  d_idx <- suppressWarnings(pick_by_tokens(c("descriptor","descripcion","desc","def")))
  if (length(d_idx) == 0 || d_idx < 1 || d_idx > ncol(dict_raw)) d_idx <- min(3, ncol(dict_raw))

  # option columns: cat_1, op_1, etc.
  opt_idx <- which(
    grepl("^(cat|op|opt|opc|opcion|resp|alt)[_. ]?\\d+", nm_l) |
      grepl("^cat_?\\d+$", nm_l) |
      grepl("^v[0-9]+$", nm_l) |
      grepl("^op_?\\d+$", nm_l)
  )
  opt_idx <- setdiff(opt_idx, c(code_idx, q_idx, d_idx))

  normalize_code <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    m <- regmatches(x, regexpr("(?i)^p\\d{1,2}(?:_\\d{1,2})?$", x))
    if (length(m) == 0 || !nzchar(m)) return(NA_character_)
    m <- tolower(m)
    parts <- strsplit(sub("^p", "", m), "_")[[1]]
    top <- sprintf("p%02d", suppressWarnings(as.integer(parts[1])))
    if (length(parts) > 1) paste0(top, "_", sprintf("%02d", suppressWarnings(as.integer(parts[2])))) else top
  }

  code_vec <- stringr::str_trim(as.character(dict_raw[[code_idx]]))
  norm_codes <- vapply(code_vec, normalize_code, character(1))
  wide <- tibble(
    code = norm_codes,
    pregunta = stringr::str_squish(as.character(dict_raw[[q_idx]])),
    descripcion = stringr::str_squish(as.character(dict_raw[[d_idx]]))
  )
  if (length(opt_idx) > 0) {
    extras <- dict_raw[opt_idx]
    names(extras) <- make.names(names(extras), unique = TRUE)
    wide <- dplyr::bind_cols(wide, extras)
  }
  wide <- wide %>%
    filter(!is.na(code) & grepl("^p\\d{2}(_\\d{2})?$", tolower(code))) %>%
    filter(!is.na(pregunta) | !is.na(descripcion)) %>%
    distinct(code, .keep_all = TRUE) %>%
    mutate(top = sub("^(p\\d{2}).*", "\\1", tolower(code))) %>%
    arrange(top, code)

  # summarize per top code
  option_cols <- setdiff(names(wide), c("code","pregunta","descripcion","top"))
  collapse_opts_vec <- function(df_top) {
    if (length(option_cols) == 0) return(NA_character_)
    vals <- as.character(unlist(df_top[option_cols], use.names = FALSE))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0) NA_character_ else paste(unique(vals), collapse = " | ")
  }
  safe_first <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x) > 0) x[1] else NA_character_
  }
  summarize_top <- function(df_top) {
    top_code <- unique(df_top$top)[1]
    cand1 <- safe_first(df_top$pregunta[df_top$code == top_code])
    cand2 <- safe_first(df_top$pregunta)
    cand3 <- safe_first(df_top$descripcion)
    stem <- safe_first(c(cand1, cand2, cand3))
    sub_rows <- df_top %>% dplyr::filter(grepl("_\\d{2}$", code) & !is.na(descripcion) & nzchar(descripcion))
    subitems <- unique(sub_rows$descripcion)
    options <- collapse_opts_vec(df_top)
    tibble::tibble(top = top_code, question = stem, subitems = list(subitems), options = options)
  }
  best <- if (nrow(wide) == 0) tibble::tibble(top = character(), question = character(), subitems = I(list()), options = character())
  else wide %>% dplyr::group_by(top) %>% dplyr::group_modify(~ summarize_top(.x)) %>% dplyr::ungroup()
  list(wide = wide, best = best)
}

# Build family questions table using any-dict
build_family_questions <- function(xlsx_path) {
  d <- get_any_dict(xlsx_path)
  # Use detected top codes in order
  items <- unique(d$best$top)
  df <- tibble::tibble(item = items) %>% dplyr::left_join(d$best, by = c("item" = "top"))
  # Label subitems with codes from wide
  if (!is.null(d$wide) && nrow(d$wide) > 0) {
    wide <- d$wide
    wide$item <- sub("^(p\\d{2}).*", "\\1", tolower(wide$code))
    sub_df <- wide %>% dplyr::filter(grepl("_\\d{2}$", code) & !is.na(descripcion) & nzchar(descripcion)) %>%
      dplyr::transmute(item = item, sub_code = code, sub_desc = descripcion)
    sub_map <- split(paste0(sub_df$sub_code, ": ", sub_df$sub_desc), sub_df$item)
    df$subitems_raw <- purrr::map(df$item, ~ sub_map[[.x]] %||% character())
  } else {
    df$subitems_raw <- lapply(seq_len(nrow(df)), function(i) as.character(df$subitems[[i]]))
  }
  # Split options collapsed with |
  df$options_raw <- ifelse(is.na(df$options) | !nzchar(df$options), NA_character_, df$options)
  df$options_list <- purrr::map(df$options_raw, function(x) {
    if (is.na(x) || !nzchar(x)) return(character())
    trimws(unlist(strsplit(x, "\\|", fixed = FALSE)))
  })
  df %>% dplyr::select(item, question, subitems_raw, options_list)
}

# Export entrypoint
export_family_questions_bilingual_clean <- function(
  xlsx_path = file.path(XLSX_DIR, "ENLA2024_6Pfamilia_EBR.xlsx"),
  title = "ENLA 2024 — Family Questionnaire (Bilingual ES–EN)"
) {
  # Build ES
  df_es <- build_family_questions(xlsx_path)

  # Initial EN using local translator
  init_q_en <- translate_local_vec(df_es$question)
  init_sub_en <- lapply(df_es$subitems_raw, translate_local_vec)
  init_opt_en <- lapply(df_es$options_list, translate_local_vec)

  # Create or load TSV overrides
  tsv_path <- file.path(DATA_DIR, "family_translations_manual.tsv")
  if (!file.exists(tsv_path)) {
    es_vec <- unique(c(
      as.character(df_es$question),
      unlist(df_es$subitems_raw, use.names = FALSE),
      unlist(df_es$options_list, use.names = FALSE)
    ))
    es_vec <- es_vec[!is.na(es_vec) & nzchar(es_vec)]
    en_vec <- translate_local_vec(es_vec)
    writeLines(c("es\ten", paste0(es_vec, "\t", en_vec)), con = tsv_path, useBytes = TRUE)
    message("Created translations TSV: ", tsv_path)
  }
  overrides <- load_translation_overrides(tsv_path)

  # Apply overrides
  q_en <- apply_overrides_vec(init_q_en, overrides)
  sub_en <- lapply(df_es$subitems_raw, function(v) apply_overrides_vec(translate_local_vec(v), overrides))
  opt_en <- lapply(df_es$options_list, function(v) apply_overrides_vec(translate_local_vec(v), overrides))

  df_en <- tibble::tibble(
    item = df_es$item,
    question_en = q_en,
    subitems_en = sub_en,
    options_en = opt_en
  )

  # LaTeX out
  out_tex <- file.path(REPORTS_DIR, "family_questionnaire_bilingual_clean.tex")
  write_bilingual_longtable(df_es, df_en, out_tex, title = title)
  message("Bilingual (clean) LaTeX report written: ", out_tex)
  invisible(out_tex)
}

# Auto-run if executed directly
if (sys.nframe() == 0) {
  export_family_questions_bilingual_clean()
}
