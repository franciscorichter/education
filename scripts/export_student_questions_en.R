# Export ENLA Student Questionnaire Questions to English CSV and LaTeX
# - Loads the student questionnaire workbook dictionary (sheet "Diccionario" or 2nd sheet)
# - Builds the p01..p27 question list with sub-items and options
# - Translates to English using DeepL if DEEPL_API_KEY is available
# - Writes Spanish and English CSVs
# - Generates a LaTeX report in English listing all questions and options
#
# Run from terminal (R):
#   R -e "source('github/education/scripts/export_student_questions_en.R'); export_student_questions_en()"
#
# Outputs:
#   github/education/data/student_questions_es.csv
#   github/education/data/student_questions_en.csv
#   github/education/reports/student_questionnaire_en.tex

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(purrr)
})

# Optional translation via DeepL (set DEEPL_API_KEY env var)
# Uses deeplr if installed; otherwise falls back to identity translation
have_deepl <- requireNamespace("deeplr", quietly = TRUE)

# Resolve paths relative to this script's directory
get_script_dir <- function() {
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(dirname(normalizePath(sub("--file=", "", file_arg[1]))))
  normalizePath(getwd(), mustWork = TRUE)
}

SCRIPT_DIR <- get_script_dir()
ROOT_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = TRUE)
XLSX_DIR <- file.path(ROOT_DIR, "data", "xlsx")
DATA_DIR <- file.path(ROOT_DIR, "data")
REPORTS_DIR <- file.path(ROOT_DIR, "reports")

# Ensure output directories exist
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(REPORTS_DIR)) dir.create(REPORTS_DIR, recursive = TRUE, showWarnings = FALSE)

# Helper: normalize p-codes to pXX or pXX_YY
normalize_code <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  m <- regmatches(x, regexpr("(?i)^p\\d{1,2}(?:_\\d{1,2})?$", x))
  if (length(m) == 0 || !nzchar(m)) return(NA_character_)
  m <- tolower(m)
  parts <- strsplit(sub("^p", "", m), "_")[[1]]
  top <- sprintf("p%02d", suppressWarnings(as.integer(parts[1])))
  if (length(parts) > 1) paste0(top, "_", sprintf("%02d", suppressWarnings(as.integer(parts[2])))) else top
}

# Read student dictionary (sheet Diccionario or 2nd sheet)
get_student_dict <- function() {
  dict_path <- file.path(XLSX_DIR, "ENLA2024_6Pestudiante_EBRD1.xlsx")
  if (!file.exists(dict_path)) stop("Student workbook not found: ", dict_path)
  sheets <- tryCatch(readxl::excel_sheets(dict_path), error = function(e) character())
  if (length(sheets) < 1) stop("No sheets in workbook: ", dict_path)
  sheet_name <- if (any(grepl("(?i)^diccionario$", sheets))) sheets[grepl("(?i)^diccionario$", sheets)][1] else sheets[min(2, length(sheets))]
  dict_raw <- tryCatch(readxl::read_excel(dict_path, sheet = sheet_name, col_names = TRUE), error = function(e) NULL)
  if (is.null(dict_raw)) stop("Failed to read dictionary sheet from: ", dict_path)
  dict_raw <- suppressMessages(tibble::as_tibble(dict_raw))

  nm <- names(dict_raw)
  nm_l <- tolower(nm)
  code_col <- which(nm_l == "variable"); if (length(code_col) == 0) code_col <- 1
  q_col <- which(nm_l == "pregunta"); if (length(q_col) == 0) q_col <- min(2, ncol(dict_raw))
  d_col <- which(nm_l == "descriptor"); if (length(d_col) == 0) d_col <- min(3, ncol(dict_raw))
  # Option columns commonly named cat_1, cat_2, ...
  opt_cols <- which(grepl("(?i)^cat_?\\d+$", nm) | grepl("^(?i)cat[_. ]?\\d+", nm))

  code_vec <- stringr::str_trim(as.character(dict_raw[[code_col]]))
  norm_codes <- vapply(code_vec, normalize_code, character(1))
  wide <- tibble(
    code = norm_codes,
    pregunta = stringr::str_squish(as.character(dict_raw[[q_col]])),
    descripcion = stringr::str_squish(as.character(dict_raw[[d_col]]))
  )
  if (length(opt_cols) > 0) {
    extras <- dict_raw[opt_cols]
    names(extras) <- make.names(names(extras), unique = TRUE)
    wide <- dplyr::bind_cols(wide, extras)
  }
  wide <- wide %>%
    filter(!is.na(code) & grepl("^p\\d{2}(_\\d{2})?$", tolower(code))) %>%
    filter(!is.na(pregunta) | !is.na(descripcion)) %>%
    distinct(code, .keep_all = TRUE) %>%
    mutate(top = sub("^(p\\d{2}).*", "\\1", tolower(code))) %>%
    arrange(top, code)

  # Summarize by top code
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

# Build p01..p27 table with subitems text labeled with codes (e.g., p10_01: text)
build_student_questions <- function() {
  d <- get_student_dict()
  pseq <- sprintf("p%02d", 1:27)
  df <- dplyr::left_join(tibble::tibble(item = pseq), d$best, by = c("item" = "top"))
  # Build subitems labeled by code from wide
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
  # Split options on separator if present
  df$options_raw <- ifelse(is.na(df$options) | !nzchar(df$options), NA_character_, df$options)
  df$options_list <- purrr::map(df$options_raw, function(x) {
    if (is.na(x) || !nzchar(x)) return(character())
    # options were collapsed with " | "
    trimws(unlist(strsplit(x, "\\|", fixed = FALSE)))
  })
  df %>% dplyr::select(item, question, subitems_raw, options_list)
}

# Translate a character vector using DeepL if available and key provided; else identity
translate_vec <- function(x, target_lang = "EN") {
  if (length(x) == 0) return(x)
  x_chr <- as.character(x)
  key <- Sys.getenv("DEEPL_API_KEY", unset = "")
  if (have_deepl && nzchar(key)) {
    # deeplr::translate2 handles vectorized input
    res <- tryCatch({
      deeplr::translate2(text = x_chr, target_lang = target_lang, auth_key = key)
    }, error = function(e) NULL)
    if (!is.null(res)) return(res)
  }
  # Fallback: return original text
  x_chr
}

# Write LaTeX report of English questions
write_latex_report <- function(df_en, out_tex) {
  # Escape LaTeX special characters
  esc <- function(s) {
    s <- gsub("\\\\", "\\\\textbackslash{}", s)
    s <- gsub("([#%&_$])", "\\\\\\1", s)
    s <- gsub("~", "\\\\textasciitilde{}", s)
    s <- gsub("\\^", "\\\\textasciicircum{}", s)
    s <- gsub("{", "\\\\{", s, fixed = TRUE)
    s <- gsub("}", "\\\\}", s, fixed = TRUE)
    s
  }
  lines <- c(
    "\\documentclass[11pt]{article}",
    "\\usepackage[margin=1in]{geometry}",
    "\\usepackage{enumitem}",
    "\\usepackage[T1]{fontenc}",
    "\\usepackage[utf8]{inputenc}",
    "\\title{ENLA 2024 — Student Questionnaire (English Translation)}",
    paste0("\\date{Generated on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "}"),
    "\\begin{document}",
    "\\maketitle",
    "\\tableofcontents",
    "\\newpage"
  )
  for (i in seq_len(nrow(df_en))) {
    item <- toupper(df_en$item[i])
    q <- df_en$question_en[i]
    subs <- df_en$subitems_en[[i]]
    opts <- df_en$options_en[[i]]
    lines <- c(lines, paste0("\\section*{", esc(item), ": ", esc(ifelse(is.na(q) || !nzchar(q), "(no question text)", q)), "}"))
    # Sub-items
    if (!is.null(subs) && length(subs) > 0) {
      lines <- c(lines, "\\subsection*{Sub-items}", "\\begin{itemize}[leftmargin=*]")
      for (s in subs) {
        if (!is.na(s) && nzchar(s)) lines <- c(lines, paste0("  \\item ", esc(s)))
      }
      lines <- c(lines, "\\end{itemize}")
    }
    # Options
    if (!is.null(opts) && length(opts) > 0) {
      lines <- c(lines, "\\subsection*{Options}", "\\begin{itemize}[leftmargin=*]")
      for (o in opts) {
        if (!is.na(o) && nzchar(o)) lines <- c(lines, paste0("  \\item ", esc(o)))
      }
      lines <- c(lines, "\\end{itemize}")
    }
    lines <- c(lines, "\\bigskip")
  }
  lines <- c(lines, "\\end{document}")
  writeLines(lines, con = out_tex)
}

# Bilingual LaTeX (ES on left, EN on right) using tabularx
write_latex_report_bilingual <- function(df_es, df_en, out_tex) {
  esc <- function(s) {
    s <- gsub("\\\\", "\\\\textbackslash{}", s)
    s <- gsub("([#%&_$])", "\\\\\\1", s)
    s <- gsub("~", "\\\\textasciitilde{}", s)
    s <- gsub("\\^", "\\\\textasciicircum{}", s)
    s <- gsub("{", "\\\\{", s, fixed = TRUE)
    s <- gsub("}", "\\\\}", s, fixed = TRUE)
    s
  }
  # Ensure same ordering by item
  ord <- match(df_es$item, df_en$item)
  df_en <- df_en[ord, ]

  header <- c(
    "\\documentclass[11pt]{article}",
    "\\usepackage[margin=1in]{geometry}",
    "\\usepackage{enumitem}",
    "\\usepackage[T1]{fontenc}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage{tabularx}",
    "\\usepackage{array}",
    "\\usepackage{ragged2e}",
    "\\newcolumntype{Y}{>{\\RaggedRight\\arraybackslash}X}",
    "\\title{ENLA 2024 — Student Questionnaire (Bilingual ES–EN)}",
    paste0("\\date{Generated on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "}"),
    "\\begin{document}",
    "\\maketitle",
    "\\tableofcontents",
    "\\newpage"
  )
  lines <- header
  for (i in seq_len(nrow(df_es))) {
    item <- toupper(df_es$item[i])
    q_es <- ifelse(is.na(df_es$question[i]) || !nzchar(df_es$question[i]), "(sin texto)", df_es$question[i])
    q_en <- ifelse(is.na(df_en$question_en[i]) || !nzchar(df_en$question_en[i]), "(no question text)", df_en$question_en[i])
    subs_es <- df_es$subitems_raw[[i]]
    subs_en <- df_en$subitems_en[[i]]
    opts_es <- df_es$options_list[[i]]
    opts_en <- df_en$options_en[[i]]

    lines <- c(lines, paste0("\\section*{", esc(item), "}"))
    lines <- c(lines, "\\begin{tabularx}{\\textwidth}{YY}")
      lines <- c(lines, "\\textbf{Pregunta (ES)} & \\textbf{Question (EN)} \\\")
    lines <- c(lines, paste0(esc(q_es), " & ", esc(q_en), " \\\\"))
    # Sub-items
    if (!is.null(subs_es) && length(subs_es) > 0 || !is.null(subs_en) && length(subs_en) > 0) {
        lines <- c(lines, "\\multicolumn{2}{l}{\\textbf{Sub-items}} \\\")
      left <- if (length(subs_es) > 0) paste0("\\begin{itemize}[leftmargin=*]", paste0("\\item ", esc(subs_es), collapse = "\n"), "\\end{itemize}") else ""
      right <- if (length(subs_en) > 0) paste0("\\begin{itemize}[leftmargin=*]", paste0("\\item ", esc(subs_en), collapse = "\n"), "\\end{itemize}") else ""
      lines <- c(lines, paste0(left, " & ", right, " \\\\"))
    }
    # Options
    if (!is.null(opts_es) && length(opts_es) > 0 || !is.null(opts_en) && length(opts_en) > 0) {
        lines <- c(lines, "\\multicolumn{2}{l}{\\textbf{Options / Opciones}} \\\")
      left <- if (length(opts_es) > 0) paste0("\\begin{itemize}[leftmargin=*]", paste0("\\item ", esc(opts_es), collapse = "\n"), "\\end{itemize}") else ""
      right <- if (length(opts_en) > 0) paste0("\\begin{itemize}[leftmargin=*]", paste0("\\item ", esc(opts_en), collapse = "\n"), "\\end{itemize}") else ""
      lines <- c(lines, paste0(left, " & ", right, " \\\\"))
    }
    lines <- c(lines, "\\end{tabularx}")
    lines <- c(lines, "\\bigskip")
  }
  lines <- c(lines, "\\end{document}")
  writeLines(lines, con = out_tex)
}

# Main entrypoint
export_student_questions_en <- function() {
  message("Building student questions table (ES)...")
  df <- build_student_questions()
  # Spanish CSV
  out_es <- file.path(DATA_DIR, "student_questions_es.csv")
  readr::write_csv(df %>% mutate(
    subitems = vapply(subitems_raw, function(v) paste(v, collapse = " | "), character(1)),
    options = vapply(options_list, function(v) paste(v, collapse = " | "), character(1))
  ) %>% select(item, question, subitems, options), out_es)
  message("Wrote ", out_es)

  message("Translating to English (DeepL if available)...")
  # Prepare vectors for translation
  q_en <- translate_vec(df$question, target_lang = "EN-US")
  # Flatten lists and translate, but keep aligned by row
  sub_en <- lapply(df$subitems_raw, function(v) translate_vec(v, target_lang = "EN-US"))
  opt_en <- lapply(df$options_list, function(v) translate_vec(v, target_lang = "EN-US"))

  df_en <- tibble::tibble(
    item = df$item,
    question_en = q_en,
    subitems_en = sub_en,
    options_en = opt_en
  )

  out_en <- file.path(DATA_DIR, "student_questions_en.csv")
  readr::write_csv(df_en %>% mutate(
    subitems = vapply(subitems_en, function(v) paste(v, collapse = " | "), character(1)),
    options = vapply(options_en, function(v) paste(v, collapse = " | "), character(1))
  ) %>% select(item, question_en, subitems, options), out_en)
  message("Wrote ", out_en)

  # LaTeX report
  out_tex <- file.path(REPORTS_DIR, "student_questionnaire_en.tex")
  write_latex_report(df_en, out_tex)
  message("LaTeX report written: ", out_tex)

  # Bilingual LaTeX report (ES–EN side by side)
  out_tex_bi <- file.path(REPORTS_DIR, "student_questionnaire_bilingual.tex")
  write_latex_report_bilingual(
    df_es = df %>% mutate(question = ifelse(is.na(question), "", question)),
    df_en = df_en,
    out_tex = out_tex_bi
  )
  message("Bilingual LaTeX report written: ", out_tex_bi)

  invisible(list(es_csv = out_es, en_csv = out_en, tex = out_tex, tex_bilingual = out_tex_bi))
}

# Auto-run if executed directly (not when sourced from tests)
if (sys.nframe() == 0) {
  export_student_questions_en()
}
