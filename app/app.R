# ENLA Item Network Shiny App
# Author: Cascade
# Date: 2025-09-18
# Description:
# - Lets you pick an ENLA questionnaire workbook, set an edge threshold, and renders the
#   item-level correlation network. Below the plot it lists all questions (variables).
# - On startup, it pre-processes the ENLA xlsx files to cached RDS files for fast loading.

suppressPackageStartupMessages({
  library(shiny)
  library(readxl)
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(igraph)
  library(RColorBrewer)
  library(tibble)
  library(scales)
  library(DT)
})

# Quiet NSE notes
utils::globalVariables(c("pregunta","descripcion","top","item","question","options"))

# ----- Paths -----
root_dir <- normalizePath("/Users/pancho/Library/CloudStorage/Dropbox/Pancho/25 - CSG/04 - Education", mustWork = TRUE)
xlsx_dir <- file.path(root_dir, "01 - Data", "xlsx")

# Check if xlsx directory exists, if not try alternative locations
if (!dir.exists(xlsx_dir)) {
  # Try to find Excel files in the root directory
  xlsx_dir <- root_dir
  # If no Excel files in root, check if there are any subdirectories with Excel files
  possible_dirs <- list.dirs(root_dir, recursive = FALSE)
  for (dir in possible_dirs) {
    if (length(list.files(dir, pattern = "\\.xlsx$", ignore.case = TRUE)) > 0) {
      xlsx_dir <- dir
      break
    }
  }
}

# Use the app directory as the cache/DB location
app_dir <- normalizePath(getwd(), mustWork = TRUE)
cache_dir <- app_dir
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

## Removed external cached mapping to avoid confusion; we now only use the 2nd sheet of the ENLA student XLSX

# ----- Helpers -----
# Bump this to force-refresh cached RDS objects after logic changes
cache_version <- 3L
col_pat <- "^p(\\d{2})(?:_\\d{2})?$"  # p01..p27_08

coerce_resp <- function(x) {
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))
  x_chr <- as.character(x)
  map_vec <- c(A=1,B=2,C=3,D=4,E=5,a=1,b=2,c=3,d=4,e=5)
  mapped <- suppressWarnings(as.numeric(recode(x_chr, !!!map_vec)))
  if (all(is.na(mapped))) mapped <- suppressWarnings(as.numeric(x_chr))
  mapped
}

# Normalize cod_mod7 to 7-digit zero-padded character
norm_cod_mod7 <- function(x) {
  if (is.null(x)) return(x)
  x_chr <- as.character(x)
  x_chr <- gsub("[^0-9]", "", x_chr)
  # keep empty as NA
  x_chr[nchar(x_chr) == 0] <- NA_character_
  # truncate or pad to 7 on the right logic: if longer, take last 7 digits
  x_chr <- ifelse(nchar(x_chr) > 7, substr(x_chr, nchar(x_chr)-6, nchar(x_chr)), x_chr)
  x_chr <- ifelse(!is.na(x_chr), sprintf("%07s", x_chr), x_chr)
  x_chr
}

# Load EM performance dataset (students) once
load_em_perf <- function() {
  em_path <- file.path(xlsx_dir, "EM_6P_2024_alumnos_innominados.xlsx")
  # Prefer prepared fast file if available
  prep_path <- file.path(app_dir, "data_prepared", "em_alumnos.csv.gz")
  if (file.exists(prep_path)) {
    df <- tryCatch(readr::read_csv(prep_path, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(df)) return(df)
  }
  if (!file.exists(em_path)) return(NULL)
  # Fast cache path keyed by source mtime
  cache_path <- file.path(cache_dir, "EM_6P_2024_alumnos_innominados.rds")
  src_mtime <- tryCatch(file.info(em_path)$mtime, error = function(e) NA)
  if (file.exists(cache_path)) {
    obj <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(obj) && identical(obj$src_mtime, src_mtime)) {
      # Ensure cod_mod7 normalized when reading from cache
      if ("cod_mod7" %in% names(obj$df)) obj$df$cod_mod7 <- norm_cod_mod7(obj$df$cod_mod7)
      return(obj$df)
    }
  }
  # Read BD sheet explicitly
  sh <- tryCatch(readxl::excel_sheets(em_path), error = function(e) character())
  sheet_to_read <- if ("BD" %in% sh) "BD" else if (length(sh) > 0) sh[1] else 1
  df <- tryCatch(readxl::read_excel(em_path, sheet = sheet_to_read), error = function(e) NULL)
  if (is.null(df)) return(NULL)

  # Ensure df is a proper data frame
  if (!is.data.frame(df)) {
    return(NULL)
  }

  df <- tibble::as_tibble(df)

  # Standardize key column names - add null check
  if (is.null(names(df)) || length(names(df)) == 0) {
    return(NULL)
  }

  # Standardize key column names safely
  cn <- names(df)
  nml <- tolower(cn)
  if (!is.null(nml) && length(nml) > 0) {
    idx <- which(nml == "id_estudiante"); if (length(idx) > 0) cn[idx] <- "ID_ESTUDIANTE"
    idx <- which(nml == "cod_mod7"); if (length(idx) > 0) cn[idx] <- "cod_mod7"
    idx <- which(nml == "medida500_l"); if (length(idx) > 0) cn[idx] <- "medida500_L"
    idx <- which(nml == "medida500_m"); if (length(idx) > 0) cn[idx] <- "medida500_M"
    names(df) <- cn
  }

  # Keep only relevant columns
  keep <- intersect(c("ID_ESTUDIANTE","cod_mod7","medida500_L","medida500_M"), names(df))
  if (length(keep) == 0) return(NULL)
  df <- dplyr::select(df, dplyr::all_of(keep))
  # Coerce scores to numeric
  if ("medida500_L" %in% names(df)) df$medida500_L <- suppressWarnings(as.numeric(df$medida500_L))
  if ("medida500_M" %in% names(df)) df$medida500_M <- suppressWarnings(as.numeric(df$medida500_M))
  # Save to fast cache with mtime
  tryCatch(saveRDS(list(df = df, src_mtime = src_mtime), cache_path), error = function(e) NULL)
  df
}
em_perf <- tryCatch({
  load_em_perf()
}, error = function(e) {
  message("Warning: Could not load EM performance data: ", e$message)
  NULL
})

# Load EM performance dataset (students) once
load_em_data <- function() {
  em_path <- file.path(app_dir, "EM_6P_2024_alumnos_innominados.rds")
  if (!file.exists(em_path)) return(NULL)

  df <- tryCatch(readRDS(em_path), error = function(e) {
    # If RDS file is corrupted, try to remove it and return NULL
    tryCatch(file.remove(em_path), error = function(e2) NULL)
    NULL
  })

  if (is.null(df)) return(NULL)

  # If it's cached with mtime structure, extract df
  if (is.list(df) && "df" %in% names(df)) {
    df <- df$df
  }

  # Validate that df is a proper data frame
  if (!is.data.frame(df)) {
    return(NULL)
  }

  df
}
em_data <- load_em_data()

# Generic dictionary reader from the 2nd sheet of any ENLA workbook
get_any_dict <- function(xlsx_path) {
  if (!file.exists(xlsx_path)) return(list(wide = tibble::tibble(), best = tibble::tibble()))
  sheets <- tryCatch(readxl::excel_sheets(xlsx_path), error = function(e) character())
  if (length(sheets) < 2) return(list(wide = tibble::tibble(), best = tibble::tibble()))
  sheet_name <- if (any(grepl("(?i)^diccionario$", sheets))) sheets[grepl("(?i)^diccionario$", sheets)][1] else sheets[2]
  dict_raw <- tryCatch(readxl::read_excel(xlsx_path, sheet = sheet_name, col_names = TRUE), error = function(e) NULL)
  if (is.null(dict_raw)) return(list(wide = tibble::tibble(), best = tibble::tibble()))
  dict_raw <- suppressMessages(tibble::as_tibble(dict_raw))

  # Heuristics to detect key columns by names and by content
  nm <- names(dict_raw)
  nm_l <- tolower(nm)
  # 1) code column: choose the column with max proportion of values matching pXX(_YY)?
  is_code_match <- function(v) {
    vv <- stringr::str_trim(as.character(v))
    m <- grepl("(?i)^p\\d{1,2}(?:_\\d{1,2})?$", vv)
    mean(m, na.rm = TRUE)
  }
  code_scores <- vapply(dict_raw, is_code_match, numeric(1))
  code_idx <- if (any(code_scores > 0)) which.max(code_scores) else 1
  # 2) pregunta (question) column: prefer names containing these tokens
  pref_name <- function(tokens) which.max(ifelse(nm_l %in% nm_l, sapply(nm_l, function(x) max(grepl(paste(tokens, collapse="|"), x, ignore.case = TRUE))), 0))
  q_idx <- suppressWarnings(pref_name(c("pregunta","enunciado","preg","stem")))
  if (length(q_idx) == 0 || q_idx < 1 || q_idx > ncol(dict_raw)) q_idx <- min(2, ncol(dict_raw))
  # 3) descripcion column
  d_idx <- suppressWarnings(pref_name(c("descriptor","descripcion","desc","def")))
  if (length(d_idx) == 0 || d_idx < 1 || d_idx > ncol(dict_raw)) d_idx <- min(3, ncol(dict_raw))
  # 4) option columns by name patterns
  opt_idx <- which(
    grepl("^(cat|op|opt|opc|opcion|resp|alt)[_. ]?\\d+", nm_l) |
    grepl("^cat_?\\d+$", nm_l) |
    grepl("^v[0-9]+$", nm_l) |
    grepl("^op_?\\d+$", nm_l)
  )
  opt_idx <- setdiff(opt_idx, c(code_idx, q_idx, d_idx))

  # Build normalized frame
  code_vec <- stringr::str_trim(as.character(dict_raw[[code_idx]]))
  normalize_code <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    m <- regmatches(x, regexpr("(?i)^p\\d{1,2}(?:_\\d{1,2})?$", x))
    if (length(m) == 0 || !nzchar(m)) return(NA_character_)
    m <- tolower(m)
    parts <- strsplit(sub("^p", "", m), "_")[[1]]
    top <- sprintf("p%02d", suppressWarnings(as.integer(parts[1])))
    if (length(parts) > 1) paste0(top, "_", sprintf("%02d", suppressWarnings(as.integer(parts[2])))) else top
  }
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
    filter(!is.na(code) & grepl("^p\\d{2}(_\\d{2})?$", code)) %>%
    filter(!is.na(pregunta) | !is.na(descripcion)) %>%
    distinct(code, .keep_all = TRUE) %>%
    mutate(top = sub("^(p\\d{2}).*", "\\1", code)) %>%
    arrange(top, code)
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
    if (nrow(df_top) == 0) return(tibble::tibble(top = NA_character_, question = NA_character_, subitems = list(character()), options = NA_character_))
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
  if (!"top" %in% names(wide)) wide$top <- NA_character_
  if (nrow(wide) == 0) {
    best <- tibble::tibble(top = character(), question = character(), subitems = I(list()), options = character())
  } else {
    best <- wide %>% dplyr::group_by(top) %>% dplyr::group_modify(~ summarize_top(.x)) %>% dplyr::ungroup()
  }
  list(wide = wide, best = best)
}

pretty_label <- function(v) {
  # Strip leading 'p' from codes like p12 or p12_03 for display
  v_low <- tolower(v)
  v2 <- sub("^p(\\d{2})", "\\1", v_low)
  v2 <- gsub("_", " ", v2)
  v2[v_low %in% c("l","m")] <- toupper(v2[v_low %in% c("l","m")])
  v2
}

# Robust student dictionary reader from the 2nd sheet of ENLA student workbook
get_student_dict <- function() {
  dict_path <- file.path(xlsx_dir, "ENLA2024_6Pestudiante_EBRD1.xlsx")
  if (!file.exists(dict_path)) return(list(wide = tibble::tibble(), best = tibble::tibble()))
  sheets <- tryCatch(readxl::excel_sheets(dict_path), error = function(e) character())
  if (length(sheets) < 1) return(list(wide = tibble::tibble(), best = tibble::tibble()))
  sheet_name <- if (any(grepl("(?i)^diccionario$", sheets))) sheets[grepl("(?i)^diccionario$", sheets)][1] else sheets[min(2, length(sheets))]
  dict_raw <- tryCatch(readxl::read_excel(dict_path, sheet = sheet_name, col_names = TRUE), error = function(e) NULL)
  if (is.null(dict_raw)) return(list(wide = tibble::tibble(), best = tibble::tibble()))
  dict_raw <- suppressMessages(tibble::as_tibble(dict_raw))
  # Map columns explicitly: variable, Pregunta, Descriptor, cat_*
  nm <- names(dict_raw)
  nm_l <- tolower(nm)
  code_col <- which(nm_l == "variable"); if (length(code_col)==0) code_col <- 1
  q_col <- which(nm_l == "pregunta"); if (length(q_col)==0) q_col <- min(2, ncol(dict_raw))
  d_col <- which(nm_l == "descriptor"); if (length(d_col)==0) d_col <- min(3, ncol(dict_raw))
  opt_cols <- which(grepl("(?i)^cat_?\\d+$", nm) | grepl("^(?i)cat[_. ]?\\d+", nm))
  code_vec <- stringr::str_trim(as.character(dict_raw[[code_col]]))
  # normalize to pXX or pXX_YY
  normalize_code <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    m <- regmatches(x, regexpr("(?i)^p\\d{1,2}(?:_\\d{1,2})?$", x))
    if (length(m) == 0 || !nzchar(m)) return(NA_character_)
    m <- tolower(m)
    parts <- strsplit(sub("^p", "", m), "_")[[1]]
    top <- sprintf("p%02d", as.integer(parts[1]))
    if (length(parts) > 1) paste0(top, "_", sprintf("%02d", as.integer(parts[2]))) else top
  }
  norm_codes <- vapply(code_vec, normalize_code, character(1))
  wide <- tibble(
    code = norm_codes,
    pregunta = stringr::str_squish(as.character(dict_raw[[q_col]])),
    descripcion = stringr::str_squish(as.character(dict_raw[[d_col]]))
  )
  # Compute top immediately to ensure the column exists even if later filters drop rows
  wide <- wide %>% mutate(top = sub("^(p\\d{2}).*", "\\1", tolower(code)))
  # bind any extra option columns (D+)
  if (length(opt_cols) > 0) {
    extras <- dict_raw[opt_cols]
    names(extras) <- make.names(names(extras), unique = TRUE)
    wide <- dplyr::bind_cols(wide, extras)
  }
  wide <- wide %>%
    filter(!is.na(code) & grepl("^p\\d{2}(_\\d{2})?$", tolower(code))) %>%
    filter(!is.na(pregunta) | !is.na(descripcion)) %>%
    distinct(code, .keep_all = TRUE) %>%
    arrange(top, code)
  # Build per-top summaries: primary question (stem), sub-items list, and options collapsed
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
    # Stem: prefer row where code==top and non-NA pregunta; else first non-NA pregunta; else first non-NA descripcion
    top_code <- unique(df_top$top)[1]
    cand1 <- safe_first(df_top$pregunta[df_top$code == top_code])
    cand2 <- safe_first(df_top$pregunta)
    cand3 <- safe_first(df_top$descripcion)
    stem <- safe_first(c(cand1, cand2, cand3))
    # Sub-items = descripcion for rows with sub-codes pXX_YY
    sub_rows <- df_top %>% dplyr::filter(grepl("_\\d{2}$", code) & !is.na(descripcion) & nzchar(descripcion))
    subitems <- unique(sub_rows$descripcion)
    options <- collapse_opts_vec(df_top)
    tibble::tibble(top = top_code, question = stem, subitems = list(subitems), options = options)
  }
  best <- wide %>% dplyr::group_by(top) %>% dplyr::group_modify(~ summarize_top(.x)) %>% dplyr::ungroup()
  list(wide = wide, best = best)
}

# Produce full student questions p01..p27 using dictionary best mapping
student_questions_full <- function() {
  d <- get_student_dict()
  if (is.null(d$best) || nrow(d$best) == 0) return(tibble::tibble(item = character(), question = character(), subitems = character(), options = character()))
  pseq <- sprintf("p%02d", 1:27)
  df <- left_join(tibble::tibble(item = pseq), d$best, by = c("item" = "top"))
  # Build subitems with codes from wide (e.g., p10_01: text)
  if (!is.null(d$wide) && nrow(d$wide) > 0) {
    wide <- d$wide
    wide$item <- sub("^(p\\d{2}).*", "\\1", wide$code)
    sub_df <- wide %>% dplyr::filter(grepl("_\\d{2}$", code) & !is.na(descripcion) & nzchar(descripcion)) %>%
      dplyr::transmute(item = item, subtxt = paste0(code, ": ", descripcion))
    sub_map <- split(sub_df$subtxt, sub_df$item)
    df$subitems <- purrr::map_chr(df$item, function(tp) {
      xs <- sub_map[[tp]]
      if (is.null(xs) || length(xs) == 0) return("")
      xs <- xs[!is.na(xs) & nzchar(xs)]
      if (length(xs) == 0) return("")
      paste0("<ul>", paste0("<li>", htmltools::htmlEscape(xs), "</li>", collapse = ""), "</ul>")
    })
  } else {
    # Fallback: render any existing list column as bullets
    df <- df %>% mutate(
      subitems = purrr::map_chr(subitems, function(xs) {
        if (is.null(xs) || length(xs) == 0 || all(is.na(xs))) return("")
        xs <- xs[!is.na(xs) & nzchar(xs)]
        if (length(xs) == 0) return("")
        paste0("<ul>", paste0("<li>", htmltools::htmlEscape(xs), "</li>", collapse = ""), "</ul>")
      })
    )
  }
  df %>% transmute(item, question = question, subitems = subitems, options = options)
}

# Improved data cache management
build_cache_for <- function(xlsx_path) {
  base <- tools::file_path_sans_ext(basename(xlsx_path))
  rds_path <- file.path(cache_dir, paste0(base, ".rds"))

  # Check for existing cache and version compatibility
  if (file.exists(rds_path)) {
    obj <- tryCatch(readRDS(rds_path), error = function(e) NULL)
    if (!is.null(obj) && is.list(obj) && !is.null(obj$version) && identical(obj$version, cache_version)) {
      return(obj)
    }
  }

  # Try prepared data first
  prepared_bd <- file.path(app_dir, "data_prepared", paste0("bd_", base, ".csv.gz"))
  source_tag <- "excel"

  if (file.exists(prepared_bd)) {
    df <- tryCatch({
      readr::read_csv(prepared_bd, show_col_types = FALSE)
    }, error = function(e) {
      NULL
    })

    if (!is.null(df)) {
      source_tag <- "prepared"
    } else {
      df <- NULL
    }
  }

  # Fallback to Excel if prepared data fails
  if (is.null(df)) {
    if (!file.exists(xlsx_path)) {
      return(list(
        base = base,
        data = NULL,
        vars = character(0),
        sheet = "File not found",
        aggregations = list(),
        error = "Source file not found"
      ))
    }

    sheets <- tryCatch(readxl::excel_sheets(xlsx_path), error = function(e) character())
    sheet_to_read <- if ("BD" %in% sheets) "BD" else if (length(sheets) > 0) sheets[1] else "BD"

    df <- tryCatch({
      readxl::read_excel(xlsx_path, sheet = sheet_to_read)
    }, error = function(e) {
      return(list(
        base = base,
        data = NULL,
        vars = character(0),
        sheet = sheet_to_read,
        aggregations = list(),
        error = paste("Failed to read sheet:", e$message)
      ))
    })

    if (is.list(df) && !is.null(df$error)) {
      return(df)  # Return error object
    }

    df <- tibble::as_tibble(df)
  }

  # Standardize key names safely
  cn <- names(df)
  if (!is.null(cn) && length(cn) > 0) {
    nml <- tolower(cn)
    idx <- which(nml == "id_estudiante"); if (length(idx) > 0) cn[idx] <- "ID_ESTUDIANTE"
    idx <- which(nml == "cod_mod7"); if (length(idx) > 0) cn[idx] <- "cod_mod7"
    names(df) <- cn
  }

  # Find item columns more robustly
  item_cols <- find_item_columns(df)

  if (length(item_cols) < 2) {
    keys_safe <- tryCatch(dplyr::select(df, dplyr::any_of(c("ID_ESTUDIANTE","cod_mod7"))), error = function(e) NULL)
    return(list(
      base = base,
      data = NULL,
      vars = character(0),
      sheet = if (exists("sheet_to_read")) sheet_to_read else source_tag,
      aggregations = list(),
      keys = keys_safe,
      error = paste("Only", length(item_cols), "pXX columns found")
    ))
  }

  # Process data without magrittr pipe to avoid masking issues
  X <- tryCatch(dplyr::select(df, dplyr::all_of(item_cols)), error = function(e) NULL)
  if (is.null(X)) {
    return(list(
      base = base,
      data = NULL,
      vars = character(0),
      sheet = if (exists("sheet_to_read")) sheet_to_read else source_tag,
      aggregations = list(),
      keys = tryCatch(dplyr::select(df, dplyr::any_of(c("ID_ESTUDIANTE","cod_mod7"))), error = function(e) NULL),
      error = "Failed to select item columns"
    ))
  }
  # Coerce responses column-wise
  X[] <- lapply(X, coerce_resp)
  keys_df <- tryCatch(dplyr::select(df, dplyr::any_of(c("ID_ESTUDIANTE","cod_mod7"))), error = function(e) NULL)

  # Precompute aggregations
  aggregations <- compute_aggregations(X, item_cols)

  # Return complete cache object
  obj <- list(
    base = base,
    data = X,
    vars = item_cols,
    sheet = if (exists("sheet_to_read")) sheet_to_read else source_tag,
    aggregations = aggregations,
    keys = keys_df,
    version = cache_version,
    source = source_tag
  )

  # Save cache
  tryCatch(saveRDS(obj, rds_path), error = function(e) NULL)

  obj
}

# Improved item column detection
find_item_columns <- function(df) {
  # Defensive checks: df must be a data.frame or matrix with columns
  if (is.null(df)) return(character(0))
  if (!is.data.frame(df) && !is.matrix(df)) return(character(0))
  nc <- tryCatch(ncol(df), error = function(e) NA_integer_)
  if (is.na(nc) || is.null(nc) || nc == 0) return(character(0))

  col_names <- names(df)
  if (is.null(col_names)) return(character(0))

  # Pattern 1: pXX or pXX_YY
  p_cols <- col_names[grepl("^(p\\d{2})(_\\d{2})?$", col_names, ignore.case = TRUE)]

  # Pattern 2: numeric columns that might be items (not IDs or metadata)
  potential_cols <- setdiff(col_names, c("ID_ESTUDIANTE", "cod_mod7", "id_estudiante", "cod_mod7"))

  # Filter to reasonable item columns (avoid single character columns, metadata, etc.)
  item_cols <- potential_cols[
    grepl("^[a-zA-Z][a-zA-Z0-9_]*$", potential_cols) &
    nchar(potential_cols) > 1 &
    nchar(potential_cols) < 20
  ]

  # Combine patterns
  all_item_cols <- unique(c(p_cols, item_cols))

  # Final validation: must have at least some non-NA values
  valid_cols <- character(0)
  for (col in all_item_cols) {
    if (sum(!is.na(df[[col]])) > 10) {  # At least 10 valid responses
      valid_cols <- c(valid_cols, col)
    }
  }

  valid_cols
}

# Improved aggregation computation
compute_aggregations <- function(X, item_cols) {
  if (is.null(X) || length(item_cols) < 2) {
    return(list(mean = NULL, median = NULL, zmean = NULL, pca1 = NULL))
  }

  # Group columns by top-level construct
  top_codes <- stringr::str_match(item_cols, "^(p\\d{2})")[,2]
  top_codes <- tolower(top_codes)
  split_cols <- split(item_cols, top_codes)

  # Only process groups with multiple items
  split_cols <- split_cols[sapply(split_cols, length) > 1]

  if (length(split_cols) == 0) {
    return(list(mean = NULL, median = NULL, zmean = NULL, pca1 = NULL))
  }

  # Aggregation function
  aggregate_group <- function(vals_df, method) {
    m <- as.matrix(vals_df)
    if (method == "mean") {
      return(rowMeans(m, na.rm = TRUE))
    } else if (method == "median") {
      return(apply(m, 1, median, na.rm = TRUE))
    } else if (method == "zmean") {
      z <- scale(m)
      return(rowMeans(z, na.rm = TRUE))
    } else if (method == "pca1") {
      # Impute missing values with column means
      impute_col_means <- function(col) {
        mu <- mean(col, na.rm = TRUE)
        if (is.na(mu)) return(rep(NA_real_, length(col)))
        col[is.na(col)] <- mu
        col
      }
      mi <- apply(m, 2, impute_col_means)

      # Remove columns with no variation
      keep_cols <- which(apply(mi, 2, function(c) sd(c, na.rm = TRUE) > 0))

      if (length(keep_cols) == 0) return(rowMeans(m, na.rm = TRUE))
      if (length(keep_cols) == 1) {
        z <- scale(mi[, keep_cols])
        return(as.numeric(z))
      }

      pc <- tryCatch(prcomp(mi[, keep_cols, drop = FALSE], center = TRUE, scale. = TRUE), error = function(e) NULL)
      if (is.null(pc) || is.null(pc$x)) return(rowMeans(m, na.rm = TRUE))
      return(as.numeric(pc$x[, 1]))
    } else {
      return(rowMeans(m, na.rm = TRUE))
    }
  }

  # Build aggregations
  build_top <- function(method) {
    purrr::map_dfc(names(split_cols), function(tp) {
      cols <- split_cols[[tp]]
      vals <- dplyr::select(X, dplyr::all_of(cols))
      out <- aggregate_group(vals, method)
      tibble::tibble(!!tp := out)
    })
  }

  list(
    mean = build_top("mean"),
    median = build_top("median"),
    zmean = build_top("zmean"),
    pca1 = build_top("pca1")
  )
}

# Discover ENLA workbooks and build or load cache (exclude temp lock files).
# Prefer loading a prebuilt augmented DB in the app directory if present.
augmented_db_path <- file.path(app_dir, "enla_augmented_db.rds")
all_cache_named <- tryCatch(readRDS(augmented_db_path), error = function(e) NULL)
if (is.null(all_cache_named)) {
  all_xlsx <- list.files(xlsx_dir, pattern = "(?i)ENLA.*\\.xlsx$", full.names = TRUE)
  all_xlsx <- all_xlsx[!grepl("^~\\$", basename(all_xlsx))]
  # Exclude base_web2 files for now
  all_xlsx <- all_xlsx[!grepl("(?i)base_web2", basename(all_xlsx))]
  all_cache <- map(all_xlsx, build_cache_for)
  all_cache_named <- setNames(all_cache, map_chr(all_xlsx, identity))
  tryCatch(saveRDS(all_cache_named, augmented_db_path), error = function(e) NULL)
} else {
  # Refresh any outdated entries based on cache_version
  changed <- FALSE
  for (nm in names(all_cache_named)) {
    obj <- all_cache_named[[nm]]
    if (!is.list(obj) || is.null(obj$version) || !identical(obj$version, cache_version)) {
      all_cache_named[[nm]] <- build_cache_for(nm)
      changed <- TRUE
    }
  }
  if (changed) tryCatch(saveRDS(all_cache_named, augmented_db_path), error = function(e) NULL)
}

# Named vector for selectInput
idx_keep <- !grepl("(?i)base_web2", basename(names(all_cache_named)))
all_cache_named <- all_cache_named[idx_keep]
all_xlsx <- names(all_cache_named)
all_cache <- unname(all_cache_named)
choices <- setNames(map_chr(all_cache, ~ .x$base), all_xlsx)

# Friendly title from workbook filename
friendly_title <- function(p) {
  b <- basename(p)
  bl <- tolower(b)
  if (grepl("estudiante", bl)) return("Estudiante")
  if (grepl("docente(mat|matemat)", bl)) return("Docente Matemática")
  if (grepl("docente(com|comunic)", bl)) return("Docente Comunicación")
  if (grepl("docente(tutor)", bl)) return("Docente Tutor")
  if (grepl("director.*f1", bl)) return("Director F1")
  if (grepl("director.*f2", bl)) return("Director F2")
  if (grepl("familia", bl)) return("Familia")
  tools::file_path_sans_ext(b)
}

## (counts will be shown inside the tab; keep titles clean)

# Build questionnaire tabs dynamically (put Estudiante first, then Familia)
est_idx <- which(grepl("(?i)estudiante", basename(all_xlsx)))
fam_idx <- which(grepl("(?i)familia", basename(all_xlsx)))
est_path <- if (length(est_idx) > 0) all_xlsx[est_idx[1]] else NULL
fam_path <- if (length(fam_idx) > 0) all_xlsx[fam_idx[1]] else NULL
rest_idx <- setdiff(seq_along(all_xlsx), c(est_idx[1], fam_idx[1]))
tab_list <- list()
if (!is.null(est_path)) {
  tab_list[[length(tab_list)+1]] <- tabPanel(title = friendly_title(est_path), value = est_path)
}
if (!is.null(fam_path)) {
  tab_list[[length(tab_list)+1]] <- tabPanel(title = friendly_title(fam_path), value = fam_path)
}
for (i in rest_idx) {
  tab_list[[length(tab_list)+1]] <- tabPanel(title = friendly_title(all_xlsx[i]), value = all_xlsx[i])
}

# ----- UI -----
ui <- fluidPage(
  title = "ENLA Educational Data Analysis Platform",
  tags$head(tags$style(HTML(
  "body{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif;} .muted{color:#6b7280}
   table.dataTable td, table.dataTable th { white-space: normal !important; word-wrap: break-word; }
   .dataTables_wrapper .dataTables_scrollBody{ overflow: auto; }")
) ),
  h1("ENLA Educational Data Analysis Platform", style = "color: #2c3e50; font-weight: bold;"),
  p("Comprehensive analysis platform for ENLA (National Learning Assessment) educational data",
    style = "color: #7f8c8d; font-size: 16px; margin-bottom: 20px;"),

  # Data availability status
  wellPanel(
    h4("📊 Data Status", style = "margin-top: 0;"),
    fluidRow(
      column(4,
        h5("EM Performance Data"),
        textOutput("em_status", inline = TRUE)
      ),
      column(4,
        h5("Questionnaire Data"),
        textOutput("questionnaire_status", inline = TRUE)
      ),
      column(4,
        h5("Data Quality"),
        textOutput("data_quality_status", inline = TRUE)
      )
    )
  ),

  # Main navigation tabs
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",

    # Tab 1: Exploratory Data Analysis
    tabPanel(
      title = "📊 Exploratory Data Analysis",
      value = "eda",
      h3("EM Performance Data Analysis"),
      p("Exploratory analysis of student performance data including mathematics assessment scores and demographic information."),

      # Placeholder content for EDA
      wellPanel(
        h4("EM Data Overview"),
        verbatimTextOutput("em_data_summary"),
        hr(),
        h4("Data Structure"),
        DTOutput("em_data_structure"),
        hr(),
        h4("Performance Distributions"),
        fluidRow(
          column(6, plotOutput("em_performance_dist")),
          column(6, plotOutput("em_demographics"))
        ),
        hr(),
        h4("Data Integration Quality Assessment"),
        wellPanel(
          style = "background-color: #f8f9fa; border-left: 4px solid #007bff;",
          verbatimTextOutput("data_integration_check")
        )
      ),
    ),

    # Tab 2: Network and Questionnaire Analysis
    tabPanel(
      title = "🔗 Network & Questionnaire Analysis",
      value = "network_analysis",
      h3("Correlation Networks and Questionnaire Exploration"),

      # Dynamic questionnaire tabs
      do.call(tabsetPanel, c(list(id = "questionnaire_tab", selected = if (!is.null(est_path)) est_path else NULL), tab_list)),
      br(),
      tags$div(
        class = "muted",
        style = "margin-bottom:12px;",
        HTML("<strong>Descripción:</strong> Selecciona un cuestionario arriba. Usa la pestaña <em>Network</em> para construir y explorar la red de ítems, y la pestaña <em>Questions</em> para ver las preguntas y sub-ítems.")
      ),
      tags$div(style = "margin:-6px 0 8px 0; color:#374151;",
               textOutput("tab_counts", inline = TRUE)
      ),
      hr(),
      tabsetPanel(
        id = "content_tabs",
        tabPanel(
          title = "Redes",
          value = "network",
          wellPanel(
            fluidRow(
              column(4,
                     sliderInput("thr", "Edge threshold |r|:", min = 0, max = 0.8, value = 0.2, step = 0.01, width = "100%")
              ),
              column(5,
                     radioButtons("level", "Network level:", inline = TRUE,
                                  choices = c("Second level: items pXX_YY" = "item",
                                              "Top level: aggregates pXX" = "top"),
                                  selected = "item")
              ),
              column(3,
                     br(),
                     actionButton("go", "Build network", class = "btn btn-primary", width = "100%")
              )
            ),
            conditionalPanel(
              condition = "input.level == 'top'",
              selectInput("agg", "Aggregation method:",
                          choices = c("Mean" = "mean",
                                      "Median" = "median",
                                      "Z-Mean" = "zmean",
                                      "PCA-1" = "pca1"),
                          selected = "mean", width = "50%")
            ),
            checkboxInput("add_perf_nodes", "Agregar nodos de desempeño L/M", value = FALSE)
          ),
          div(style = "margin:6px 0 4px 4px; font-size: 13px; color:#374151;",
              textOutput("net_stats", inline = TRUE)
          ),
          div(style = "margin:6px 0 4px 4px; font-size: 12px; color:#6b7280;",
              textOutput("integration_status", inline = TRUE)
          ),
          plotOutput("network_plot", height = "720px")
        ),
        tabPanel(
          title = "Cuestionario",
          value = "questions",
          DTOutput("questions_table")
        )
      )
    ),

    # Tab 3: Advanced Modeling
    tabPanel(
      title = "🤖 Advanced Modeling",
      value = "advanced",
      h3("Advanced Statistical Models and Machine Learning"),
      p("Advanced modeling capabilities including regression analysis, clustering, and predictive modeling for educational outcomes."),

      # Placeholder content for advanced modeling
      wellPanel(
        h4("Advanced Analytics Features"),
        p("This section will include:"),
        tags$ul(
          tags$li("Regression modeling for outcome prediction"),
          tags$li("Clustering analysis for student grouping"),
          tags$li("Machine learning models for performance prediction"),
          tags$li("Factor analysis for construct validation"),
          tags$li("Longitudinal analysis for trend detection")
        ),
        hr(),
        h4("Coming Soon"),
        p("Advanced modeling features are under development and will be available in future releases.")
      )
    )
  )
)

# ----- Server -----
server <- function(input, output, session) {

  # EDA Tab: EM Data Summary
  output$em_data_summary <- renderPrint({
    if (is.null(em_data)) {
      return("EM performance data not available")
    }

    cat("EM Performance Data Summary:\n")
    cat(sprintf("Total rows: %d\n", nrow(em_data)))
    cat(sprintf("Total columns: %d\n", ncol(em_data)))
    cat("\nColumn names:\n")
    cat(paste(names(em_data), collapse = ", "))
    cat("\n\nData types:\n")
    cat(paste(sapply(em_data, class), collapse = ", "))
    cat("\n\nMissing values per column:\n")
    na_counts <- sapply(em_data, function(x) sum(is.na(x)))
    print(as.data.frame(na_counts))
  })

  # EDA Tab: Data Status Indicators
  output$em_status <- renderText({
    if (is.null(em_data)) {
      "❌ Not loaded"
    } else {
      sprintf("✅ %d rows", nrow(em_data))
    }
  })

  output$questionnaire_status <- renderText({
    if (length(all_cache) == 0) {
      "❌ No data"
    } else {
      sprintf("✅ %d datasets", length(all_cache))
    }
  })

  output$data_quality_status <- renderText({
    if (length(all_cache) == 0) {
      "❌ No data"
    } else {
      high_quality <- sum(sapply(all_cache, function(x) mean(is.na(x$data)) < 0.3))
      total <- length(all_cache)
      sprintf("✅ %d/%d good quality", high_quality, total)
    }
  })

  # EDA Tab: Data Structure Table
  output$em_data_structure <- renderDT({
    if (is.null(em_data)) {
      return(data.frame(Message = "EM performance data not available"))
    }

    # Create data structure summary
    structure_data <- data.frame(
      Column = names(em_data),
      Type = sapply(em_data, class),
      Non_NA_Values = sapply(em_data, function(x) sum(!is.na(x))),
      NA_Count = sapply(em_data, function(x) sum(is.na(x))),
      Unique_Values = sapply(em_data, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )

    structure_data$NA_Percentage <- round(structure_data$NA_Count / nrow(em_data) * 100, 2)

    datatable(structure_data,
              options = list(pageLength = 10, autoWidth = TRUE),
              rownames = FALSE) %>%
      formatPercentage('NA_Percentage', 1)
  })

  # EDA Tab: Performance Distribution Plot
  output$em_performance_dist <- renderPlot({
    if (is.null(em_data)) {
      plot(1, type = "n", xlab = "", ylab = "", main = "No EM data available")
      text(1, 1, "EM performance data not available")
      return()
    }

    # Check for performance columns
    perf_cols <- grep("medida500", names(em_data), ignore.case = TRUE, value = TRUE)

    if (length(perf_cols) == 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "No performance data found")
      text(1, 1, "No performance columns found in EM data")
      return()
    }

    # Create histograms for performance scores
    par(mfrow = c(length(perf_cols), 1), mar = c(4, 4, 2, 1))

    for (col in perf_cols) {
      hist(em_data[[col]],
           main = paste("Distribution of", col),
           xlab = col,
           col = "lightblue",
           border = "white",
           breaks = 30)
    }
  })

  # Data Status Indicators
  output$em_status <- renderText({
    if (is.null(em_data)) {
      "❌ Not loaded"
    } else {
      sprintf("✅ %d rows", nrow(em_data))
    }
  })

  output$questionnaire_status <- renderText({
    if (length(all_cache) == 0) {
      "❌ No data"
    } else {
      sprintf("✅ %d datasets", length(all_cache))
    }
  })

  output$data_quality_status <- renderText({
    if (length(all_cache) == 0) {
      "❌ No data"
    } else {
      high_quality <- sum(sapply(all_cache, function(x) mean(is.na(x$data)) < 0.3))
      total <- length(all_cache)
      sprintf("✅ %d/%d good quality", high_quality, total)
    }
  })

  # Data Integration Quality Check
  output$data_integration_check <- renderPrint({
    cat("=== DATA INTEGRATION QUALITY ASSESSMENT ===\n\n")

    # 1. Check EM data
    cat("1. EM PERFORMANCE DATA:\n")
    if (is.null(em_data)) {
      cat("❌ EM data not loaded\n")
    } else {
      cat(sprintf("✅ EM data: %d rows, %d columns\n", nrow(em_data), ncol(em_data)))
      cat("Columns:", paste(names(em_data), collapse = ", "), "\n")

      # Check for L/M scores
      l_col <- grep("medida500_L|medida500_l", names(em_data), ignore.case = TRUE, value = TRUE)
      m_col <- grep("medida500_M|medida500_m", names(em_data), ignore.case = TRUE, value = TRUE)

      if (length(l_col) > 0) {
        l_na_pct <- mean(is.na(em_data[[l_col[1]]])) * 100
        cat(sprintf("✅ L scores available (%s): %.1f%% missing\n", l_col[1], l_na_pct))
      } else {
        cat("❌ L scores missing\n")
      }

      if (length(m_col) > 0) {
        m_na_pct <- mean(is.na(em_data[[m_col[1]]])) * 100
        cat(sprintf("✅ M scores available (%s): %.1f%% missing\n", m_col[1], m_na_pct))
      } else {
        cat("❌ M scores missing\n")
      }
    }
    cat("\n")

    # 2. Check questionnaire data
    cat("2. QUESTIONNAIRE DATA:\n")
    if (length(all_cache) == 0) {
      cat("❌ No questionnaire data loaded\n")
    } else {
      for (i in seq_along(all_cache)) {
        cache <- all_cache[[i]]
        base_name <- cache$base
        cat(sprintf("✅ %s: %d rows, %d item columns, %d total columns\n",
                   base_name, nrow(cache$data), length(cache$vars), ncol(cache$data)))

        # Check for join keys
        has_student_id <- "ID_ESTUDIANTE" %in% names(cache$data)
        has_school_code <- "cod_mod7" %in% names(cache$data)

        if (has_student_id && has_school_code) {
          cat("   🔑 Join keys: ID_ESTUDIANTE ✓, cod_mod7 ✓\n")
        } else if (has_student_id) {
          cat("   🔑 Join keys: ID_ESTUDIANTE ✓, cod_mod7 ❌\n")
        } else if (has_school_code) {
          cat("   🔑 Join keys: ID_ESTUDIANTE ❌, cod_mod7 ✓\n")
        } else {
          cat("   ❌ No join keys found\n")
        }

        # Check data quality
        na_pct <- mean(is.na(cache$data)) * 100
        cat(sprintf("   📊 Data quality: %.1f%% missing values overall\n", na_pct))

        # Check item column quality
        if (length(cache$vars) > 0) {
          item_na_pct <- mean(sapply(cache$data[cache$vars], function(x) mean(is.na(x)))) * 100
          cat(sprintf("   📊 Item quality: %.1f%% missing values in items\n", item_na_pct))
        }
      }
    }
    cat("\n")

    # 3. Integration Analysis
    cat("3. DATA INTEGRATION ANALYSIS:\n")

    # Check student data for EM integration
    student_caches <- all_cache[grepl("estudiante|familia", sapply(all_cache, function(x) x$base), ignore.case = TRUE)]
    if (length(student_caches) > 0) {
      cat("✅ Student/Family questionnaires available for EM integration\n")

      # Check ID overlap
      for (cache in student_caches) {
        if ("ID_ESTUDIANTE" %in% names(cache$data) && !is.null(em_data)) {
          student_ids <- unique(cache$data$ID_ESTUDIANTE[!is.na(cache$data$ID_ESTUDIANTE)])
          em_ids <- unique(em_data$ID_ESTUDIANTE[!is.na(em_data$ID_ESTUDIANTE)])

          overlap <- length(intersect(student_ids, em_ids))
          total_student <- length(student_ids)
          total_em <- length(em_ids)

          if (total_student > 0) {
            match_rate <- overlap / total_student
            cat(sprintf("   🔗 %s: %d/%d students match EM data (%.1f%%)\n",
                       cache$base, overlap, total_student, match_rate * 100))
          }
        }
      }
    } else {
      cat("❌ No student questionnaires found for EM integration\n")
    }

    # Check school-level data for school code integration
    school_caches <- all_cache[!grepl("estudiante|familia", sapply(all_cache, function(x) x$base), ignore.case = TRUE)]
    if (length(school_caches) > 0 && !is.null(em_data)) {
      cat("✅ School-level questionnaires available\n")

      # Check school code overlap
      school_codes <- unique(unlist(lapply(school_caches, function(x) {
        if ("cod_mod7" %in% names(x$data)) {
          x$data$cod_mod7[!is.na(x$data$cod_mod7)]
        } else {
          character(0)
        }
      })))

      em_school_codes <- unique(em_data$cod_mod7[!is.na(em_data$cod_mod7)])
      school_overlap <- length(intersect(school_codes, em_school_codes))

      if (length(school_codes) > 0) {
        cat(sprintf("   🔗 School codes: %d/%d match EM data (%.1f%%)\n",
                   school_overlap, length(school_codes), school_overlap/length(school_codes)*100))
      }
    }

    # 4. Recommendations
    cat("\n4. RECOMMENDATIONS:\n")
    if (is.null(em_data)) {
      cat("⚠️  Load EM performance data for complete analysis\n")
    }

    low_quality_caches <- all_cache[sapply(all_cache, function(x) {
      mean(is.na(x$data)) > 0.5
    })]
    if (length(low_quality_caches) > 0) {
      cat("⚠️  Review data quality for:", paste(sapply(low_quality_caches, function(x) x$base), collapse = ", "), "\n")
    }

    # Check for data completeness
    incomplete_datasets <- all_cache[sapply(all_cache, function(x) length(x$vars) < 10)]
    if (length(incomplete_datasets) > 0) {
      cat("⚠️  Few items detected in:", paste(sapply(incomplete_datasets, function(x) x$base), collapse = ", "), "\n")
    }

    cat("✅ Data integration quality check complete\n")
  })

  plot_data <- eventReactive(input$go, {
    req(input$questionnaire_tab)
    thr <- input$thr
    # Resolve selection
    xlsx_path <- input$questionnaire_tab
    cache <- build_cache_for(xlsx_path)
    validate(need(!is.null(cache$data), paste0("No pXX variables were found for ", cache$base, " (sheet ", cache$sheet, ")")))

    X <- cache$data
    item_cols <- cache$vars
    # Items detected prior to NA/SD filtering, depending on level
    level <- isolate(input$level)
    if (!is.null(level) && level == "top") {
      items_detected <- length(unique(stringr::str_match(item_cols, "^(p\\d{2})")[,2]))
    } else {
      items_detected <- length(item_cols)
    }

    # If top-level requested, aggregate sub-items pXX_YY into pXX using selected aggregation
    if (!is.null(level) && level == "top") {
      top_codes <- stringr::str_match(item_cols, "^(p\\d{2})")[,2]
      top_codes <- tolower(top_codes)
      # Build list of columns per top
      split_cols <- split(item_cols, top_codes)
      # Aggregation helper per group
      aggregate_group <- function(vals_df, method) {
        m <- as.matrix(vals_df)
        if (method == "mean") {
          return(rowMeans(m, na.rm = TRUE))
        } else if (method == "median") {
          return(apply(m, 1, median, na.rm = TRUE))
        } else if (method == "zmean") {
          # Standardize each column then average
          z <- scale(m)
          return(rowMeans(z, na.rm = TRUE))
        } else if (method == "pca1") {
          # Impute NAs by column means before PCA
          impute_col_means <- function(col) {
            mu <- mean(col, na.rm = TRUE)
            if (is.na(mu)) return(rep(NA_real_, length(col)))
            col[is.na(col)] <- mu
            col
          }
          mi <- apply(m, 2, impute_col_means)
          # If all-NA columns lead to NA, fallback to rowMeans
          if (all(is.na(mi))) return(rowMeans(m, na.rm = TRUE))
          # If only one column after removing constant/NA, fallback to z of that column
          keep_cols <- which(apply(mi, 2, function(c) sd(c, na.rm = TRUE) > 0))
          if (length(keep_cols) == 0) return(rowMeans(m, na.rm = TRUE))
          if (length(keep_cols) == 1) {
            z <- scale(mi[, keep_cols])
            return(as.numeric(z))
          }
          pc <- tryCatch(prcomp(mi[, keep_cols, drop = FALSE], center = TRUE, scale. = TRUE), error = function(e) NULL)
          if (is.null(pc) || is.null(pc$x)) return(rowMeans(m, na.rm = TRUE))
          return(as.numeric(pc$x[, 1]))
        } else {
          return(rowMeans(m, na.rm = TRUE))
        }
      }
      agg_method <- isolate(input$agg)
      X_top <- purrr::map_dfc(names(split_cols), function(tp) {
        cols <- split_cols[[tp]]
        vals <- dplyr::select(X, dplyr::all_of(cols))
        out <- aggregate_group(vals, agg_method)
        tibble::tibble(!!tp := out)
      })
      X <- X_top
      item_cols <- names(X_top)
    }

    # ---- Integrate EM performance L/M as extra nodes (optional) ----
    if (isTRUE(isolate(input$add_perf_nodes))) {
      # Decide join key based on questionnaire type with improved logic
      join_key <- if (grepl("(?i)estudiante|familia", basename(xlsx_path))) {
        "ID_ESTUDIANTE"  # Individual student level
      } else {
        "cod_mod7"  # School level
      }

      # Check if we can perform integration
      has_join_key_in_questionnaire <- join_key %in% names(cache$keys)
      has_join_key_in_em <- join_key %in% names(em_perf)
      has_em_data <- !is.null(em_perf)

      if (has_em_data && has_join_key_in_questionnaire && has_join_key_in_em) {
        # Get questionnaire IDs and EM IDs for matching
        questionnaire_ids <- cache$keys[[join_key]]
        em_ids <- em_perf[[join_key]]

        # Remove NAs and get unique values
        questionnaire_ids_clean <- unique(questionnaire_ids[!is.na(questionnaire_ids)])
        em_ids_clean <- unique(em_ids[!is.na(em_ids)])

        # Calculate match statistics
        matches <- intersect(questionnaire_ids_clean, em_ids_clean)
        match_rate <- length(matches) / length(questionnaire_ids_clean)

        # Only proceed if we have reasonable match rate (>10%)
        if (match_rate > 0.1) {
          df_join <- dplyr::bind_cols(cache$keys, X)

          # Ensure consistent data types for joining
          df_join[[join_key]] <- as.character(df_join[[join_key]])
          em2 <- em_perf
          em2[[join_key]] <- as.character(em2[[join_key]])

          # Normalize school codes if needed
          if (join_key == "cod_mod7") {
            df_join[[join_key]] <- norm_cod_mod7(df_join[[join_key]])
            em2[[join_key]] <- norm_cod_mod7(em2[[join_key]])
          }

          # Perform the join
          df_join <- dplyr::left_join(df_join, em2, by = join_key)

          # Add L and M columns if available
          l_added <- FALSE
          m_added <- FALSE

          if ("medida500_L" %in% names(df_join)) {
            X$L <- suppressWarnings(as.numeric(df_join$medida500_L))
            l_added <- TRUE
          }

          if ("medida500_M" %in% names(df_join)) {
            X$M <- suppressWarnings(as.numeric(df_join$medida500_M))
            m_added <- TRUE
          }

          # Success message
          success_msg <- sprintf("✅ Performance integration successful (%.1f%% match rate)", match_rate * 100)
          if (l_added && m_added) {
            success_msg <- paste(success_msg, "L/M scores added")
          } else if (l_added) {
            success_msg <- paste(success_msg, "L scores added")
          } else if (m_added) {
            success_msg <- paste(success_msg, "M scores added")
          } else {
            success_msg <- paste(success_msg, "⚠️ No L/M columns found")
          }

        } else {
          # Poor match rate - don't integrate but explain why
          warning_msg <- sprintf("⚠️ Performance integration skipped: only %.1f%% match rate (minimum 10% required)",
                                match_rate * 100)
        }
      } else {
        # Cannot integrate - missing requirements
        missing_parts <- c()
        if (!has_em_data) missing_parts <- c(missing_parts, "EM performance data")
        if (!has_join_key_in_questionnaire) missing_parts <- c(missing_parts, paste(join_key, "in questionnaire"))
        if (!has_join_key_in_em) missing_parts <- c(missing_parts, paste(join_key, "in EM data"))

        error_msg <- paste("❌ Performance integration failed: missing", paste(missing_parts, collapse = ", "))
      }
    }

    # Drop low-quality columns
    sds <- map_dbl(X, ~ sd(.x, na.rm = TRUE))
    na_pct <- map_dbl(X, ~ mean(is.na(.x)))
    keep <- names(X)[sds > 0 & !is.na(sds) & na_pct < 0.9]
    # Always keep L and M if present
    keep <- union(keep, intersect(c("L","M"), names(X)))
    X <- dplyr::select(X, dplyr::all_of(keep))

    # Compute correlations
    cor_mat <- suppressWarnings(cor(X, use = "pairwise.complete.obs", method = "pearson"))
    # Build edge list from upper triangle to avoid factor comparisons
    inds <- which(upper.tri(cor_mat), arr.ind = TRUE)
    cor_df <- tibble(
      var1 = colnames(cor_mat)[inds[, 1]],
      var2 = colnames(cor_mat)[inds[, 2]],
      cor  = cor_mat[inds]
    ) %>% filter(!is.na(cor) & abs(cor) >= thr)

    # Node groups and colors (group by top pXX for items; for top-level, group is the node itself)
    node_names <- colnames(cor_mat)
    if (!is.null(level) && level == "top") {
      node_group <- node_names
    } else {
      node_group <- stringr::str_match(node_names, "^(p\\d{2})")[,2]
    }
    # Group L and M into their own categories for legend
    node_group[is.na(node_group) & node_names %in% c("L","M")] <- node_names[node_names %in% c("L","M")]
    node_df <- tibble(name = node_names, group = node_group)

    groups <- sort(unique(node_df$group))
    base_pal <- RColorBrewer::brewer.pal(8, "Set2")
    palette <- grDevices::colorRampPalette(base_pal)(max(8, length(groups)))
    col_map <- setNames(rep(palette, length.out = length(groups)), groups)

    G <- igraph::graph_from_data_frame(cor_df, directed = FALSE, vertices = node_df)
    V(G)$color <- col_map[V(G)$group]
    E(G)$weight <- abs(E(G)$cor)

    list(G = G, col_map = col_map, base = cache$base, vars = item_cols, items_detected = items_detected)
  }, ignoreInit = TRUE)

  output$integration_status <- renderText({
    req(plot_data())
    G <- plot_data()$G

    # Check if integration was attempted
    hasL <- "L" %in% igraph::V(G)$name
    hasM <- "M" %in% igraph::V(G)$name

    # Get integration info from plot_data
    base_name <- plot_data()$base
    is_student_data <- grepl("(?i)estudiante|familia", base_name)
    join_key_used <- if (is_student_data) "ID_ESTUDIANTE" else "cod_mod7"

    # Show integration status
    if (isTRUE(isolate(input$add_perf_nodes))) {
      if (hasL || hasM) {
        paste0("🔗 Performance integration: ", join_key_used, " ✓")
      } else {
        paste0("🔗 Performance integration: ", join_key_used, " ❌")
      }
    } else {
      paste0("🔗 Performance integration: disabled")
    }
  })

  output$network_plot <- renderPlot({
    req(plot_data())
    G <- plot_data()$G
    col_map <- plot_data()$col_map
    set.seed(123)
    coords <- layout_with_fr(G)
    labels_pretty <- pretty_label(V(G)$name)
    par(bg = "white", mar = c(1,1,1,1))
    title_suffix <- if (!is.null(isolate(input$level)) && isolate(input$level) == "top") "Top-level (pXX aggregates)" else "Item-level (pXX_YY)"
    # Larger nodes for L and M
    v_sizes <- ifelse(V(G)$name %in% c("L","M"), 14, 8)
    plot(G,
         layout = coords,
         vertex.label = labels_pretty,
         vertex.label.cex = 0.7,
         vertex.size = v_sizes,
         vertex.color = V(G)$color,
         edge.width = rescale(E(G)$weight, to = c(0.4, 3)),
         edge.color = rgb(0.2,0.2,0.2,0.25),
         main = paste0(plot_data()$base, " — ", title_suffix, " (|r| ≥ ", input$thr, ")"))
    legend(
      "topleft",
      legend = names(col_map),
      col = unname(col_map),
      pch = 16,
      pt.cex = 1.2,
      bty = "n",
      ncol = 2,
      title = ifelse(!is.null(isolate(input$level)) && isolate(input$level) == "top", "Top (pXX)", "Group (pXX)")
    )
  })

  output$questions_table <- renderDT({
    req(input$questionnaire_tab)
    # Always attempt to show dictionary-based questions for the selected tab (no need to build network)
    tryCatch({
      xlsx_path <- isolate(input$questionnaire_tab)
      # If Estudiante tab, prefer the dedicated student dictionary
      if (grepl("(?i)estudiante", basename(xlsx_path))) {
        dict <- get_student_dict()
      } else {
        dict <- get_any_dict(xlsx_path)
      }
      if (!is.null(dict$best) && nrow(dict$best) > 0) {
        # Build full item list dynamically from dictionary tops
        derive_tops <- function(d) {
          if (!is.null(d$wide) && nrow(d$wide) > 0 && "top" %in% names(d$wide)) {
            unique(d$wide$top)
          } else if (!is.null(d$best) && nrow(d$best) > 0 && "top" %in% names(d$best)) {
            unique(d$best$top)
          } else {
            character(0)
          }
        }
        tops <- derive_tops(dict)
        tops <- tops[!is.na(tops) & nzchar(tops)]
        if (length(tops) == 0) tops <- character(0)
        # Sort numerically by the number after 'p'
        ord <- order(suppressWarnings(as.integer(sub("^p", "", tops))))
        tops <- tops[ord]
        best <- dict$best
        df <- dplyr::left_join(tibble::tibble(item = tops), best, by = c("item" = "top"))
        # Ensure columns exist
        if (!"subitems" %in% names(df)) df$subitems <- ""
        if (!"options" %in% names(df)) df$options <- NA_character_
        if (!"question" %in% names(df)) df$question <- NA_character_
        # If we have a wide dictionary, enrich subitems with codes
        if (!is.null(dict$wide) && nrow(dict$wide) > 0) {
          wide <- dict$wide
          wide$item <- sub("^(p\\d{2}).*", "\\1", wide$code)
          sub_df <- wide %>% dplyr::filter(grepl("_\\d{2}$", code) & !is.na(descripcion) & nzchar(descripcion)) %>%
            dplyr::transmute(item = item, subtxt = paste0(code, ": ", descripcion))
          sub_map <- split(sub_df$subtxt, sub_df$item)
          df$subitems <- purrr::map_chr(df$item, function(tp) {
            xs <- sub_map[[tp]]
            if (is.null(xs) || length(xs) == 0) return("")
            xs <- xs[!is.na(xs) & nzchar(xs)]
            if (length(xs) == 0) return("")
            paste0("<ul>", paste0("<li>", htmltools::htmlEscape(xs), "</li>", collapse = ""), "</ul>")
          })
        }
        # Coerce to character to avoid JS list/object errors
        df <- df %>% mutate(
          question = dplyr::coalesce(as.character(question), ""),
          subitems = dplyr::coalesce(as.character(subitems), ""),
          options  = dplyr::coalesce(as.character(options),  "")
        )
        plen <- max(10, min(100, nrow(df)))
        return(
          df %>% rename(`Item (pXX)` = item, `Pregunta (stem)` = question, `Sub-items` = subitems, `Opciones` = options) %>%
            datatable(options = list(pageLength = plen, autoWidth = TRUE), rownames = FALSE, escape = FALSE)
        )
      } else {
        # Fallback to blank questions placeholder
        return(tibble::tibble(Message = "No dictionary rows detected for this workbook") %>% datatable(options = list(dom='t'), rownames = FALSE))
      }
    }, error = function(e) {
      # Graceful error table instead of JS error
      tibble::tibble(Message = paste("No dictionary available for this workbook:", basename(isolate(input$questionnaire_tab))),
                     Detail = e$message) %>%
        datatable(options = list(dom = 't', autoWidth = TRUE), rownames = FALSE)
    })
  })
}

shinyApp(ui, server)
