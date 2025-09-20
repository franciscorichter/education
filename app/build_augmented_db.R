suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
})

# Paths
root_dir <- normalizePath("/Users/pancho/Library/CloudStorage/Dropbox/Pancho/25 - CSG/04 - Education", mustWork = TRUE)
xlsx_dir <- file.path(root_dir, "01 - Data", "xlsx")
app_dir <- normalizePath(getwd(), mustWork = TRUE)
augmented_db_path <- file.path(app_dir, "enla_augmented_db.rds")

# Helpers
col_pat <- "^p(\\d{2})(?:_\\d{2})?$"
coerce_resp <- function(x) {
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))
  x_chr <- as.character(x)
  map_vec <- c(A=1,B=2,C=3,D=4,E=5,a=1,b=2,c=3,d=4,e=5)
  mapped <- suppressWarnings(as.numeric(recode(x_chr, !!!map_vec)))
  if (all(is.na(mapped))) mapped <- suppressWarnings(as.numeric(x_chr))
  mapped
}

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
    impute_col_means <- function(col) {
      mu <- mean(col, na.rm = TRUE)
      if (is.na(mu)) return(rep(NA_real_, length(col)))
      col[is.na(col)] <- mu
      col
    }
    mi <- apply(m, 2, impute_col_means)
    keep_cols <- which(apply(mi, 2, function(c) sd(c) > 0))
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

build_cache_for <- function(xlsx_path) {
  base <- tools::file_path_sans_ext(basename(xlsx_path))
  sheets <- tryCatch(readxl::excel_sheets(xlsx_path), error = function(e) character())
  sheet_to_read <- if ("BD" %in% sheets) "BD" else if (length(sheets) > 0) sheets[1] else "BD"
  df <- readxl::read_excel(xlsx_path, sheet = sheet_to_read)
  df <- tibble::as_tibble(df)
  item_cols <- names(df)[str_detect(names(df), col_pat)]
  keep_idx <- str_match(item_cols, "^p(\\d{2})")[,2]
  keep_num <- suppressWarnings(as.integer(keep_idx))
  keep <- !is.na(keep_num) & keep_num >= 1 & keep_num <= 27
  item_cols <- item_cols[keep]
  if (length(item_cols) < 2) {
    return(list(base = base, data = NULL, vars = character(0), sheet = sheet_to_read, aggregations = list()))
  }
  X <- df %>% select(all_of(item_cols)) %>% mutate(across(everything(), coerce_resp))
  top_codes <- stringr::str_match(item_cols, "^(p\\d{2})")[,2] %>% tolower()
  split_cols <- split(item_cols, top_codes)
  build_top <- function(method) {
    XT <- purrr::map_dfc(names(split_cols), function(tp) {
      cols <- split_cols[[tp]]
      vals <- dplyr::select(X, dplyr::all_of(cols))
      out <- aggregate_group(vals, method)
      tibble::tibble(!!tp := out)
    })
    keep_idx <- stringr::str_match(names(XT), "^p(\\d{2})")[,2]
    keep_num <- suppressWarnings(as.integer(keep_idx))
    keep <- !is.na(keep_num) & keep_num >= 1 & keep_num <= 27
    dplyr::select(XT, which(keep))
  }
  aggregations <- list(
    mean = build_top("mean"),
    median = build_top("median"),
    zmean = build_top("zmean"),
    pca1 = build_top("pca1")
  )
  list(base = base, data = X, vars = item_cols, sheet = sheet_to_read, aggregations = aggregations)
}

# Build DB
all_xlsx <- list.files(xlsx_dir, pattern = "(?i)ENLA.*\\.xlsx$", full.names = TRUE)
all_xlsx <- all_xlsx[!grepl("^~\\$", basename(all_xlsx))]
all_cache <- purrr::map(all_xlsx, build_cache_for)
all_cache_named <- setNames(all_cache, all_xlsx)
saveRDS(all_cache_named, augmented_db_path)
cat("Saved augmented DB to:", augmented_db_path, "\n")
