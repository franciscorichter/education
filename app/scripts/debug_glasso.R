# Standalone Graphical Lasso debug script
# Usage from shell or R console:
#   Rscript app/scripts/debug_glasso.R [QUESTIONNAIRE_NAME] [RHO] [MAX_EDGES_PLOT]
# Example:
#   Rscript app/scripts/debug_glasso.R estudiante 0.15 300
# If QUESTIONNAIRE_NAME is omitted, the first available questionnaire is used.
# If RHO is omitted, defaults to 0.10. If MAX_EDGES_PLOT omitted, defaults to 300.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(glasso)
  library(igraph)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
# Flexible parsing: if first arg is numeric, treat as rho; otherwise as questionnaire pattern
q_name_arg <- NA_character_
rho <- 0.10
max_edges_plot <- 300
if (length(args) >= 1) {
  if (grepl("^[0-9.]+$", args[[1]])) {
    rho <- as.numeric(args[[1]])
    if (length(args) >= 2) max_edges_plot <- as.integer(args[[2]])
  } else {
    q_name_arg <- args[[1]]
    if (length(args) >= 2) rho <- as.numeric(args[[2]])
    if (length(args) >= 3) max_edges_plot <- as.integer(args[[3]])
  }
}

# Resolve paths relative to this script
repo_root <- normalizePath(getwd(), mustWork = FALSE)
app_dir <- file.path(repo_root, "app")
outputs_dir <- file.path(repo_root, "outputs")
if (!dir.exists(outputs_dir)) dir.create(outputs_dir, recursive = TRUE)
rds_path <- normalizePath(file.path(app_dir, "../data/enla_processed_data.rds"), mustWork = FALSE)
if (!file.exists(rds_path)) stop("RDS not found at ", rds_path)
cat("Loading RDS:", rds_path, "\n")

enla_data <- readRDS(rds_path)
if (is.null(enla_data$questionnaire_data) || length(enla_data$questionnaire_data) == 0) {
  stop("No questionnaire_data in RDS")
}

# Pick questionnaire (prefer success==TRUE and not base_web2)
qnms <- names(enla_data$questionnaire_data)
valid <- vapply(enla_data$questionnaire_data, function(q) isTRUE(q$success), logical(1))
not_base <- !grepl("(?i)base_web2", qnms)
pool <- qnms[valid & not_base]
if (length(pool) == 0) pool <- qnms[valid]
if (length(pool) == 0) pool <- qnms

if (!is.na(q_name_arg)) {
  sel <- pool[grepl(q_name_arg, pool, ignore.case = TRUE)]
  if (length(sel) == 0) stop("No valid questionnaire matching ", q_name_arg)
  q_name <- sel[[1]]
} else {
  q_name <- pool[[1]]
}
cat("Questionnaire:", q_name, "\n")

q <- enla_data$questionnaire_data[[q_name]]
if (is.null(q) || !isTRUE(q$success)) stop("Questionnaire not successful or missing")
X <- q$data
if (is.null(X) || nrow(X) == 0) stop("Questionnaire data empty")

# Variable selection (same as app logic)
nms <- colnames(X); if (is.null(nms)) nms <- character(0)
vars <- grep("^p\\d{1,2}(_\\d{1,2})?$", nms, ignore.case = TRUE, value = TRUE)
if (length(vars) == 0) {
  num_cols <- names(X)[vapply(X, is.numeric, logical(1))]
  vars <- num_cols
}
if (length(vars) < 2) stop("<2 candidate variables found")

cat("Candidate variables:", length(vars), "\n")

# Subset and preprocess
X <- dplyr::select(X, dplyr::all_of(vars))
X <- X[rowSums(is.na(X)) < ncol(X), , drop = FALSE]
if (nrow(X) < 10) stop("Not enough rows after NA filtering: ", nrow(X))
X[] <- lapply(X, function(col) suppressWarnings(as.numeric(col)))
for (j in seq_len(ncol(X))) {
  m <- mean(X[[j]], na.rm = TRUE); if (is.na(m)) m <- 0
  idx <- which(is.na(X[[j]])); if (length(idx) > 0) X[[j]][idx] <- m
}
# Remove zero-variance columns
sds <- vapply(X, function(x) sd(x, na.rm = TRUE), numeric(1))
keep <- names(sds)[sds > 0 & !is.na(sds)]
X <- dplyr::select(X, dplyr::all_of(keep))
if (ncol(X) < 2) stop("All selected variables have zero variance")

cat(sprintf("Data dims after cleaning: n=%d, p=%d\n", nrow(X), ncol(X)))

S <- stats::cov(X)
cat("Cov computed. Running glasso with rho=", rho, "...\n", sep = "")
fit <- glasso::glasso(S, rho = rho)
Theta <- fit$wi

# Adjacency and edges
adj <- (abs(Theta) > .Machine$double.eps)
diag(adj) <- FALSE
idx <- which(adj, arr.ind = TRUE)
if (nrow(idx) > 0) idx <- idx[idx[,1] < idx[,2], , drop = FALSE]

weights <- if (nrow(idx) > 0) {
  abs(Theta[cbind(idx[,1], idx[,2])])
} else numeric(0)

edge_df <- if (nrow(idx) > 0) {
  data.frame(
    from = colnames(Theta)[idx[,1]],
    to   = colnames(Theta)[idx[,2]],
    weight = weights,
    stringsAsFactors = FALSE
  )
} else {
  data.frame(from = character(0), to = character(0), weight = numeric(0), stringsAsFactors = FALSE)
}

cat("Edges:", nrow(edge_df), "\n")
if (nrow(edge_df) > 0) {
  print(utils::head(edge_df[order(-edge_df$weight), ], 10))
}

# Plot a subset
if (nrow(edge_df) > 0) {
  ord <- order(-edge_df$weight)
  edge_plot <- edge_df[ord, , drop = FALSE]
  if (nrow(edge_plot) > max_edges_plot) edge_plot <- edge_plot[seq_len(max_edges_plot), , drop = FALSE]
  used <- sort(unique(c(edge_plot$from, edge_plot$to)))
  nodes <- data.frame(name = colnames(Theta), group = stringr::str_match(colnames(Theta), "^(p\\d{2})")[,2], stringsAsFactors = FALSE)
  nodes$group[is.na(nodes$group)] <- nodes$name[is.na(nodes$group)]
  nodes <- dplyr::filter(nodes, .data$name %in% used)
  G <- igraph::graph_from_data_frame(edge_plot, directed = FALSE, vertices = nodes)
  safe_q <- gsub("[^A-Za-z0-9_]+","_", q_name)
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  out_png <- file.path(outputs_dir, sprintf("glasso_%s_rho%0.2f_edges%d_%s.png", safe_q, rho, nrow(edge_plot), ts))
  png(out_png, width = 1200, height = 900)
  set.seed(123)
  plot(G, layout = igraph::layout_with_fr(G), vertex.size = 6, vertex.label = NA, edge.width = scales::rescale(E(G)$weight, to = c(0.2, 2)))
  dev.off()
  cat("Saved plot to:", out_png, "\n")
  # Save full edges CSV as well
  out_csv <- file.path(outputs_dir, sprintf("glasso_edges_%s_rho%0.2f_%s.csv", safe_q, rho, ts))
  utils::write.csv(edge_df[ord, , drop = FALSE], out_csv, row.names = FALSE)
  cat("Saved edges to:", out_csv, "\n")
}

# Sanity checks
stopifnot(is.data.frame(edge_df))
stopifnot(identical(colnames(edge_df), c("from","to","weight")))
stopifnot(ncol(edge_df) == 3)
if (nrow(idx) != nrow(edge_df)) {
  warning("Row mismatch between idx and edge_df: ", nrow(idx), " vs ", nrow(edge_df))
}

cat("Done.\n")
