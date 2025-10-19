#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(pcalg)
  library(graph)
  library(igraph)
  library(ggplot2)
})

message_line <- function(...) cat(paste0(..., "\n"))
rule <- function(char = "=") paste0(rep(char, 80), collapse = "")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
output_dir <- "outputs"
fci_dir <- file.path(output_dir, "fci")
fig_dir <- file.path(fci_dir, "figures")
preprocessed_dir <- file.path(output_dir, "preprocessed")

dir.create(fci_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

ALPHA <- {
  val <- suppressWarnings(as.numeric(Sys.getenv("FCI_ALPHA", NA_character_)))
  if (is.na(val)) 0.01 else val
}
MAX_COND_SET <- {
  val <- suppressWarnings(as.integer(Sys.getenv("FCI_MAX_COND_SET", NA_character_)))
  if (is.na(val) || val < 0) 3L else val
}
MAX_VARIABLES <- {
  val <- suppressWarnings(as.integer(Sys.getenv("FCI_MAX_VARIABLES", NA_character_)))
  if (is.na(val) || val <= 0) 40L else val
}
INCLUDE_VARS <- c("academic_lang", "academic_math", "socioemotional_learning")
EXCLUDE_PATTERN <- "^id|^peso|^weight|^factor"

message_line(rule())
message_line("PIPELINE 04 - FCI (FAST CAUSAL INFERENCE)")
message_line(rule())
message_line("\nParameters:")
message_line("  - Alpha: ", ALPHA)
message_line("  - Max conditioning set: ", MAX_COND_SET)
message_line("  - Max variables: ", MAX_VARIABLES)

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------
standardize_matrix <- function(df) {
  mat <- scale(df)
  colnames(mat) <- colnames(df)
  mat
}

compute_sufficient_statistics <- function(mat) {
  list(C = cor(mat), n = nrow(mat))
}

run_fci <- function(suff_stat, labels, alpha, m_max) {
  fci(suffStat = suff_stat,
      indepTest = gaussCItest,
      alpha = alpha,
      labels = labels,
      m.max = m_max,
      skel.method = "stable",
      conservative = FALSE,
      maj.rule = FALSE,
      verbose = FALSE)
}

extract_edges <- function(graphNEL) {
  adj <- as(graphNEL, "matrix")
  labels <- colnames(adj)
  tbl <- as.data.frame(as.table(adj), stringsAsFactors = FALSE)
  names(tbl) <- c("from", "to", "value")
  edge_tbl <- tbl %>%
    filter(value != 0) %>%
    mutate(reverse = adj[cbind(match(to, labels), match(from, labels))],
           direction = if_else(reverse == 0, "directed", "undirected")) %>%
    filter(direction == "directed" | from < to) %>%
    select(from, to, direction)
  edge_tbl
}

extract_target_parents <- function(edge_tbl, targets) {
  edge_tbl %>%
    filter(direction != "undirected", to %in% targets) %>%
    arrange(to, from) %>%
    group_by(to) %>%
    summarise(parents = paste(sort(unique(from)), collapse = ", "), .groups = "drop")
}

build_igraph <- function(edge_tbl, vertices) {
  g <- make_empty_graph(n = length(vertices), directed = TRUE)
  g <- set_vertex_attr(g, "name", value = vertices)

  directed_edges <- edge_tbl %>% filter(direction == "directed") %>% select(from, to)
  if (nrow(directed_edges) > 0) {
    g <- add_edges(g, as.vector(t(as.matrix(directed_edges))))
  }

  undirected_edges <- edge_tbl %>% filter(direction == "undirected") %>% select(from, to)
  if (nrow(undirected_edges) > 0) {
    symmetric_edges <- rbind(as.matrix(undirected_edges), undirected_edges %>% mutate(tmp = from, from = to, to = tmp) %>% select(from, to) %>% as.matrix())
    g <- add_edges(g, as.vector(t(symmetric_edges)))
  }
  g
}

compute_node_statistics <- function(graph_obj) {
  tibble(
    variable = V(graph_obj)$name,
    degree = degree(graph_obj, mode = "all"),
    in_degree = degree(graph_obj, mode = "in"),
    out_degree = degree(graph_obj, mode = "out"),
    betweenness = betweenness(graph_obj, directed = TRUE),
    closeness = closeness(graph_obj, mode = "out", normalized = TRUE)
  ) %>% arrange(desc(degree))
}

plot_full_network <- function(graph_obj, node_stats, highlight, path_png) {
  labels <- V(graph_obj)$name
  png(filename = path_png, width = 12, height = 12, units = "in", res = 150)
  plot(graph_obj,
       vertex.size = scales::rescale(node_stats$degree, to = c(10, 30)),
       vertex.label.cex = 0.6,
       vertex.color = ifelse(V(graph_obj)$name %in% highlight, "#F94144", "#90CAF9"),
       vertex.label = labels,
       edge.arrow.size = 0.25,
       edge.color = "#555555",
       layout = layout_with_fr(graph_obj),
       main = "FCI: Partial Ancestral Graph (PAG)")
  dev.off()
}

plot_ego_network <- function(graph_obj, node, path_png) {
  ego_graph <- make_ego_graph(graph_obj, order = 1, nodes = node)[[1]]
  labels <- V(ego_graph)$name
  png(filename = path_png, width = 9, height = 9, units = "in", res = 150)
  plot(ego_graph,
       vertex.size = 20,
       vertex.label.cex = 0.8,
       vertex.color = ifelse(V(ego_graph)$name == node, "#F94144", "#F9C74F"),
       vertex.label = labels,
       edge.arrow.size = 0.3,
       layout = layout_with_fr(ego_graph),
       main = paste("Neighborhood of", node))
  dev.off()
}

plot_degree_distribution <- function(node_stats, path_png) {
  p <- ggplot(node_stats, aes(x = degree)) +
    geom_histogram(binwidth = 1, fill = "#74C69D", color = "black", alpha = 0.7) +
    labs(title = "Node Degree Distribution",
         x = "Degree",
         y = "Count") +
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  ggsave(path_png, p, width = 8, height = 6, dpi = 150)
}

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
# Prepare data for FCI
# ------------------------------------------------------------------------------
message_line("\nPreparing data for FCI...")

numeric_df <- analysis_data %>%
  select(where(is.numeric)) %>%
  select(-matches(EXCLUDE_PATTERN, ignore.case = TRUE))

present_includes <- intersect(INCLUDE_VARS, names(numeric_df))

if (ncol(numeric_df) > MAX_VARIABLES) {
  sds <- numeric_df %>% summarise(across(everything(), ~sd(., na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "sd") %>%
    arrange(desc(sd))
  top_vars <- head(sds$variable, MAX_VARIABLES)
  keep <- unique(c(present_includes, top_vars))
  numeric_df <- numeric_df %>% select(all_of(keep))
  message_line("  - Reduced variables to ", ncol(numeric_df), " (by SD, keeping key outcomes)")
}

fci_data <- numeric_df %>% drop_na()
message_line("  - FCI dataset: ", nrow(fci_data), " observations × ", ncol(fci_data), " variables")

if (nrow(fci_data) < 50 || ncol(fci_data) < 3) {
  stop("Insufficient data for FCI (need >= 50 rows and >= 3 variables after filtering)")
}

mat <- standardize_matrix(fci_data)
labels <- colnames(mat)
suff_stat <- compute_sufficient_statistics(mat)

# ------------------------------------------------------------------------------
# Run FCI
# ------------------------------------------------------------------------------
message_line("\nRunning FCI (Fast Causal Inference)...")
fci_fit <- run_fci(suff_stat, labels, ALPHA, MAX_COND_SET)

# Extract edges and stats
edge_tbl <- extract_edges(fci_fit@graph)
igraph_obj <- build_igraph(edge_tbl, labels)
node_stats <- compute_node_statistics(igraph_obj)

# Identify parents of key outcomes
parents <- extract_target_parents(edge_tbl, present_includes)

# ------------------------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------------------------
write_csv(edge_tbl, file.path(fci_dir, "fci_edges.csv"))
write_csv(node_stats, file.path(fci_dir, "fci_node_statistics.csv"))
if (nrow(parents) > 0) write_csv(parents, file.path(fci_dir, "fci_parents.csv"))

adj_mat <- as(fci_fit@graph, "matrix")
adj_df <- as.data.frame(adj_mat) %>% rownames_to_column("from")
write_csv(adj_df, file.path(fci_dir, "fci_adjacency_matrix.csv"))

# ------------------------------------------------------------------------------
# Figures
# ------------------------------------------------------------------------------
message_line("\nGenerating FCI network figures...")
plot_full_network(igraph_obj, node_stats, present_includes, file.path(fig_dir, "fci_full_network.png"))
plot_degree_distribution(node_stats, file.path(fig_dir, "fci_degree_distribution.png"))
for (driver in present_includes) if (driver %in% V(igraph_obj)$name) plot_ego_network(igraph_obj, driver, file.path(fig_dir, paste0("fci_", driver, "_neighborhood.png")))

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
message_line("")
message_line(rule())
message_line("FCI ANALYSIS COMPLETE")
message_line(rule())
message_line("\nGenerated outputs:")
message_line("  - Edge list: ", file.path(fci_dir, "fci_edges.csv"))
message_line("  - Node statistics: ", file.path(fci_dir, "fci_node_statistics.csv"))
if (nrow(parents) > 0) message_line("  - Parents of key outcomes: ", file.path(fci_dir, "fci_parents.csv"))
message_line("  - Adjacency matrix: ", file.path(fci_dir, "fci_adjacency_matrix.csv"))
message_line("  - Figures: ", fig_dir)
figs <- list.files(fig_dir, pattern = "\\.png$", full.names = FALSE)
message_line("  - Generated ", length(figs), " figures")
message_line("")
message_line("✓ FCI pipeline complete!")
