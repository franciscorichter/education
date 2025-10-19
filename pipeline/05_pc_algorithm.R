#!/usr/bin/env Rscript
# ============================================================================== 
# Pipeline Step 05: PC Algorithm for Directed Causal Graphs
# ============================================================================== 
# Rebuilds the causal discovery stage using the PC (Peter-Clark) algorithm.
# The script prepares the data, runs the PC routine, attempts to orient the
# resulting CPDAG into a DAG when possible, and exports edge tables together
# with diagnostic figures. Heavy computations (e.g., bootstrapping) can be
# toggled off so the script is ready to run later without modification.
#
# NOTE: This script is intentionally not executed by the assistant. Review the
# configuration section and run it manually when appropriate.
#
# Reference: Spirtes, Glymour, & Scheines (2000). Causation, Prediction, and
# Search. MIT Press.
# ============================================================================== 

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(rlang)
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
pc_dir <- file.path(output_dir, "pc_algorithm")
fig_dir <- file.path(pc_dir, "figures")
preprocessed_dir <- file.path(output_dir, "preprocessed")

dir.create(pc_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

ALPHA <- 0.01          # Significance level for conditional independence tests
MAX_COND_SET <- {
  val <- suppressWarnings(as.integer(Sys.getenv("PC_MAX_COND_SET", NA_character_)))
  if (is.na(val) || val < 0) 3L else val
}                       # Maximum size of conditioning sets (m.max)
MAX_VARIABLES <- {
  val <- suppressWarnings(as.integer(Sys.getenv("PC_MAX_VARIABLES", NA_character_)))
  if (is.na(val) || val <= 0) 40L else val
}                       # Limit variables to manage computational cost
INCLUDE_VARS <- c("academic_lang", "academic_math", "socioemotional_learning")
EXCLUDE_PATTERN <- "^id|^peso|^weight|^factor"

RUN_BOOTSTRAP <- identical(tolower(Sys.getenv("PC_RUN_BOOTSTRAP", "false")), "true")
N_BOOT <- {
  val <- suppressWarnings(as.integer(Sys.getenv("PC_BOOT_SAMPLES", NA_character_)))
  if (is.na(val) || val <= 0) 30L else val
}           # Number of bootstrap resamples if RUN_BOOTSTRAP = TRUE
BOOT_SEED <- {
  val <- suppressWarnings(as.integer(Sys.getenv("PC_BOOT_SEED", NA_character_)))
  if (is.na(val)) 123L else val
}

QUESTIONNAIRE_KEY <- Sys.getenv("PC_QUESTIONNAIRE", "estudiante")
QUESTIONNAIRE_ID_COL <- Sys.getenv("PC_QUESTIONNAIRE_ID", "id_estudiante")

message_line(rule())
message_line("PIPELINE 05 - PC ALGORITHM (CAUSAL DISCOVERY)")
message_line(rule())
message_line("\nConfiguration:")
message_line("  - Alpha: ", ALPHA)
message_line("  - Max conditioning set: ", MAX_COND_SET)
message_line("  - Max variables: ", MAX_VARIABLES)
message_line("  - Bootstrap enabled: ", RUN_BOOTSTRAP)
if (RUN_BOOTSTRAP) message_line("  - Bootstrap samples: ", N_BOOT)
message_line("  - Questionnaire: ", QUESTIONNAIRE_KEY)

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------
load_analysis_data <- function(source) {
  obj <- if (is.character(source)) {
    if (!file.exists(source)) {
      stop("Preprocessed data not found. Run 02_preprocess.R first.")
    }
    readRDS(source)
  } else {
    source
  }
  if (!is.list(obj) || is.null(obj$analysis)) {
    stop("Preprocessed object does not contain an `analysis` component.")
  }
  obj$analysis
}

load_questionnaire_data <- function(obj, key) {
  if (!is.list(obj) || is.null(obj$raw)) {
    stop("Preprocessed object does not contain raw questionnaire data.")
  }
  if (!key %in% names(obj$raw)) {
    stop("Questionnaire key '", key, "' not found in preprocessed object.")
  }
  obj$raw[[key]]
}

select_numeric_variables <- function(df, include = character(), exclude_regex = NULL, max_vars = Inf) {
  numeric_df <- df %>% select(where(is.numeric))
  if (!is.null(exclude_regex)) {
    numeric_df <- numeric_df %>% select(-matches(exclude_regex, ignore.case = TRUE))
  }
  available <- names(numeric_df)
  keep <- union(intersect(include, available), available)
  if (length(keep) > max_vars) {
    variances <- numeric_df %>%
      summarise(across(all_of(keep), ~sd(., na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "sd") %>%
      arrange(desc(sd)) %>%
      slice_head(n = max_vars) %>%
      pull(variable)
    keep <- unique(c(intersect(include, keep), variances))
  }
  numeric_df %>% select(all_of(keep))
}

prepare_questionnaire_dataset <- function(questionnaire_df, analysis_df, id_col, include_vars) {
  if (!id_col %in% names(questionnaire_df)) {
    stop("ID column '", id_col, "' not present in questionnaire data.")
  }

  numeric_cols <- questionnaire_df %>%
    select(any_of(id_col), where(is.numeric)) %>%
    distinct(.data[[id_col]], .keep_all = TRUE)

  if (id_col != "id_estudiante") {
    numeric_cols <- numeric_cols %>% rename(id_estudiante = !!sym(id_col))
  }

  analysis_subset <- analysis_df %>%
    select(id_estudiante, all_of(include_vars))

  combined <- numeric_cols %>%
    inner_join(analysis_subset, by = "id_estudiante")

  missing_vars <- setdiff(include_vars, names(combined))
  if (length(missing_vars) > 0) {
    stop("Following required variables are missing after join: ", paste(missing_vars, collapse = ", "))
  }

  combined %>% select(-id_estudiante)
}

categorize_questionnaire_columns <- function(cols) {
  question_pattern <- "^p\\d{2}(?:_\\d+)?$"
  latent_pattern <- "^est"

  questions <- cols[str_detect(cols, regex(question_pattern, ignore_case = TRUE))]
  latent <- cols[str_detect(cols, regex(latent_pattern, ignore_case = TRUE))]
  list(
    questions = questions,
    latent = latent,
    other = setdiff(cols, union(questions, latent))
  )
}

clean_variable_label <- function(name) {
  label <- name %>%
    str_replace_all("_", " ") %>%
    str_replace_all("(?<=[A-Za-z])(?=\\d)", " ") %>%
    str_replace_all("(?<=\\d)(?=[A-Za-z])", " ") %>%
    str_squish()
  str_to_title(label)
}

build_pc_dataset <- function(df, include_vars, candidate_cols, max_vars, exclude_regex) {
  candidates <- intersect(candidate_cols, names(df))
  selected <- df %>% select(any_of(c(include_vars, candidates)))
  selected <- select_numeric_variables(selected, include = include_vars, exclude_regex = exclude_regex, max_vars = max_vars)
  list(
    data = selected %>% drop_na(),
    columns = colnames(selected)
  )
}

standardize_matrix <- function(df) {
  mat <- scale(df)
  colnames(mat) <- colnames(df)
  mat
}

compute_sufficient_statistics <- function(mat) {
  list(C = cor(mat), n = nrow(mat))
}

run_pc <- function(suff_stat, labels, alpha, m_max) {
  pc(suffStat = suff_stat,
     indepTest = gaussCItest,
     alpha = alpha,
     labels = labels,
     m.max = m_max,
     skel.method = "stable",
     conservative = FALSE,
     maj.rule = FALSE,
     solve.confl = FALSE,
     verbose = FALSE)
}

orient_to_dag <- function(graphNEL) {
  tryCatch({
    res <- pdag2dag(graphNEL)
    if (isTRUE(res$success)) res$graph else NULL
  }, error = function(e) NULL)
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

build_igraph <- function(edge_tbl, vertices, label_map = NULL) {
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
  if (!is.null(label_map)) {
    resolved <- label_map[vertices]
    resolved[is.na(resolved)] <- vertices[is.na(resolved)]
    g <- set_vertex_attr(g, "display_label", value = resolved)
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
  labels <- vertex_attr(graph_obj, "display_label")
  if (is.null(labels)) labels <- V(graph_obj)$name
  png(filename = path_png, width = 12, height = 12, units = "in", res = 150)
  plot(graph_obj,
       vertex.size = scales::rescale(node_stats$degree, to = c(10, 30)),
       vertex.label.cex = 0.6,
       vertex.color = ifelse(V(graph_obj)$name %in% highlight, "#F94144", "#90CAF9"),
       vertex.label = labels,
       edge.arrow.size = 0.25,
       edge.color = "#555555",
       layout = layout_with_fr(graph_obj),
       main = "PC Algorithm: Directed Causal Graph")
  dev.off()
}

plot_ego_network <- function(graph_obj, node, path_png) {
  ego_graph <- make_ego_graph(graph_obj, order = 1, nodes = node)[[1]]
  labels <- vertex_attr(ego_graph, "display_label")
  if (is.null(labels)) labels <- V(ego_graph)$name
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

bootstrap_edge_stability <- function(data_mat, alpha, m_max, edge_tbl, n_boot, seed) {
  set.seed(seed)
  labels <- colnames(data_mat)
  res <- vector("list", n_boot)
  for (i in seq_len(n_boot)) {
    boot_idx <- sample(seq_len(nrow(data_mat)), replace = TRUE)
    boot_data <- data_mat[boot_idx, , drop = FALSE]
    boot_stat <- compute_sufficient_statistics(boot_data)
    boot_fit <- tryCatch(run_pc(boot_stat, labels, alpha, m_max), error = function(e) NULL)
    if (!is.null(boot_fit)) {
      res[[i]] <- extract_edges(boot_fit@graph)
    }
    if (i %% 10 == 0) message_line("    • Completed bootstrap iteration ", i, "/", n_boot)
  }
  edge_tbl %>%
    mutate(stability = map_dbl(seq_len(n()), function(idx) {
      edge <- edge_tbl[idx, ]
      mean(map_lgl(res, function(edge_set) {
        if (is.null(edge_set)) return(FALSE)
        any(edge_set$from == edge$from & edge_set$to == edge$to & edge_set$direction == edge$direction)
      }), na.rm = TRUE)
    }))
}

# ------------------------------------------------------------------------------
# Pipeline (not executed by assistant)
# ------------------------------------------------------------------------------
message_line("\nLoading preprocessed dataset...")
preprocessed_path <- file.path(preprocessed_dir, "enla_preprocessed.rds")
preprocessed_obj <- readRDS(preprocessed_path)
analysis_data <- load_analysis_data(preprocessed_obj)
message_line("  - Loaded: ", nrow(analysis_data), " observations × ", ncol(analysis_data), " variables")

questionnaire_data <- load_questionnaire_data(preprocessed_obj, QUESTIONNAIRE_KEY)

combined_df <- prepare_questionnaire_dataset(
  questionnaire_df = questionnaire_data,
  analysis_df = analysis_data,
  id_col = QUESTIONNAIRE_ID_COL,
  include_vars = INCLUDE_VARS
)

column_groups <- categorize_questionnaire_columns(setdiff(names(combined_df), INCLUDE_VARS))
network_definitions <- list(
  questions = column_groups$questions,
  latent = column_groups$latent
)

pc_inputs <- imap(network_definitions, function(cols, key) {
  dataset_info <- build_pc_dataset(
    df = combined_df,
    include_vars = INCLUDE_VARS,
    candidate_cols = cols,
    max_vars = MAX_VARIABLES,
    exclude_regex = EXCLUDE_PATTERN
  )
  data_matrix <- standardize_matrix(dataset_info$data)
  list(
    name = key,
    columns = dataset_info$columns,
    data = dataset_info$data,
    matrix = data_matrix,
    suff_stat = compute_sufficient_statistics(data_matrix),
    label_map = setNames(map_chr(dataset_info$columns, clean_variable_label), dataset_info$columns)
  )
})

walk(pc_inputs, function(input) {
  message_line("  - Network '", input$name, "': ", nrow(input$data), " rows × ", length(input$columns), " variables")
})

message_line("\nPC algorithm configured; two network variants prepared (questions vs latent constructs).")
message_line("When executing, the script will for each network:")
message_line("  1. Fit the PC algorithm and attempt DAG orientation.")
message_line("  2. Export edge tables to `outputs/pc_algorithm/` with network-specific suffixes.")
message_line("  3. Produce network diagnostics in `outputs/pc_algorithm/figures/`.")
if (RUN_BOOTSTRAP) message_line("  4. Estimate edge stability via bootstrapping (slow).")

message_line("\nExecution outline (for manual run):")
message_line("--------------------------------------------------")
message_line("pc_input <- pc_inputs[['questions']]  # or 'latent'")
message_line("pc_fit <- run_pc(pc_input$suff_stat, colnames(pc_input$matrix), ALPHA, MAX_COND_SET)")
message_line("dag_graph <- orient_to_dag(pc_fit@graph)")
message_line("graph_used <- if (!is.null(dag_graph)) dag_graph else pc_fit@graph")
message_line("edge_table <- extract_edges(graph_used)")
message_line("parents <- extract_target_parents(edge_table, c('academic_lang','academic_math','socioemotional_learning'))")
message_line("igraph_obj <- build_igraph(edge_table, colnames(pc_input$matrix), pc_input$label_map)")
message_line("node_stats <- compute_node_statistics(igraph_obj)")
message_line("plot_full_network(igraph_obj, node_stats, INCLUDE_VARS, file.path(fig_dir, paste0('pc_full_network_', pc_input$name, '.png')))")
message_line("for (driver in INCLUDE_VARS) if (driver %in% V(igraph_obj)$name) plot_ego_network(igraph_obj, driver, file.path(fig_dir, paste0('pc_', pc_input$name, '_', driver, '_neighborhood.png')))")
message_line("plot_degree_distribution(node_stats, file.path(fig_dir, paste0('pc_degree_distribution_', pc_input$name, '.png')))")
if (RUN_BOOTSTRAP) {
  message_line("edge_stability <- bootstrap_edge_stability(pc_input$matrix, ALPHA, MAX_COND_SET, edge_table, N_BOOT, BOOT_SEED)")
  message_line("write_csv(edge_stability, file.path(pc_dir, paste0('pc_edge_stability_', pc_input$name, '.csv')))")
}
message_line("write_csv(edge_table, file.path(pc_dir, paste0('pc_edges_', pc_input$name, '.csv')))")
message_line("write_csv(parents, file.path(pc_dir, paste0('pc_parents_', pc_input$name, '.csv')))")
message_line("write_csv(node_stats, file.path(pc_dir, paste0('pc_node_statistics_', pc_input$name, '.csv')))")
message_line("--------------------------------------------------")

message_line("\nNo computations were performed. The script is ready for manual execution.")
