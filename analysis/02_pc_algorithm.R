#!/usr/bin/env Rscript
# ==============================================================================
# PC Algorithm for Causal Discovery in Educational Data
# ==============================================================================
# This script applies the PC (Peter-Clark) algorithm to discover causal
# relationships among educational variables. The PC algorithm uses conditional
# independence tests to construct a directed acyclic graph (DAG).
#
# Reference: Spirtes, P., Glymour, C., & Scheines, R. (2000). 
#            Causation, Prediction, and Search. MIT Press.
#
# Author: Education Analysis Team
# Date: 2024
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(pcalg)      # PC algorithm implementation
  library(graph)      # Graph manipulation
  library(Rgraphviz)  # Graph visualization
  library(ggplot2)
  library(igraph)     # Alternative graph tools
})

# ==============================================================================
# Configuration
# ==============================================================================

ROOT_DIR <- here::here()
OUTPUT_DIR <- file.path(ROOT_DIR, "outputs")
PREPROC_DIR <- file.path(OUTPUT_DIR, "preprocessed")
PC_DIR <- file.path(OUTPUT_DIR, "pc_algorithm")
FIG_DIR <- file.path(PC_DIR, "figures")

# Create directories
dir.create(PC_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# PC algorithm parameters
ALPHA <- 0.01  # Significance level for independence tests (conservative)
MAX_COND_SET <- 3  # Maximum size of conditioning sets

message("="*80)
message("PC Algorithm for Causal Discovery")
message("="*80)
message("\nParameters:")
message("  - Significance level (alpha): ", ALPHA)
message("  - Max conditioning set size: ", MAX_COND_SET)

# ==============================================================================
# Load Preprocessed Data
# ==============================================================================

message("\n[1/7] Loading preprocessed data...")
data_file <- file.path(PREPROC_DIR, "enla_clean.rds")
if (!file.exists(data_file)) {
  stop("Preprocessed data not found. Run 00_data_preprocessing.R first.")
}

obj <- readRDS(data_file)
em_analysis <- obj$analysis_data

message("  - Loaded ", nrow(em_analysis), " observations")

# ==============================================================================
# Prepare Data for PC Algorithm
# ==============================================================================

message("\n[2/7] Preparing data for PC algorithm...")

# Select key variables for causal analysis
# Focus on variables most relevant to academic performance
key_vars <- c("medida500_M", "medida500_L")

# Add other numeric variables (limit to avoid computational burden)
numeric_vars <- em_analysis %>%
  select_if(is.numeric) %>%
  select(-matches("^id|^ID|^peso|^weight|^factor|^estrato", ignore.case = TRUE)) %>%
  names()

# Select top variables by correlation with performance
if (length(numeric_vars) > 35) {
  cor_with_perf <- cor(em_analysis[numeric_vars], 
                       em_analysis[c("medida500_M", "medida500_L")],
                       use = "pairwise.complete.obs")
  
  top_vars <- apply(abs(cor_with_perf), 1, max) %>%
    sort(decreasing = TRUE) %>%
    head(35) %>%
    names()
  
  numeric_vars <- unique(c(key_vars, top_vars))
  message("  - Selected top 35 variables by correlation with performance")
}

# Create analysis matrix with complete cases
pc_data <- em_analysis %>%
  select(all_of(numeric_vars)) %>%
  na.omit()

message("  - Analysis matrix: ", nrow(pc_data), " observations x ", 
        ncol(pc_data), " variables")
message("  - Removed ", nrow(em_analysis) - nrow(pc_data), 
        " rows with missing values")

# Standardize variables (important for PC algorithm)
pc_data_std <- scale(pc_data)
colnames(pc_data_std) <- colnames(pc_data)

# ==============================================================================
# Compute Correlation Matrix
# ==============================================================================

message("\n[3/7] Computing correlation matrix...")

# Correlation matrix (used for Gaussian tests)
cor_matrix <- cor(pc_data_std)

# Check for perfect correlations (can cause issues)
perfect_cor <- which(abs(cor_matrix) > 0.999 & cor_matrix != 1, arr.ind = TRUE)
if (nrow(perfect_cor) > 0) {
  message("  WARNING: Found ", nrow(perfect_cor)/2, " pairs with near-perfect correlation")
}

# ==============================================================================
# Run PC Algorithm
# ==============================================================================

message("\n[4/7] Running PC algorithm...")
message("  This may take several minutes...")

# Define sufficient statistic (correlation matrix + sample size)
suffStat <- list(C = cor_matrix, n = nrow(pc_data_std))

# Run PC algorithm
set.seed(42)  # For reproducibility
start_time <- Sys.time()

pc_fit <- tryCatch({
  pc(suffStat = suffStat,
     indepTest = gaussCItest,  # Gaussian conditional independence test
     alpha = ALPHA,
     labels = colnames(pc_data_std),
     fixedGaps = NULL,
     fixedEdges = NULL,
     NAdelete = TRUE,
     m.max = MAX_COND_SET,
     u2pd = "relaxed",  # Convert CPDAG to DAG
     skel.method = "stable",
     conservative = FALSE,
     maj.rule = FALSE,
     solve.confl = FALSE,
     verbose = FALSE)
}, error = function(e) {
  message("  ERROR in PC algorithm: ", e$message)
  return(NULL)
})

end_time <- Sys.time()
runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

if (is.null(pc_fit)) {
  stop("PC algorithm failed. Check data quality and parameters.")
}

message("  - Completed in ", round(runtime, 2), " seconds")
message("  - Number of edges: ", sum(as(pc_fit@graph, "matrix") != 0) / 2)

# ==============================================================================
# Extract and Analyze Results
# ==============================================================================

message("\n[5/7] Extracting causal structure...")

# Get adjacency matrix
adj_matrix <- as(pc_fit@graph, "matrix")

# Convert to edge list
edges <- which(adj_matrix != 0, arr.ind = TRUE)
edge_list <- tibble(
  from = colnames(pc_data_std)[edges[, 1]],
  to = colnames(pc_data_std)[edges[, 2]],
  edge_type = adj_matrix[edges]
) %>%
  filter(from < to)  # Remove duplicates (undirected edges)

# Classify edges
edge_list <- edge_list %>%
  mutate(
    direction = case_when(
      edge_type == 1 ~ "directed",
      edge_type == 2 ~ "undirected",
      TRUE ~ "unknown"
    )
  )

message("  - Total edges: ", nrow(edge_list))
message("  - Directed edges: ", sum(edge_list$direction == "directed"))
message("  - Undirected edges: ", sum(edge_list$direction == "undirected"))

# Save edge list
write_csv(edge_list, file.path(PC_DIR, "pc_edges.csv"))

# Identify parents of performance variables
perf_parents_math <- edge_list %>%
  filter(to == "medida500_M" | from == "medida500_M") %>%
  mutate(parent = ifelse(to == "medida500_M", from, to))

perf_parents_lang <- edge_list %>%
  filter(to == "medida500_L" | from == "medida500_L") %>%
  mutate(parent = ifelse(to == "medida500_L", from, to))

message("\n  Variables connected to Math performance: ", nrow(perf_parents_math))
message("  Variables connected to Language performance: ", nrow(perf_parents_lang))

# ==============================================================================
# Visualize Causal Graph
# ==============================================================================

message("\n[6/7] Generating visualizations...")

# Convert to igraph for better visualization
ig <- graph_from_data_frame(
  d = edge_list %>% select(from, to),
  directed = FALSE,
  vertices = colnames(pc_data_std)
)

# Calculate node importance (degree centrality)
node_degree <- degree(ig)
node_betweenness <- betweenness(ig)

node_stats <- tibble(
  variable = names(node_degree),
  degree = node_degree,
  betweenness = node_betweenness
) %>%
  arrange(desc(degree))

write_csv(node_stats, file.path(PC_DIR, "node_statistics.csv"))

message("  Top 10 most connected variables:")
print(head(node_stats, 10))

# Plot 1: Full network
png(file.path(FIG_DIR, "pc_full_network.png"), 
    width = 12, height = 12, units = "in", res = 150)
set.seed(42)
plot(ig,
     vertex.size = sqrt(node_degree) * 3,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.color = ifelse(names(node_degree) %in% c("medida500_M", "medida500_L"),
                           "red", "lightblue"),
     edge.width = 1.5,
     edge.color = "gray50",
     layout = layout_with_fr(ig),
     main = "PC Algorithm: Full Causal Network")
dev.off()
message("  - Saved pc_full_network.png")

# Plot 2: Ego network around performance variables
ego_math <- make_ego_graph(ig, order = 1, nodes = "medida500_M")[[1]]
ego_lang <- make_ego_graph(ig, order = 1, nodes = "medida500_L")[[1]]

png(file.path(FIG_DIR, "pc_math_neighborhood.png"), 
    width = 10, height = 10, units = "in", res = 150)
plot(ego_math,
     vertex.size = 20,
     vertex.label.cex = 0.8,
     vertex.color = ifelse(V(ego_math)$name == "medida500_M", "red", "lightblue"),
     edge.width = 2,
     main = "Variables Connected to Math Performance")
dev.off()
message("  - Saved pc_math_neighborhood.png")

png(file.path(FIG_DIR, "pc_language_neighborhood.png"), 
    width = 10, height = 10, units = "in", res = 150)
plot(ego_lang,
     vertex.size = 20,
     vertex.label.cex = 0.8,
     vertex.color = ifelse(V(ego_lang)$name == "medida500_L", "red", "lightgreen"),
     edge.width = 2,
     main = "Variables Connected to Language Performance")
dev.off()
message("  - Saved pc_language_neighborhood.png")

# Plot 3: Node degree distribution
p <- ggplot(node_stats, aes(x = degree)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Node Degrees in Causal Network",
       x = "Degree (Number of Connections)",
       y = "Count") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(FIG_DIR, "pc_degree_distribution.png"), p, 
       width = 8, height = 6, dpi = 150)
message("  - Saved pc_degree_distribution.png")

# ==============================================================================
# Stability Analysis (Bootstrap)
# ==============================================================================

message("\n[7/7] Running stability analysis (bootstrap)...")
message("  This may take several minutes...")

N_BOOT <- 50  # Number of bootstrap samples
boot_edges <- vector("list", N_BOOT)

set.seed(123)
for (i in 1:N_BOOT) {
  if (i %% 10 == 0) message("  - Bootstrap iteration ", i, "/", N_BOOT)
  
  # Bootstrap sample
  boot_idx <- sample(1:nrow(pc_data_std), replace = TRUE)
  boot_data <- pc_data_std[boot_idx, ]
  boot_cor <- cor(boot_data)
  boot_suffStat <- list(C = boot_cor, n = nrow(boot_data))
  
  # Run PC
  boot_fit <- tryCatch({
    pc(suffStat = boot_suffStat,
       indepTest = gaussCItest,
       alpha = ALPHA,
       labels = colnames(boot_data),
       m.max = MAX_COND_SET,
       verbose = FALSE)
  }, error = function(e) NULL)
  
  if (!is.null(boot_fit)) {
    boot_adj <- as(boot_fit@graph, "matrix")
    boot_edges[[i]] <- which(boot_adj != 0, arr.ind = TRUE)
  }
}

# Calculate edge stability (proportion of bootstrap samples containing each edge)
edge_stability <- edge_list %>%
  rowwise() %>%
  mutate(
    stability = mean(sapply(boot_edges, function(be) {
      if (is.null(be)) return(FALSE)
      from_idx <- which(colnames(pc_data_std) == from)
      to_idx <- which(colnames(pc_data_std) == to)
      any(be[, 1] == from_idx & be[, 2] == to_idx) | 
        any(be[, 1] == to_idx & be[, 2] == from_idx)
    }))
  ) %>%
  ungroup() %>%
  arrange(desc(stability))

write_csv(edge_stability, file.path(PC_DIR, "edge_stability.csv"))

message("\n  Edge stability summary:")
message("    - Highly stable edges (>80%): ", sum(edge_stability$stability > 0.8))
message("    - Moderately stable edges (50-80%): ", 
        sum(edge_stability$stability > 0.5 & edge_stability$stability <= 0.8))
message("    - Unstable edges (<50%): ", sum(edge_stability$stability <= 0.5))

# ==============================================================================
# Summary Report
# ==============================================================================

message("\n" %+% "="*80)
message("PC Algorithm Analysis Complete!")
message("="*80)
message("\nGenerated outputs:")
message("  - Edge list: ", file.path(PC_DIR, "pc_edges.csv"))
message("  - Node statistics: ", file.path(PC_DIR, "node_statistics.csv"))
message("  - Edge stability: ", file.path(PC_DIR, "edge_stability.csv"))
message("  - Figures: ", FIG_DIR)
message("\nKey findings:")
message("  - Total edges discovered: ", nrow(edge_list))
message("  - Most connected variable: ", node_stats$variable[1], 
        " (degree = ", node_stats$degree[1], ")")
message("  - Variables connected to Math: ", nrow(perf_parents_math))
message("  - Variables connected to Language: ", nrow(perf_parents_lang))

message("\n✓ Ready for next analysis step!")
message("  Run 03_graphical_lasso.R for network estimation")
