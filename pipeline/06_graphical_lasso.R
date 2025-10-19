#!/usr/bin/env Rscript
# ==============================================================================
# Pipeline Step 06: Graphical Lasso for Network Estimation
# ==============================================================================
# Estimates a sparse precision matrix using Graphical Lasso to identify
# conditional independence relationships in the educational data.
#
# Reference: Friedman, J., Hastie, T., & Tibshirani, R. (2008).
#            Sparse inverse covariance estimation with the graphical lasso.
#            Biostatistics, 9(3), 432-441.
#
# Input: Preprocessed data from Step 02
# Output: Sparse network structure, edge weights, and community analysis
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(glasso)     # Graphical Lasso implementation
  library(huge)       # High-dimensional undirected graph estimation
  library(qgraph)     # Network visualization
  library(igraph)     # Graph analysis
  library(ggplot2)
  library(gridExtra)
  library(corrplot)
})

message_line <- function(...) cat(paste0(..., "\n"))
rule <- function(char = "=") paste0(rep(char, 80), collapse = "")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
output_dir <- "outputs"
glasso_dir <- file.path(output_dir, "graphical_lasso")
fig_dir <- file.path(glasso_dir, "figures")
preprocessed_dir <- file.path(output_dir, "preprocessed")

dir.create(glasso_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# GLasso parameters
GAMMA <- 0.5  # EBIC gamma parameter (0 = BIC, 0.5 = balanced, 1 = conservative)

message_line(rule())
message_line("PIPELINE 06 - GRAPHICAL LASSO (NETWORK ESTIMATION)")
message_line(rule())
message_line("\nParameters:")
message_line("  - EBIC gamma: ", GAMMA)

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
# Prepare data for GLasso
# ------------------------------------------------------------------------------
message_line("\nPreparing data for GLasso...")

# Select numeric variables
numeric_vars <- analysis_data %>%
  select_if(is.numeric) %>%
  select(-matches("^id|^peso|^weight|^factor", ignore.case = TRUE)) %>%
  names()

# Limit to manageable number (GLasso can handle more variables than PC)
if (length(numeric_vars) > 50) {
  # Select top variables by correlation with performance measures
  perf_vars <- c("medida500_M", "medida500_L")
  cor_with_perf <- cor(analysis_data[numeric_vars],
                       analysis_data[perf_vars],
                       use = "pairwise.complete.obs")

  var_importance <- apply(abs(cor_with_perf), 1, max) %>%
    sort(decreasing = TRUE) %>%
    head(50) %>%
    names()

  numeric_vars <- unique(c(perf_vars, var_importance))
  message_line("  - Selected top 50 variables by correlation with performance")
}

# Create analysis matrix with complete cases
glasso_data <- analysis_data %>%
  select(all_of(numeric_vars)) %>%
  na.omit()

message_line("  - GLasso dataset: ", nrow(glasso_data), " observations × ", ncol(glasso_data), " variables")

# Standardize variables
glasso_data_std <- scale(glasso_data)
colnames(glasso_data_std) <- colnames(glasso_data)

# ------------------------------------------------------------------------------
# Compute sample covariance matrix
# ------------------------------------------------------------------------------
message_line("\nComputing sample covariance matrix...")

S <- cov(glasso_data_std)
n <- nrow(glasso_data_std)
p <- ncol(glasso_data_std)

message_line("  - Sample size (n): ", n)
message_line("  - Number of variables (p): ", p)
message_line("  - Condition number: ", round(kappa(S), 2))

# ------------------------------------------------------------------------------
# Select optimal regularization parameter (lambda)
# ------------------------------------------------------------------------------
message_line("\nSelecting optimal lambda using EBIC...")
message_line("  This may take several minutes...")

# Use huge package for model selection
set.seed(42)
huge_fit <- huge(glasso_data_std,
                 method = "glasso",
                 nlambda = 50,
                 lambda.min.ratio = 0.01,
                 verbose = FALSE)

# Select lambda using EBIC
huge_select <- huge.select(huge_fit,
                           criterion = "ebic",
                           ebic.gamma = GAMMA,
                           verbose = FALSE)

lambda_opt <- huge_select$opt.lambda
message_line("  - Optimal lambda: ", round(lambda_opt, 4))
message_line("  - Number of edges: ", sum(huge_select$refit != 0) / 2)

# ------------------------------------------------------------------------------
# Fit Graphical Lasso with optimal lambda
# ------------------------------------------------------------------------------
message_line("\nFitting Graphical Lasso with optimal lambda...")

# Fit glasso
glasso_fit <- glasso(S, rho = lambda_opt, penalize.diagonal = FALSE)

# Extract precision matrix (inverse covariance)
Theta <- glasso_fit$wi
rownames(Theta) <- colnames(Theta) <- colnames(glasso_data_std)

# Extract partial correlation matrix
partial_cor <- -cov2cor(Theta)
diag(partial_cor) <- 1

message_line("  - Sparsity: ", round(sum(Theta == 0) / length(Theta) * 100, 2), "%")
message_line("  - Non-zero elements: ", sum(Theta != 0))

# ------------------------------------------------------------------------------
# Extract network structure
# ------------------------------------------------------------------------------
message_line("\nExtracting network structure...")

# Create adjacency matrix (threshold small values)
threshold <- 1e-6
adj_matrix <- (abs(Theta) > threshold) * 1
diag(adj_matrix) <- 0

# Convert to edge list with weights
edges <- which(adj_matrix != 0 & upper.tri(adj_matrix), arr.ind = TRUE)
edge_list <- data.frame(
  from = colnames(glasso_data_std)[edges[, 1]],
  to = colnames(glasso_data_std)[edges[, 2]],
  precision = Theta[edges],
  partial_cor = partial_cor[edges],
  abs_partial_cor = abs(partial_cor[edges])
) %>%
  arrange(desc(abs_partial_cor))

message_line("  - Total edges: ", nrow(edge_list))
message_line("  - Mean |partial correlation|: ", round(mean(edge_list$abs_partial_cor), 3))

# Save edge list
write_csv(edge_list, file.path(glasso_dir, "glasso_edges.csv"))

# ------------------------------------------------------------------------------
# Identify neighbors of performance variables
# ------------------------------------------------------------------------------
message_line("\nIdentifying performance variable neighbors...")

perf_vars <- c("medida500_M", "medida500_L")

for (perf_var in perf_vars) {
  neighbors <- edge_list %>%
    filter(from == perf_var | to == perf_var) %>%
    mutate(neighbor = ifelse(from == perf_var, to, from)) %>%
    arrange(desc(abs_partial_cor))

  if (nrow(neighbors) > 0) {
    write_csv(neighbors, file.path(glasso_dir, paste0(perf_var, "_neighbors.csv")))
    message_line("  ", perf_var, " connected to ", nrow(neighbors), " variables:")
    print(head(neighbors %>% select(neighbor, partial_cor), 5))
  }
}

# ------------------------------------------------------------------------------
# Network analysis
# ------------------------------------------------------------------------------
message_line("\nAnalyzing network properties...")

# Convert to igraph
ig <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected",
                                   diag = FALSE, weighted = NULL)

# Calculate centrality measures
node_stats <- data.frame(
  variable = colnames(glasso_data_std),
  degree = degree(ig),
  betweenness = betweenness(ig),
  closeness = closeness(ig),
  eigenvector = eigen_centrality(ig)$vector,
  stringsAsFactors = FALSE
) %>%
  arrange(desc(degree))

write_csv(node_stats, file.path(glasso_dir, "node_centrality.csv"))

message_line("  Top 10 most central variables (by degree):")
print(head(node_stats %>% select(variable, degree, betweenness), 10))

# ------------------------------------------------------------------------------
# Community detection
# ------------------------------------------------------------------------------
message_line("\nDetecting network communities...")

# Use Louvain algorithm for community detection
communities <- cluster_louvain(ig)
node_stats$community <- membership(communities)

message_line("  - Number of communities: ", max(node_stats$community))
message_line("  - Modularity: ", round(modularity(communities), 3))

# Save community assignments
write_csv(node_stats %>% select(variable, community),
          file.path(glasso_dir, "communities.csv"))

# ------------------------------------------------------------------------------
# Generate visualizations
# ------------------------------------------------------------------------------
message_line("\nGenerating visualizations...")

# 1. Partial correlation heatmap
png(file.path(fig_dir, "glasso_partial_correlations.png"),
    width = 12, height = 12, units = "in", res = 150)
corrplot(partial_cor, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, tl.cex = 0.7,
         title = "Partial Correlation Network (GLasso)",
         mar = c(0, 0, 2, 0),
         is.corr = TRUE)
dev.off()
message_line("  ✓ Saved: glasso_partial_correlations.png")

# 2. Network visualization with communities
png(file.path(fig_dir, "glasso_network.png"),
    width = 12, height = 12, units = "in", res = 150)
qgraph(partial_cor,
       layout = "spring",
       groups = as.factor(node_stats$community),
       vsize = 4,
       edge.width = 0.5,
       minimum = 0.05,  # Only show edges with |r| > 0.05
       cut = 0.1,
       maximum = 1,
       title = "Conditional Independence Network (GLasso)",
       legend = TRUE,
       legend.cex = 0.4)
dev.off()
message_line("  ✓ Saved: glasso_network.png")

# 3. Ego networks for performance variables
for (perf_var in perf_vars) {
  if (perf_var %in% V(ig)$name) {
    ego_graph <- make_ego_graph(ig, order = 1, nodes = perf_var)[[1]]

    png(file.path(fig_dir, paste0("glasso_", perf_var, "_ego_network.png")),
        width = 10, height = 10, units = "in", res = 150)
    plot(ego_graph,
         vertex.size = 20,
         vertex.label.cex = 0.8,
         vertex.color = ifelse(V(ego_graph)$name == perf_var, "red", "lightblue"),
         edge.width = 2,
         main = paste(perf_var, "Neighborhood"))
    dev.off()
    message_line("  ✓ Saved: glasso_", perf_var, "_ego_network.png")
  }
}

# 4. Degree distribution
p1 <- ggplot(node_stats, aes(x = degree)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Node Degree Distribution",
       x = "Degree", y = "Count") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 5. Centrality comparison
p2 <- ggplot(node_stats %>% head(20),
             aes(x = reorder(variable, degree), y = degree)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top 20 Variables by Degree Centrality",
       x = NULL, y = "Degree") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p_combined <- grid.arrange(p1, p2, ncol = 2)
ggsave(file.path(fig_dir, "glasso_centrality_analysis.png"), p_combined,
       width = 14, height = 6, dpi = 150)
message_line("  ✓ Saved: glasso_centrality_analysis.png")

# 6. Community structure
community_sizes <- table(node_stats$community)
p3 <- ggplot(data.frame(community = names(community_sizes),
                        size = as.numeric(community_sizes)),
             aes(x = reorder(community, -size), y = size)) +
  geom_col(fill = "coral", alpha = 0.7) +
  labs(title = "Community Sizes",
       x = "Community", y = "Number of Variables") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(fig_dir, "glasso_communities.png"), p3,
       width = 8, height = 6, dpi = 150)
message_line("  ✓ Saved: glasso_communities.png")

# ------------------------------------------------------------------------------
# Compare with simple correlation
# ------------------------------------------------------------------------------
message_line("\nComparing GLasso with simple correlation...")

# Simple correlation
simple_cor <- cor(glasso_data_std)

# Count edges
simple_edges <- sum(abs(simple_cor) > 0.1 & upper.tri(simple_cor))
glasso_edges <- sum(abs(partial_cor) > 0.05 & upper.tri(partial_cor))

message_line("  - Simple correlation edges (|r| > 0.1): ", simple_edges)
message_line("  - GLasso edges (|partial r| > 0.05): ", glasso_edges)
message_line("  - Sparsity gain: ",
             round((1 - glasso_edges/simple_edges) * 100, 1), "%")

# ------------------------------------------------------------------------------
# Summary report
# ------------------------------------------------------------------------------
message_line("")
message_line(rule())
message_line("GRAPHICAL LASSO COMPLETE")
message_line(rule())

message_line("\nGenerated outputs:")
message_line("  - Edge list: ", file.path(glasso_dir, "glasso_edges.csv"))
message_line("  - Node centrality: ", file.path(glasso_dir, "node_centrality.csv"))
message_line("  - Communities: ", file.path(glasso_dir, "communities.csv"))
for (perf_var in perf_vars) {
  message_line("  - ", perf_var, " neighbors: ", file.path(glasso_dir, paste0(perf_var, "_neighbors.csv")))
}
message_line("  - Figures: ", fig_dir)

figs <- list.files(fig_dir, pattern = "\\.png$", full.names = FALSE)
message_line("  - Generated ", length(figs), " figures")

message_line("\nKey findings:")
message_line("  - Optimal lambda: ", round(lambda_opt, 4))
message_line("  - Network sparsity: ", round(sum(Theta == 0) / length(Theta) * 100, 2), "%")
message_line("  - Total edges: ", nrow(edge_list))
message_line("  - Number of communities: ", max(node_stats$community))
message_line("  - Most central variable: ", node_stats$variable[1],
             " (degree = ", node_stats$degree[1], ")")

message_line("\n✓ All pipeline steps complete!")
message_line("  Ready to integrate results into paper")

message_line("")
message_line("✓ Graphical Lasso pipeline complete!")
