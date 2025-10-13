#!/usr/bin/env Rscript
# ==============================================================================
# Graphical Lasso for Sparse Network Estimation
# ==============================================================================
# This script applies the Graphical Lasso (GLasso) to estimate a sparse
# precision matrix (inverse covariance) that encodes conditional independence
# relationships in the educational data.
#
# Reference: Friedman, J., Hastie, T., & Tibshirani, R. (2008). 
#            Sparse inverse covariance estimation with the graphical lasso.
#            Biostatistics, 9(3), 432-441.
#
# Author: Education Analysis Team
# Date: 2024
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

# ==============================================================================
# Configuration
# ==============================================================================

ROOT_DIR <- here::here()
OUTPUT_DIR <- file.path(ROOT_DIR, "outputs")
PREPROC_DIR <- file.path(OUTPUT_DIR, "preprocessed")
GLASSO_DIR <- file.path(OUTPUT_DIR, "graphical_lasso")
FIG_DIR <- file.path(GLASSO_DIR, "figures")

# Create directories
dir.create(GLASSO_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# GLasso parameters
GAMMA <- 0.5  # EBIC gamma parameter (0 = BIC, 0.5 = balanced, 1 = conservative)

message("="*80)
message("Graphical Lasso for Network Estimation")
message("="*80)
message("\nParameters:")
message("  - EBIC gamma: ", GAMMA)

# ==============================================================================
# Load Preprocessed Data
# ==============================================================================

message("\n[1/8] Loading preprocessed data...")
data_file <- file.path(PREPROC_DIR, "enla_clean.rds")
if (!file.exists(data_file)) {
  stop("Preprocessed data not found. Run 00_data_preprocessing.R first.")
}

obj <- readRDS(data_file)
em_analysis <- obj$analysis_data

message("  - Loaded ", nrow(em_analysis), " observations")

# ==============================================================================
# Prepare Data for GLasso
# ==============================================================================

message("\n[2/8] Preparing data for GLasso...")

# Select numeric variables
numeric_vars <- em_analysis %>%
  select_if(is.numeric) %>%
  select(-matches("^id|^ID|^peso|^weight|^factor|^estrato", ignore.case = TRUE)) %>%
  names()

# Limit to manageable number of variables
if (length(numeric_vars) > 40) {
  # Select top variables by variance and correlation with performance
  var_importance <- em_analysis %>%
    select(all_of(numeric_vars)) %>%
    summarise(across(everything(), ~sd(., na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "sd") %>%
    arrange(desc(sd)) %>%
    head(40) %>%
    pull(variable)
  
  numeric_vars <- unique(c("medida500_M", "medida500_L", var_importance))
  message("  - Selected top 40 variables by variance")
}

# Create analysis matrix with complete cases
glasso_data <- em_analysis %>%
  select(all_of(numeric_vars)) %>%
  na.omit()

message("  - Analysis matrix: ", nrow(glasso_data), " observations x ", 
        ncol(glasso_data), " variables")

# Standardize variables
glasso_data_std <- scale(glasso_data)
colnames(glasso_data_std) <- colnames(glasso_data)

# ==============================================================================
# Compute Sample Covariance Matrix
# ==============================================================================

message("\n[3/8] Computing sample covariance matrix...")

S <- cov(glasso_data_std)
n <- nrow(glasso_data_std)
p <- ncol(glasso_data_std)

message("  - Sample size (n): ", n)
message("  - Number of variables (p): ", p)
message("  - Condition number: ", round(kappa(S), 2))

# ==============================================================================
# Select Regularization Parameter (Lambda)
# ==============================================================================

message("\n[4/8] Selecting optimal lambda using EBIC...")
message("  This may take several minutes...")

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
message("  - Optimal lambda: ", round(lambda_opt, 4))
message("  - Number of edges: ", sum(huge_select$refit != 0) / 2)

# ==============================================================================
# Fit Graphical Lasso with Optimal Lambda
# ==============================================================================

message("\n[5/8] Fitting Graphical Lasso with optimal lambda...")

# Fit glasso
glasso_fit <- glasso(S, rho = lambda_opt, penalize.diagonal = FALSE)

# Extract precision matrix (inverse covariance)
Theta <- glasso_fit$wi
rownames(Theta) <- colnames(Theta) <- colnames(glasso_data_std)

# Extract partial correlation matrix
partial_cor <- -cov2cor(Theta)
diag(partial_cor) <- 1

message("  - Sparsity: ", round(sum(Theta == 0) / length(Theta) * 100, 2), "%")
message("  - Non-zero elements: ", sum(Theta != 0))

# ==============================================================================
# Extract Network Structure
# ==============================================================================

message("\n[6/8] Extracting network structure...")

# Create adjacency matrix (threshold small values)
threshold <- 1e-6
adj_matrix <- (abs(Theta) > threshold) * 1
diag(adj_matrix) <- 0

# Convert to edge list with weights
edges <- which(adj_matrix != 0 & upper.tri(adj_matrix), arr.ind = TRUE)
edge_list <- tibble(
  from = colnames(glasso_data_std)[edges[, 1]],
  to = colnames(glasso_data_std)[edges[, 2]],
  precision = Theta[edges],
  partial_cor = partial_cor[edges],
  abs_partial_cor = abs(partial_cor[edges])
) %>%
  arrange(desc(abs_partial_cor))

message("  - Total edges: ", nrow(edge_list))
message("  - Mean |partial correlation|: ", 
        round(mean(edge_list$abs_partial_cor), 3))

# Save edge list
write_csv(edge_list, file.path(GLASSO_DIR, "glasso_edges.csv"))

# Identify neighbors of performance variables
math_neighbors <- edge_list %>%
  filter(from == "medida500_M" | to == "medida500_M") %>%
  mutate(neighbor = ifelse(from == "medida500_M", to, from)) %>%
  arrange(desc(abs_partial_cor))

lang_neighbors <- edge_list %>%
  filter(from == "medida500_L" | to == "medida500_L") %>%
  mutate(neighbor = ifelse(from == "medida500_L", to, from)) %>%
  arrange(desc(abs_partial_cor))

message("\n  Variables connected to Math (top 10):")
print(head(math_neighbors %>% select(neighbor, partial_cor), 10))

message("\n  Variables connected to Language (top 10):")
print(head(lang_neighbors %>% select(neighbor, partial_cor), 10))

# Save neighbor lists
write_csv(math_neighbors, file.path(GLASSO_DIR, "math_neighbors.csv"))
write_csv(lang_neighbors, file.path(GLASSO_DIR, "language_neighbors.csv"))

# ==============================================================================
# Network Analysis
# ==============================================================================

message("\n[7/8] Analyzing network properties...")

# Convert to igraph
ig <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", 
                                   diag = FALSE, weighted = NULL)

# Calculate centrality measures
node_stats <- tibble(
  variable = colnames(glasso_data_std),
  degree = degree(ig),
  betweenness = betweenness(ig),
  closeness = closeness(ig),
  eigenvector = eigen_centrality(ig)$vector
) %>%
  arrange(desc(degree))

write_csv(node_stats, file.path(GLASSO_DIR, "node_centrality.csv"))

message("  Top 10 most central variables (by degree):")
print(head(node_stats %>% select(variable, degree, betweenness), 10))

# Detect communities
communities <- cluster_louvain(ig)
node_stats$community <- membership(communities)

message("\n  - Number of communities: ", max(node_stats$community))
message("  - Modularity: ", round(modularity(communities), 3))

# Save community assignments
write_csv(node_stats %>% select(variable, community), 
          file.path(GLASSO_DIR, "communities.csv"))

# ==============================================================================
# Visualizations
# ==============================================================================

message("\n[8/8] Generating visualizations...")

# Plot 1: Partial correlation heatmap
png(file.path(FIG_DIR, "glasso_partial_correlations.png"), 
    width = 12, height = 12, units = "in", res = 150)
corrplot(partial_cor, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, tl.cex = 0.7,
         title = "Partial Correlation Network (GLasso)",
         mar = c(0, 0, 2, 0),
         is.corr = TRUE)
dev.off()
message("  - Saved glasso_partial_correlations.png")

# Plot 2: Network visualization using qgraph
png(file.path(FIG_DIR, "glasso_network.png"), 
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
message("  - Saved glasso_network.png")

# Plot 3: Ego networks for performance variables
png(file.path(FIG_DIR, "glasso_math_ego_network.png"), 
    width = 10, height = 10, units = "in", res = 150)
math_idx <- which(colnames(partial_cor) == "medida500_M")
qgraph(partial_cor,
       layout = "spring",
       vsize = 6,
       cut = 0.05,
       maximum = 1,
       title = "Math Performance Neighborhood",
       labels = colnames(partial_cor))
dev.off()
message("  - Saved glasso_math_ego_network.png")

# Plot 4: Degree distribution
p1 <- ggplot(node_stats, aes(x = degree)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Node Degree Distribution",
       x = "Degree", y = "Count") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Plot 5: Centrality comparison
p2 <- ggplot(node_stats %>% head(20), 
             aes(x = reorder(variable, degree), y = degree)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top 20 Variables by Degree Centrality",
       x = NULL, y = "Degree") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p_combined <- grid.arrange(p1, p2, ncol = 2)
ggsave(file.path(FIG_DIR, "glasso_centrality_analysis.png"), p_combined,
       width = 14, height = 6, dpi = 150)
message("  - Saved glasso_centrality_analysis.png")

# Plot 6: Community structure
community_sizes <- table(node_stats$community)
p3 <- ggplot(data.frame(community = names(community_sizes),
                        size = as.numeric(community_sizes)),
             aes(x = reorder(community, -size), y = size)) +
  geom_col(fill = "coral", alpha = 0.7) +
  labs(title = "Community Sizes",
       x = "Community", y = "Number of Variables") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(FIG_DIR, "glasso_communities.png"), p3,
       width = 8, height = 6, dpi = 150)
message("  - Saved glasso_communities.png")

# Plot 7: Lambda path (regularization path)
png(file.path(FIG_DIR, "glasso_lambda_path.png"), 
    width = 10, height = 6, units = "in", res = 150)
plot(huge_select)
dev.off()
message("  - Saved glasso_lambda_path.png")

# ==============================================================================
# Compare with Simple Correlation
# ==============================================================================

message("\nComparing GLasso with simple correlation...")

# Simple correlation
simple_cor <- cor(glasso_data_std)

# Count edges
simple_edges <- sum(abs(simple_cor) > 0.1 & upper.tri(simple_cor))
glasso_edges <- sum(abs(partial_cor) > 0.05 & upper.tri(partial_cor))

message("  - Simple correlation edges (|r| > 0.1): ", simple_edges)
message("  - GLasso edges (|partial r| > 0.05): ", glasso_edges)
message("  - Sparsity gain: ", 
        round((1 - glasso_edges/simple_edges) * 100, 1), "%")

# ==============================================================================
# Summary Report
# ==============================================================================

message("\n" %+% "="*80)
message("Graphical Lasso Analysis Complete!")
message("="*80)
message("\nGenerated outputs:")
message("  - Edge list: ", file.path(GLASSO_DIR, "glasso_edges.csv"))
message("  - Node centrality: ", file.path(GLASSO_DIR, "node_centrality.csv"))
message("  - Communities: ", file.path(GLASSO_DIR, "communities.csv"))
message("  - Math neighbors: ", file.path(GLASSO_DIR, "math_neighbors.csv"))
message("  - Language neighbors: ", file.path(GLASSO_DIR, "language_neighbors.csv"))
message("  - Figures: ", FIG_DIR)

message("\nKey findings:")
message("  - Optimal lambda: ", round(lambda_opt, 4))
message("  - Network sparsity: ", round(sum(Theta == 0) / length(Theta) * 100, 2), "%")
message("  - Total edges: ", nrow(edge_list))
message("  - Number of communities: ", max(node_stats$community))
message("  - Most central variable: ", node_stats$variable[1], 
        " (degree = ", node_stats$degree[1], ")")
message("  - Variables connected to Math: ", nrow(math_neighbors))
message("  - Variables connected to Language: ", nrow(lang_neighbors))

message("\n✓ All analyses complete!")
message("\nRecommended next steps:")
message("  1. Review edge lists to identify key relationships")
message("  2. Examine community structure for variable clustering")
message("  3. Compare PC algorithm and GLasso results for robustness")
message("  4. Integrate findings into paper")
