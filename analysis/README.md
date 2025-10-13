# Educational Network Analysis Pipeline

This directory contains the complete R analysis pipeline for the paper "A Network Analysis of Factors Influencing Academic and Socioemotional Learning in Peruvian Primary Schools."

## Overview

The analysis consists of four main scripts that should be run in sequence:

1. **00_data_preprocessing.R** - Data loading and preprocessing
2. **01_eda_analysis.R** - Exploratory data analysis and visualization
3. **02_pc_algorithm.R** - Causal discovery using PC algorithm
4. **03_graphical_lasso.R** - Sparse network estimation using Graphical Lasso

## Requirements

### R Version
- R >= 4.0.0

### Required Packages

```r
# Data manipulation
install.packages(c("readr", "dplyr", "tidyr", "tibble", "here"))

# Visualization
install.packages(c("ggplot2", "corrplot", "GGally", "gridExtra"))

# Causal discovery (PC algorithm)
install.packages("BiocManager")
BiocManager::install(c("graph", "Rgraphviz"))
install.packages(c("pcalg", "igraph"))

# Network estimation (Graphical Lasso)
install.packages(c("glasso", "huge", "qgraph"))
```

## Usage

### Quick Start

Run all analyses in sequence:

```bash
cd /path/to/github/education/analysis
Rscript 00_data_preprocessing.R
Rscript 01_eda_analysis.R
Rscript 02_pc_algorithm.R
Rscript 03_graphical_lasso.R
```

### Individual Scripts

#### 1. Data Preprocessing

```bash
Rscript 00_data_preprocessing.R
```

**What it does:**
- Loads raw ENLA 2024 data from RDS file
- Cleans and standardizes categorical variables (gender, area, school type, language)
- Handles missing data
- Creates analysis-ready datasets
- Generates summary statistics

**Outputs:**
- `outputs/preprocessed/enla_clean.rds` - Cleaned data with metadata
- `outputs/preprocessed/enla_analysis.csv` - Analysis dataset in CSV format

#### 2. Exploratory Data Analysis

```bash
Rscript 01_eda_analysis.R
```

**What it does:**
- Computes descriptive statistics by demographic groups
- Generates violin plots showing performance distributions
- Creates scatter plots with density contours
- Analyzes correlations among variables
- Calculates performance gaps

**Outputs:**
- `outputs/eda/descriptive_stats_*.csv` - Summary statistics
- `outputs/eda/correlation_matrix.csv` - Full correlation matrix
- `outputs/eda/performance_gaps.csv` - Gap analysis results
- `outputs/eda/figures/violin_*.png` - Distribution plots (4 files)
- `outputs/eda/figures/scatter_*.png` - Scatter plots (4 files)
- `outputs/eda/figures/correlation_heatmap.png` - Correlation visualization

#### 3. PC Algorithm (Causal Discovery)

```bash
Rscript 02_pc_algorithm.R
```

**What it does:**
- Applies PC algorithm to discover causal structure
- Uses conditional independence tests (Gaussian CI test)
- Identifies directed and undirected edges
- Performs stability analysis via bootstrap
- Visualizes causal networks

**Parameters:**
- `ALPHA = 0.01` - Significance level for independence tests
- `MAX_COND_SET = 3` - Maximum conditioning set size
- `N_BOOT = 50` - Number of bootstrap samples

**Outputs:**
- `outputs/pc_algorithm/pc_edges.csv` - Discovered causal edges
- `outputs/pc_algorithm/node_statistics.csv` - Node centrality measures
- `outputs/pc_algorithm/edge_stability.csv` - Bootstrap stability analysis
- `outputs/pc_algorithm/figures/pc_full_network.png` - Complete causal graph
- `outputs/pc_algorithm/figures/pc_math_neighborhood.png` - Math ego network
- `outputs/pc_algorithm/figures/pc_language_neighborhood.png` - Language ego network
- `outputs/pc_algorithm/figures/pc_degree_distribution.png` - Degree distribution

#### 4. Graphical Lasso (Network Estimation)

```bash
Rscript 03_graphical_lasso.R
```

**What it does:**
- Estimates sparse precision matrix using Graphical Lasso
- Selects optimal regularization parameter via EBIC
- Computes partial correlations (conditional dependencies)
- Detects network communities
- Analyzes network centrality

**Parameters:**
- `GAMMA = 0.5` - EBIC gamma parameter (0 = BIC, 1 = conservative)

**Outputs:**
- `outputs/graphical_lasso/glasso_edges.csv` - Network edges with weights
- `outputs/graphical_lasso/node_centrality.csv` - Centrality measures
- `outputs/graphical_lasso/communities.csv` - Community assignments
- `outputs/graphical_lasso/math_neighbors.csv` - Variables connected to Math
- `outputs/graphical_lasso/language_neighbors.csv` - Variables connected to Language
- `outputs/graphical_lasso/figures/glasso_network.png` - Full network visualization
- `outputs/graphical_lasso/figures/glasso_partial_correlations.png` - Heatmap
- `outputs/graphical_lasso/figures/glasso_centrality_analysis.png` - Centrality plots
- `outputs/graphical_lasso/figures/glasso_communities.png` - Community structure
- `outputs/graphical_lasso/figures/glasso_lambda_path.png` - Regularization path

## Output Structure

```
outputs/
├── preprocessed/
│   ├── enla_clean.rds
│   └── enla_analysis.csv
├── eda/
│   ├── descriptive_stats_*.csv
│   ├── correlation_matrix.csv
│   ├── performance_gaps.csv
│   └── figures/
│       ├── violin_*.png (4 files)
│       ├── scatter_*.png (4 files)
│       └── correlation_heatmap.png
├── pc_algorithm/
│   ├── pc_edges.csv
│   ├── node_statistics.csv
│   ├── edge_stability.csv
│   └── figures/
│       ├── pc_full_network.png
│       ├── pc_math_neighborhood.png
│       ├── pc_language_neighborhood.png
│       └── pc_degree_distribution.png
└── graphical_lasso/
    ├── glasso_edges.csv
    ├── node_centrality.csv
    ├── communities.csv
    ├── math_neighbors.csv
    ├── language_neighbors.csv
    └── figures/
        ├── glasso_network.png
        ├── glasso_partial_correlations.png
        ├── glasso_math_ego_network.png
        ├── glasso_centrality_analysis.png
        ├── glasso_communities.png
        └── glasso_lambda_path.png
```

## Methodology

### PC Algorithm

The PC (Peter-Clark) algorithm discovers causal relationships through conditional independence testing:

1. **Skeleton Discovery**: Start with complete graph, remove edges based on conditional independence
2. **Edge Orientation**: Orient edges using v-structures and propagation rules
3. **Output**: Directed Acyclic Graph (DAG) representing causal structure

**Key assumptions:**
- Causal Markov Condition
- Faithfulness
- Causal Sufficiency (no unmeasured confounders)
- Acyclicity

### Graphical Lasso

GLasso estimates a sparse precision matrix (inverse covariance) using ℓ₁ regularization:

- **Optimization**: Maximize penalized log-likelihood
- **Sparsity**: ℓ₁ penalty sets small elements to exactly zero
- **Interpretation**: Zero elements indicate conditional independence
- **Selection**: EBIC criterion balances fit and sparsity

**Advantages:**
- Provides edge weights (strength of conditional association)
- Handles high-dimensional data
- Produces interpretable sparse networks

## Interpreting Results

### Edge Lists

- **PC Algorithm**: Directed edges suggest potential causal relationships
- **GLasso**: Undirected edges with weights (partial correlations)
- **Stability**: Higher stability scores indicate more reliable edges

### Centrality Measures

- **Degree**: Number of connections (local importance)
- **Betweenness**: Frequency on shortest paths (bridging role)
- **Closeness**: Average distance to other nodes (global accessibility)
- **Eigenvector**: Importance based on connections to important nodes

### Communities

- Groups of densely connected variables
- May represent latent constructs (e.g., family factors, school climate)
- Bridge variables connect different communities

## Troubleshooting

### Common Issues

1. **Missing RDS file**
   - Ensure `data/enla_processed_data.rds` exists
   - Check file path in preprocessing script

2. **Package installation errors**
   - Bioconductor packages require `BiocManager`
   - Rgraphviz may need system dependencies (graphviz)

3. **Memory issues**
   - Reduce number of variables in analysis
   - Increase R memory limit: `options(java.parameters = "-Xmx8g")`

4. **Long computation times**
   - PC algorithm: Reduce `MAX_COND_SET` or sample size
   - GLasso: Reduce `nlambda` in huge() call
   - Bootstrap: Reduce `N_BOOT`

### Performance Optimization

- Use parallel processing for bootstrap (modify scripts)
- Pre-filter variables by variance or correlation
- Use sparse matrix representations for large networks

## Citation

If you use this analysis pipeline, please cite:

```
Aburto, E., & Richter, F. (2024). A Network Analysis of Factors Influencing 
Academic and Socioemotional Learning in Peruvian Primary Schools. 
[Manuscript in preparation]
```

## References

- Spirtes, P., Glymour, C., & Scheines, R. (2000). *Causation, Prediction, and Search* (2nd ed.). MIT Press.
- Friedman, J., Hastie, T., & Tibshirani, R. (2008). Sparse inverse covariance estimation with the graphical lasso. *Biostatistics*, 9(3), 432-441.

## Contact

For questions or issues, please contact:
- Elías Aburto (National University of Engineering, Peru)
- Francisco Richter (Università della Svizzera italiana, Switzerland)

## License

This analysis code is provided for research purposes. Please contact the authors for usage permissions.
