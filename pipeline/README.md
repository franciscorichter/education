# Educational Network Analysis Pipeline

This directory contains the complete R analysis pipeline for the paper "A Network Analysis of Factors Influencing Academic and Socioemotional Learning in Peruvian Primary Schools."

## Overview

The pipeline consists of 6 sequential steps that transform raw Excel data into publication-ready results:

1. **01_load_data.R** - Load four Excel datasets
2. **02_preprocess.R** - Merge and clean data
3. **03_eda.R** - Exploratory data analysis with visualizations
4. **04_pca.R** - Principal component analysis
5. **05_pc_algorithm.R** - Causal discovery using PC algorithm
6. **06_graphical_lasso.R** - Sparse network estimation

## Quick Start

### Run Complete Pipeline
```bash
cd /path/to/github/education
Rscript pipeline/run_pipeline.R
```

### Run Individual Steps
```bash
Rscript pipeline/01_load_data.R
Rscript pipeline/02_preprocess.R
Rscript pipeline/03_eda.R
Rscript pipeline/04_pca.R
Rscript pipeline/05_pc_algorithm.R
Rscript pipeline/06_graphical_lasso.R
```

## Data Requirements

The pipeline expects four Excel files in the specified locations:

| Dataset | Path | Purpose |
|---------|------|---------|
| **alumnos** | `01 - Data/xlsx/EM_6P_2024_alumnos_innominados.xlsx` | Student performance data |
| **base_hse** | `01 - Data/xlsx/base_web2_HSE_ENLA_2024.xlsx` | Socioeconomic and school data |
| **estudiante** | `01 - Data/xlsx/ENLA2024_6Pestudiante_EBRD1.xlsx` | Student questionnaire responses |
| **familia** | `01 - Data/xlsx/ENLA2024_6Pfamilia_EBR.xlsx` | Family questionnaire responses |

## Pipeline Steps

### Step 1: Load Data (`01_load_data.R`)
- Loads four Excel files with appropriate sheet selection
- Reports file existence, sheet names, and dimensions
- Uses `BD` sheet for alumnos file (data sheet)
- Uses first sheet for other files

**Output**: Console report of load status

### Step 2: Preprocess (`02_preprocess.R`)
- Merges datasets using common ID columns
- Cleans column names and removes empty rows/columns
- Identifies performance measures and demographic variables
- Creates analysis-ready datasets

**Output**:
- `outputs/preprocessed/enla_preprocessed.rds` - Full processed data
- `outputs/preprocessed/enla_analysis_ready.csv` - Analysis subset

### Step 3: EDA (`03_eda.R`)
- Computes descriptive statistics by demographic groups
- Generates violin plots for performance distributions
- Creates correlation heatmaps
- Analyzes performance gaps

**Output**:
- `outputs/eda/descriptive_stats.csv`
- `outputs/eda/performance_correlations.csv`
- `outputs/eda/performance_gaps.csv`
- `outputs/eda/figures/` - 8+ PNG figures

### Step 4: PCA (`04_pca.R`)
- Performs principal component analysis on standardized data
- Identifies main patterns of variation
- Generates scree plots and component loadings

**Output**:
- `outputs/pca/pca_eigenvalues.csv`
- `outputs/pca/pca_loadings.csv`
- `outputs/pca/pca_scores.csv`
- `outputs/pca/figures/` - 5 PNG figures

### Step 5: PC Algorithm (`05_pc_algorithm.R`)
- Applies PC algorithm for causal discovery
- Uses conditional independence testing
- Performs bootstrap stability analysis
- Visualizes causal networks

**Output**:
- `outputs/pc_algorithm/pc_edges.csv`
- `outputs/pc_algorithm/node_statistics.csv`
- `outputs/pc_algorithm/edge_stability.csv`
- `outputs/pc_algorithm/figures/` - 4 PNG figures

### Step 6: Graphical Lasso (`06_graphical_lasso.R`)
- Estimates sparse precision matrix using GLasso
- Identifies conditional independence relationships
- Detects network communities
- Compares with simple correlation

**Output**:
- `outputs/graphical_lasso/glasso_edges.csv`
- `outputs/graphical_lasso/node_centrality.csv`
- `outputs/graphical_lasso/communities.csv`
- `outputs/graphical_lasso/figures/` - 6 PNG figures

## Output Structure

```
outputs/
├── preprocessed/
│   ├── enla_preprocessed.rds
│   └── enla_analysis_ready.csv
├── eda/
│   ├── descriptive_stats.csv
│   ├── performance_correlations.csv
│   ├── performance_gaps.csv
│   └── figures/
│       ├── violin_*.png (4 files)
│       └── correlation_heatmap.png
├── pca/
│   ├── pca_eigenvalues.csv
│   ├── pca_loadings.csv
│   ├── pca_scores.csv
│   └── figures/
│       ├── pca_scree_plot.png
│       ├── pca_variable_loadings.png
│       └── pca_individuals.png
├── pc_algorithm/
│   ├── pc_edges.csv
│   ├── node_statistics.csv
│   ├── edge_stability.csv
│   └── figures/
│       ├── pc_full_network.png
│       └── pc_math_neighborhood.png
└── graphical_lasso/
    ├── glasso_edges.csv
    ├── node_centrality.csv
    ├── communities.csv
    └── figures/
        ├── glasso_network.png
        └── glasso_partial_correlations.png
```

## Required R Packages

Install these packages before running the pipeline:

```r
# Core packages
install.packages(c("readxl", "readr", "dplyr", "tidyr", "tibble", "stringr", "purrr"))

# Visualization
install.packages(c("ggplot2", "corrplot", "GGally", "gridExtra"))

# PCA
install.packages(c("FactoMineR", "factoextra"))

# PC algorithm (causal discovery)
install.packages("pcalg")  # May require BiocManager

# Graphical Lasso (network estimation)
install.packages(c("glasso", "huge", "qgraph", "igraph"))

# If pcalg fails:
# install.packages("BiocManager")
# BiocManager::install("pcalg")
```

## Parameters

### PC Algorithm (`05_pc_algorithm.R`)
- `ALPHA = 0.01` - Significance level for independence tests
- `MAX_COND_SET = 3` - Maximum conditioning set size
- `N_BOOT = 50` - Bootstrap samples for stability

### Graphical Lasso (`06_graphical_lasso.R`)
- `GAMMA = 0.5` - EBIC gamma parameter (0 = BIC, 1 = conservative)

## Methodology

### PC Algorithm
The PC (Peter-Clark) algorithm discovers causal relationships through:
1. **Skeleton Discovery** - Remove edges based on conditional independence
2. **Edge Orientation** - Orient edges using v-structures and propagation rules
3. **Output** - Directed Acyclic Graph (DAG) representing causal structure

### Graphical Lasso
GLasso estimates sparse precision matrices using ℓ₁ regularization:
- **Optimization** - Maximize penalized log-likelihood
- **Sparsity** - ℓ₁ penalty sets small elements to exactly zero
- **Interpretation** - Zero elements indicate conditional independence

## Interpreting Results

### Edge Lists
- **PC Algorithm** - Directed edges suggest potential causal relationships
- **GLasso** - Undirected edges with weights (partial correlations)
- **Stability** - Higher stability scores indicate more reliable edges

### Centrality Measures
- **Degree** - Number of connections (local importance)
- **Betweenness** - Frequency on shortest paths (bridging role)
- **Closeness** - Average distance to other nodes (global accessibility)

### Communities
- Groups of densely connected variables
- May represent latent constructs (family factors, school climate)
- Bridge variables connect different communities

## Troubleshooting

### Common Issues

1. **Missing Excel files**
   - Ensure all four Excel files exist in the specified paths
   - Check file permissions

2. **Package installation errors**
   - Some packages require Bioconductor (`BiocManager`)
   - Rgraphviz may need system dependencies

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

## Integration with Paper

The generated outputs directly support the paper sections:

1. **EDA plots** → Data section (Figures 1-2 in paper)
2. **PC algorithm results** → Methods & Results sections
3. **GLasso networks** → Results & Discussion sections
4. **CSV files** → Tables and supplementary materials

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
