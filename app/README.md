# ENLA Item Network Shiny App

## Overview

This is a comprehensive Shiny web application for analyzing ENLA (National Learning Assessment) questionnaire data through interactive network visualization and exploration. The app allows users to:

- **Load and analyze multiple ENLA questionnaire datasets** (Student, Family, Teacher, Director surveys)
- **Generate correlation-based item networks** with customizable thresholds
- **Explore questionnaire items** with detailed question descriptions and sub-items
- **Aggregate item-level data** into higher-level constructs using various methods (mean, median, z-score, PCA)
- **Integrate performance data** from EM (Evaluación de Matemáticas) assessments

## Features

### Network Analysis
- **Item-level networks**: Visualize correlations between individual questionnaire items (pXX_YY format)
- **Top-level networks**: Aggregate sub-items into main constructs (pXX format) using different aggregation methods
- **Performance integration**: Optional overlay of student performance data (L/M scores) as additional network nodes
- **Interactive controls**: Adjustable correlation thresholds and network parameters

### Data Exploration
- **Questionnaire browser**: Detailed view of all questions, sub-items, and response options
- **Dictionary integration**: Automatic extraction of question metadata from Excel dictionary sheets
- **Multiple aggregation methods**: Mean, median, z-score normalization, and PCA-based aggregation

### Technical Capabilities
- **Efficient caching**: Pre-processed RDS files for fast loading
- **Robust data handling**: Support for various Excel formats and column naming conventions
- **Error handling**: Graceful fallbacks when data is missing or malformed
- **Multi-language support**: Spanish interface with English technical terms

## Dataset Structure

The app processes ENLA 2024 questionnaire data including:

- **Student Survey** (ENLA2024_6Pestudiante_EBRD1.xlsx) - 27 main constructs (p01-p27)
- **Family Survey** (ENLA2024_6Pfamilia_EBR.xlsx) - Family perspective on education
- **Teacher Surveys**:
  - Mathematics (ENLA2024_6PdocenteMAT_EBR.xlsx)
  - Communication (ENLA2024_6PdocenteCOM_EBR.xlsx)
  - Tutor (ENLA2024_6PdocenteTutor_EBR.xlsx)
- **Director Surveys**:
  - F1 (ENLA2024_6Pdirector_EBRF1.xlsx)
  - F2 (ENLA2024_6Pdirector_EBRF2.xlsx)
- **Performance Data** (EM_6P_2024_alumnos_innominados.xlsx) - Mathematics assessment scores

## Installation and Setup

### Quick Setup
```bash
# Make executable and run setup
chmod +x setup.sh
./setup.sh
```

### Running the Application

1. **Navigate to the app directory**:
   ```bash
   cd "/Users/pancho/Library/CloudStorage/Dropbox/Pancho/25 - CSG/04 - Education/github/education/app"
   ```

2. **Start the Shiny app**:
   ```r
   library(shiny)
   runApp("app.R")
   ```

3. **Open in browser**: The app will automatically open in your default web browser

### Alternative: Command Line
```bash
R -e "shiny::runApp('/Users/pancho/Library/CloudStorage/Dropbox/Pancho/25 - CSG/04 - Education/github/education/app/app.R')"
```

## Usage Guide

### 1. Select Questionnaire
- Choose from available ENLA questionnaires in the tab selector
- Each tab represents a different survey type (Student, Family, Teachers, Directors)

### 2. Build Network (Redes Tab)
- **Edge threshold**: Set minimum absolute correlation |r| (0.0 to 0.8)
- **Network level**:
  - "Second level: items pXX_YY": Individual questionnaire items
  - "Top level: aggregates pXX": Aggregated constructs
- **Aggregation method** (top-level only):
  - **Mean**: Simple average of sub-items
  - **Median**: Median value of sub-items
  - **Z-Mean**: Standardized (z-score) average
  - **PCA-1**: First principal component
- **Performance integration**: Optionally add EM performance nodes (L/M scores)

### 3. Explore Questions (Cuestionario Tab)
- Browse detailed question information
- View sub-items and response options
- Understand the structure of each questionnaire

## Data Processing

### Automatic Caching
- The app automatically processes Excel files into RDS format for faster loading
- Cache files are stored in the app directory and updated when source files change
- Version control ensures cache invalidation when processing logic changes

### Data Sources
- **Primary data**: Excel files in `../01 - Data/xlsx/` directory
- **Processed data**: Compressed CSV files in `data_prepared/` directory
- **Cache**: RDS files for rapid loading

## Technical Details

### File Structure
```
app/
├── app.R                    # Main Shiny application
├── build_augmented_db.R     # Database construction utility
├── preprocess_enla_data.py  # Python preprocessing script
├── data_prepared/           # Preprocessed CSV files
├── *.rds                    # Cached RDS files for each questionnaire
└── enla_augmented_db.rds    # Combined database
```

### Key Functions
- `build_cache_for()`: Processes individual Excel files into cache format
- `get_student_dict()`: Extracts question dictionaries from Excel sheets
- `aggregate_group()`: Implements various aggregation methods
- `plot_data()`: Generates network data for visualization

### Performance Features
- **Lazy loading**: Only processes data when needed
- **Efficient correlation computation**: Uses pairwise complete observations
- **Memory optimization**: Removes low-quality columns automatically
- **Fast rendering**: Pre-computed layouts for network visualization

## Output and Analysis

### Network Statistics
- Node count and edge count
- Items detected in each questionnaire
- Performance integration status (L/M indicators)
- Data source information

### Visual Features
- **Color coding**: Different colors for each construct group
- **Node sizing**: Larger nodes for performance indicators
- **Edge weighting**: Thicker edges for stronger correlations
- **Interactive legend**: Color mapping for construct groups

## Troubleshooting

### Common Issues

1. **Missing Excel files**: Ensure ENLA files are in the correct directory
2. **Package dependencies**: Install all required R packages
3. **Memory issues**: Large datasets may require more RAM
4. **Cache problems**: Delete RDS files to force fresh processing

### Data Quality Checks
- **Column detection**: Automatic identification of pXX item columns
- **Response coercion**: Converts A-E responses to numeric values
- **Missing data handling**: Robust handling of NA values
- **Format normalization**: Standardizes column names across files

## Development Notes

### Code Organization
- **Modular design**: Separate functions for data processing, UI, and server logic
- **Error handling**: Comprehensive try-catch blocks throughout
- **Documentation**: Detailed comments explaining complex operations
- **Version control**: Cache versioning for logic changes

### Extensibility
- Easy to add new questionnaires by placing Excel files in the data directory
- Aggregation methods can be extended by modifying `aggregate_group()`
- Network visualization parameters can be customized in the plotting section

## Citation and Credits

This application was developed as part of educational research analysis for the ENLA 2024 dataset. It represents a comprehensive tool for exploring questionnaire networks and understanding item relationships in educational assessment data.

**Author**: Cascade AI Assistant
**Date**: 2025-09-18
**Version**: 1.0

## License

This software is provided for educational and research purposes. Please ensure compliance with data usage agreements for the ENLA dataset.
