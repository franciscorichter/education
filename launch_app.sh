#!/bin/bash

# ENLA Educational Data Analysis Platform - Launcher Script
# This script launches the R Shiny application from the correct directory

echo "🚀 Starting ENLA Educational Data Analysis Platform..."
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Navigate to the app directory
cd "$SCRIPT_DIR/app"

echo "📍 Working directory: $(pwd)"
echo ""

# Check if R is installed
if ! command -v R &> /dev/null; then
    echo "❌ Error: R is not installed or not in PATH"
    echo "Please install R from https://cran.r-project.org/"
    exit 1
fi

# Check if required packages are installed
echo "📦 Checking R packages..."
R -e "
required_packages <- c('shiny', 'readxl', 'readr', 'dplyr', 'purrr', 'stringr', 'igraph', 'RColorBrewer', 'tibble', 'scales', 'DT', 'htmltools')
missing_packages <- required_packages[!required_packages %in% installed.packages()[,'Package']]

if (length(missing_packages) > 0) {
    cat('⚠️  Missing packages:', paste(missing_packages, collapse = ', '), '\n')
    cat('�� Installing missing packages...\n')
    install.packages(missing_packages, repos = 'https://cran.rstudio.com/')
    cat('✅ Packages installed successfully!\n')
} else {
    cat('✅ All required packages are installed!\n')
}
" 2>/dev/null || {
    echo "⚠️  Could not check/install R packages automatically"
    echo "📋 Please ensure you have these R packages installed:"
    echo "   - shiny, readxl, readr, dplyr, purrr, stringr, igraph, RColorBrewer, tibble, scales, DT, htmltools"
    echo ""
    read -p "Press Enter to continue anyway..."
}

echo ""
echo "🔍 Checking data files..."

# Check if data directory exists
if [ ! -d "../data" ]; then
    echo "❌ Error: Data directory not found at ../data"
    echo "Please ensure the data files are in the correct location"
    exit 1
fi

# Count Excel files
XLSX_COUNT=$(find ../data -name "*.xlsx" -type f | wc -l)
echo "📊 Found $XLSX_COUNT Excel data files"

if [ "$XLSX_COUNT" -eq 0 ]; then
    echo "⚠️  Warning: No Excel files found in data directory"
    read -p "Press Enter to continue anyway..."
fi

echo ""
echo "🌐 Starting Shiny application..."
echo "📱 The app will be available at: http://127.0.0.1:7856"
echo "🛑 Press Ctrl+C to stop the application"
echo ""

# Launch the Shiny app
R -e "shiny::runApp('app.R', port = 7856, host = '0.0.0.0')"
