#!/bin/bash

# ENLA Educational Data Analysis Platform - Clean Architecture
# Launcher with data validation

echo "🚀 ENLA Educational Data Analysis Platform"
echo "=========================================="
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "📍 Working directory: $(pwd)"
echo ""

# Option to run data validation only
if [ "$1" = "--validate-only" ]; then
    echo "🔍 Running data validation..."
    R -e "source('scripts/validate_data.R')"
    exit $?
fi

# Check if R is installed
if ! command -v R &> /dev/null; then
    echo "❌ Error: R is not installed or not in PATH"
    echo "Please install R from https://cran.r-project.org/"
    exit 1
fi

# Check if required packages are installed
echo "📦 Checking R packages..."
R -e "
required_packages <- c('shiny', 'readxl', 'readr', 'dplyr', 'purrr', 'stringr', 'igraph', 'RColorBrewer', 'tibble', 'scales', 'DT')
missing_packages <- required_packages[!required_packages %in% installed.packages()[,'Package']]

if (length(missing_packages) > 0) {
    cat('⚠️  Missing packages:', paste(missing_packages, collapse = ', '), '\n')
    cat('📥 Installing missing packages...\n')
    install.packages(missing_packages, repos = 'https://cran.rstudio.com/')
    cat('✅ Packages installed successfully!\n')
} else {
    cat('✅ All required packages are installed!\n')
}
" 2>/dev/null || {
    echo "⚠️  Could not check/install R packages automatically"
    echo "📋 Please ensure you have these R packages installed:"
    echo "   - shiny, readxl, readr, dplyr, purrr, stringr, igraph, RColorBrewer, tibble, scales, DT"
    echo ""
    read -p "Press Enter to continue anyway..."
}

echo ""
echo "🔍 Validating data..."

# Run data validation
R -e "source('scripts/validate_data.R')"

VALIDATION_EXIT=$?
if [ $VALIDATION_EXIT -ne 0 ]; then
    echo ""
    echo "❌ Data validation failed"
    echo "💡 Please check your data files and try again"
    echo "   Or run: ./launch_app.sh --validate-only"
    exit 1
fi

echo ""
echo "🔄 Loading data for dynamic integration..."

# Run the flexible data loader
R -e "source('scripts/flexible_data_integration.R')"

LOADER_EXIT_CODE=$?
if [ $LOADER_EXIT_CODE -ne 0 ]; then
    echo ""
    echo "❌ Error: Data loading failed"
    echo "💡 Check the error messages above for details"
    exit 1
fi

echo ""
echo "🌐 Starting Shiny application..."
echo "📱 The app will be available at: http://127.0.0.1:7856"
echo "🛑 Press Ctrl+C to stop the application"
echo ""
echo "🎯 Features:"
echo "   • Choose your own matching columns for data integration"
echo "   • Real-time integration with immediate feedback"
echo "   • Network analysis and correlation visualizations"
echo "   • Data explorer for examining raw data"
echo ""

# Launch the Shiny app
R -e "shiny::runApp('app/app.R', port = 7856, host = '0.0.0.0')"
