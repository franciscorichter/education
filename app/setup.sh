#!/bin/bash
# ENLA Network App Setup Script
# Run this from the app directory to ensure all dependencies are installed

echo "=== ENLA Network App Setup ==="
echo "Checking R packages..."

# Check if required packages are installed
R -e "
required_packages <- c('shiny', 'readxl', 'readr', 'dplyr', 'purrr', 'stringr', 'igraph', 'RColorBrewer', 'tibble', 'scales', 'DT')
missing_packages <- required_packages[!required_packages %in% installed.packages()[,'Package']]
if (length(missing_packages) > 0) {
  cat('Installing missing packages:', paste(missing_packages, collapse=', '), '\n')
  install.packages(missing_packages)
} else {
  cat('All required packages are installed!\n')
}
"

echo ""
echo "=== Setup Complete ==="
echo "To run the app:"
echo "1. Open R/RStudio"
echo "2. Set working directory to this app folder"
echo "3. Run: shiny::runApp('app.R')"
echo ""
echo "Or from command line:"
echo "R -e \"shiny::runApp('/path/to/app/app.R')\""
echo ""
echo "The app will open in your default web browser."
