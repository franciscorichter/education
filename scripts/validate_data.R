# Data Validation Script for ENLA Platform
# Run this to check your data before using the app

cat("🔍 ENLA Data Validation Tool\n")
cat("=============================\n\n")

# Check data directory
if (!dir.exists("data")) {
  cat("❌ data/ directory not found\n")
  cat("   Please create data/ directory and add Excel files\n")
  quit(save = "no", status = 1)
}

# Check Excel files
xlsx_dir <- file.path("data", "xlsx")
if (!dir.exists(xlsx_dir)) {
  cat("❌ data/xlsx/ directory not found\n")
  cat("   Please create data/xlsx/ and add Excel files\n")
  quit(save = "no", status = 1)
}

# List Excel files
xlsx_files <- list.files(xlsx_dir, pattern = "\\.xlsx$", ignore.case = TRUE)
if (length(xlsx_files) == 0) {
  cat("❌ No Excel files found in data/xlsx/\n")
  cat("   Please add ENLA Excel files to data/xlsx/\n")
  quit(save = "no", status = 1)
}

cat("✅ Found", length(xlsx_files), "Excel files:\n")
for (file in xlsx_files) {
  cat("   📄", file, "\n")
}

# Check for required EM file
em_file <- grep("EM.*alumnos", xlsx_files, ignore.case = TRUE, value = TRUE)
if (length(em_file) == 0) {
  cat("\n⚠️  Warning: EM performance data not found\n")
  cat("   Looking for file containing 'EM' and 'alumnos'\n")
  cat("   The app will work but without performance integration\n")
} else {
  cat("\n✅ Found EM performance data:", em_file[1], "\n")
}

# Check for questionnaire files
questionnaire_files <- xlsx_files[!grepl("EM.*alumnos", xlsx_files, ignore.case = TRUE)]
if (length(questionnaire_files) == 0) {
  cat("\n❌ No questionnaire files found\n")
  cat("   Looking for ENLA questionnaire files\n")
  quit(save = "no", status = 1)
}

cat("✅ Found", length(questionnaire_files), "questionnaire files:\n")
for (file in questionnaire_files) {
  cat("   📋", file, "\n")
}

# Try to load a sample file to test structure
cat("\n🔍 Testing data structure...\n")
sample_file <- file.path(xlsx_dir, questionnaire_files[1])

tryCatch({
  sheets <- readxl::excel_sheets(sample_file)
  cat("✅", sample_file, "has", length(sheets), "sheets\n")
  
  if ("BD" %in% sheets) {
    cat("✅ Found 'BD' sheet (data sheet)\n")
  } else {
    cat("⚠️  No 'BD' sheet found, will use first sheet\n")
  }
  
  # Try to read a few rows
  data <- readxl::read_excel(sample_file, n_max = 5)
  cat("✅ Successfully read", nrow(data), "rows and", ncol(data), "columns\n")
  
}, error = function(e) {
  cat("❌ Error reading", sample_file, ":", e$message, "\n")
  quit(save = "no", status = 1)
})

cat("\n🎯 Data validation completed!\n")
cat("============================\n")
cat("✅ All required files found\n")
cat("✅ Data structure looks good\n")
cat("✅ Ready to run: ./launch_app.sh\n")
