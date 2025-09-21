# ENLA Educational Data Analysis Platform - Application

This is the main R Shiny application for analyzing ENLA educational data.

## 📁 Current Structure

```
app/                          # R Shiny application directory
├── 📄 app.R                 # Main application script (with embedded paths)
├── 📄 launch_app.sh         # Launcher script
├── 📄 README.md             # This file
├── 📁 data_prepared/        # Pre-processed CSV data (8MB)
└── 📄 *.rds                 # Cached data files (60MB total)
```

## 🚀 How to Run

### Option 1: Launcher Script (Recommended)
```bash
cd ../  # Go to education directory
./launch_app.sh
```

### Option 2: Direct R Command
```bash
cd app/
R -e "shiny::runApp('app.R')"
```

### Option 3: RStudio
1. Open `app.R` in RStudio
2. Click "Run App" button

## 🔧 Technical Details

### Path Configuration
The path configuration is embedded directly in `app.R` to avoid circular dependency issues:

- **Smart Path Resolution**: Uses RStudio API when available, falls back to working directory
- **Relative Paths**: All paths are relative to the app directory
- **Automatic Fallback**: Finds data files in multiple locations if needed
- **No Hardcoded Paths**: Works from any location without absolute paths

### Data Loading
- **Primary Data**: Excel files in `../data/xlsx/` (247MB total)
- **Cached Data**: RDS files in app directory (60MB) for fast loading
- **Pre-processed Data**: CSV files in `data_prepared/` (8MB) for quick access

### Performance Features
- **Intelligent Caching**: Versioned cache system
- **Fast Loading**: Pre-built RDS files for instant access
- **Memory Efficient**: Optimized data structures
- **Error Recovery**: Graceful fallbacks if files missing

## 🔗 Enhanced Data Integration

### **Join Keys by Questionnaire Type:**

#### **👨‍🎓 Student & Family Questionnaires**
- **Join Key**: `ID_ESTUDIANTE`
- **Description**: Individual student level matching
- **Use**: Links student/family responses to EM performance data

#### **👨‍🏫 Teacher Questionnaires**  
- **Join Keys**: `cod_mod7`, `anexo`, `ID_seccion`
- **Description**: Teacher/section level matching
- **Use**: Links teacher responses to school-level EM data

#### **🏫 Director Questionnaires**
- **Join Keys**: `cod_mod7`, `anexo`  
- **Description**: School/director level matching
- **Use**: Links director responses to school-level EM data

### **Integration Features:**
- **Multi-key matching**: Handles complex join scenarios with multiple keys
- **Automatic key detection**: Determines appropriate join keys based on questionnaire type
- **Performance integration**: Links questionnaire responses with academic performance data
- **Quality assessment**: Validates join key availability and match rates

## 📊 Data Status

The app automatically detects and loads:
- ✅ 9 Excel data files (247MB)
- ✅ 8 ENLA questionnaires + 1 performance dataset
- ✅ 60MB cached RDS files
- ✅ 8MB pre-processed CSV files

## 🌐 Access URLs
- **Primary**: http://127.0.0.1:7856
- **Local**: http://localhost:7856
- **Network**: http://0.0.0.0:7856

## 🔍 Troubleshooting

If you encounter issues:
1. Check that you're running from the correct directory
2. Ensure all data files are in `../data/xlsx/`
3. Verify R and required packages are installed
4. Check the launcher script output for specific errors

## 📝 Notes

This app is designed to be completely standalone and portable. All path dependencies have been resolved using relative paths and intelligent fallbacks. The enhanced join key system provides robust data integration across different questionnaire types.
