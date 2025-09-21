# ENLA Educational Data Analysis Platform

A standalone R Shiny application for analyzing ENLA (National Learning Assessment) educational data.

## 📁 Current Project Structure

```
github/education/                    # Git repository root
├── 📁 app/                          # R Shiny application
│   ├── 📄 app.R                     # Main application script (with embedded paths)
│   ├── 📄 launch_app.sh            # Easy launcher script
│   ├── 📄 README.md                 # App documentation
│   ├── 📁 data_prepared/            # Pre-processed CSV data (8MB)
│   └── 📄 *.rds                     # Cached data files (60MB total)
├── 📁 data/                         # All data files in one place
│   ├── 📄 *.xlsx                    # 9 Excel data files (247MB)
│   └── 📄 Nota.txt                  # Data matching instructions
├── 📁 .git/                         # Git repository
└── 📄 README.md                     # This file
```

## 🚀 Quick Start

### **Option 1: Launcher Script (Recommended)**
```bash
cd github/education
./launch_app.sh
```

### **Option 2: Direct R Command**
```bash
cd github/education/app
R -e "shiny::runApp('app.R')"
```

### **Option 3: RStudio**
1. Open `app/app.R` in RStudio
2. Click "Run App" button

## 📊 Data Status

### **✅ Complete Dataset Ready**
- **9 Excel Files** (247MB total)
- **8 ENLA Questionnaires** + **1 Performance Dataset**
- **60MB Cached RDS Files** (ready for instant loading)
- **8MB Pre-processed CSV Files**

### **📋 Available Data Files:**
- **Student Survey** (ENLA2024_6Pestudiante_EBRD1.xlsx) - 103MB
- **Family Survey** (ENLA2024_6Pfamilia_EBR.xlsx) - 69MB
- **Math Teacher Survey** (ENLA2024_6PdocenteMAT_EBR.xlsx) - 3.4MB
- **Communication Teacher Survey** (ENLA2024_6PdocenteCOM_EBR.xlsx) - 3.8MB
- **Tutor Teacher Survey** (ENLA2024_6PdocenteTutor_EBR.xlsx) - 3.4MB
- **Director Survey F1** (ENLA2024_6Pdirector_EBRF1.xlsx) - 1.3MB
- **Director Survey F2** (ENLA2024_6Pdirector_EBRF2.xlsx) - 1.6MB
- **Performance Data** (EM_6P_2024_alumnos_innominados.xlsx) - 17MB
- **Base Data** (base_web2_HSE_ENLA_2024.xlsx) - 23MB

## 🔧 Key Features

### **✅ Portable & Standalone**
- **No hardcoded paths** - works from any location
- **Embedded path configuration** - truly portable
- **Self-contained** - all data included
- **Git-ready** - can be shared or moved easily

### **🚀 Performance Optimized**
- **Intelligent caching** - 60MB RDS cache files
- **Fast loading** - pre-processed CSV data available
- **Versioned cache** - automatically refreshes when needed
- **Memory efficient** - optimized data structures

### **📊 Analysis Capabilities**
- **Correlation Network Analysis** - visualize relationships between questionnaire items
- **Multi-level Analysis** - item-level and construct-level network analysis
- **Performance Integration** - link questionnaire responses with academic performance
- **Interactive Visualizations** - dynamic network plots with filtering options
- **Data Quality Assessment** - built-in data validation and quality checks
- **Statistical Summary** - comprehensive statistical analysis of performance data

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
- **Enhanced error handling**: Provides detailed feedback on integration issues
- **Quality validation**: Comprehensive validation of data integration requirements

## 🔄 Data Processing

The app automatically processes Excel files and creates cached RDS files for faster loading. The cache is versioned and will automatically refresh when the source data or processing logic changes.

### **Processing Pipeline:**
1. **Excel Files** → **Data Cleaning** → **RDS Cache** → **Analysis Ready**
2. **Automatic Fallbacks** - uses cached data when Excel files unavailable
3. **Quality Validation** - checks data integrity during processing

## 📈 Usage Guide

### **1. Launch Application**
```bash
./launch_app.sh
```

### **2. Check Data Status**
- Look at the "📊 Data Status" panel on the main page
- Should show "✅ 8 datasets" and "✅ EM performance data loaded"

### **3. Explore Data**
- **EDA Tab**: Examine EM performance data and distributions with statistical summaries
- **Network Analysis Tab**: Build correlation networks between questionnaire items
- **Advanced Tab**: Future machine learning features

### **4. Network Analysis**
- Select questionnaire from dropdown
- Choose correlation threshold (0.1-0.8)
- Select analysis level (item-level or construct-level)
- Click "Build network" to generate visualization
- Use "Agregar nodos de desempeño L/M" to include performance nodes

## 🎯 App Will Be Available At:
- **URL**: http://127.0.0.1:7856
- **Local Access**: http://localhost:7856
- **Network Access**: http://0.0.0.0:7856

## 📋 System Requirements

### **R Version**
- R 4.0 or higher
- Available from: https://cran.r-project.org/

### **Required R Packages**
- shiny, readxl, readr, dplyr, purrr, stringr
- igraph, RColorBrewer, tibble, scales, DT

### **Automatic Installation**
The launcher script automatically checks and installs missing packages.

## 🛠️ Technical Architecture

### **Data Processing Pipeline**
```mermaid
graph LR
    A[Excel Files] → B[Path Resolution]
    A → C[Data Cleaning & Validation]
    B → D[Enhanced Join Key System]
    C → D
    D → E[Statistical Analysis]
    D → F[Network Generation]
    E → G[Interactive Visualization]
```

### **Key Improvements Made**
- ✅ **Enhanced join key logic** - proper matching by questionnaire type
- ✅ **Multiple key support** - handles complex join scenarios
- ✅ **Improved data integration** - better performance data linking
- ✅ **Enhanced error handling** - detailed user feedback
- ✅ **Statistical summaries** - comprehensive data analysis
- ✅ **Comprehensive documentation** - clear usage guide

## 🔧 Troubleshooting

### **Common Issues:**

1. **"Data directory not found"**
   - Ensure you're running from the correct directory
   - Check that `data/xlsx/` contains Excel files

2. **"R packages missing"**
   - Run the launcher script - it installs missing packages automatically
   - Or manually install: `install.packages(c("shiny", "readxl", "dplyr", ...))`

3. **"Port 7856 already in use"**
   - Change port in app.R or use different port
   - Kill existing R processes

4. **"Excel files not found"**
   - The app uses cached RDS files - this is normal and faster
   - Excel files are used only when cache needs refreshing

## 📚 Data Dictionary

### **File Naming Convention:**
- `ENLA2024_6P{type}_{subtype}.xlsx`
- Types: estudiante, familia, docenteMAT, docenteCOM, docenteTutor, director

### **Join Key Definitions:**
- **Student ID**: `ID_ESTUDIANTE` - Individual student identifier
- **School Code**: `cod_mod7` - 7-digit school identifier  
- **Annex**: `anexo` - School annex identifier
- **Section ID**: `ID_seccion` - Classroom section identifier
- **Performance**: `medida500_L` (Reading), `medida500_M` (Math)

## 🎉 Success Indicators

### **App Successfully Started:**
- ✅ "Starting ENLA Educational Data Analysis Platform..."
- ✅ "Listening on http://127.0.0.1:7856"
- ✅ Browser opens automatically

### **Data Successfully Loaded:**
- ✅ "📊 Data Status" shows green checkmarks
- ✅ "Questionnaire Data: ✅ 8 datasets"
- ✅ "EM Performance Data: ✅ Loaded"

### **Analysis Ready:**
- ✅ Network plots generate without errors
- ✅ Data tables display correctly
- ✅ All interactive features working
- ✅ Statistical summaries available

---

**🎯 This is now a fully standalone, portable ENLA analysis platform with enhanced data integration and statistical analysis capabilities!**

**📧 Contact**: For questions or issues, please refer to the original project documentation or create an issue in the git repository.
