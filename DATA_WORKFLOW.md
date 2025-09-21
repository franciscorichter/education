# ENLA Data Management Workflow

## 📋 Complete Data Processing Pipeline

### **🔍 1. Validate Data Files**
```bash
R -e "source('scripts/validate_data.R')"
```
- Checks Excel files exist in `data/xlsx/`
- Validates EM performance data
- Ensures all required files are present

### **📊 2. Load & Process Raw Data**
```bash
R -e "source('scripts/flexible_data_integration.R')"
```
- Loads 9 Excel files from `data/xlsx/`
- Processes questionnaire data (finds pXX columns)
- Loads EM performance data
- Saves processed data to `data/enla_raw_data.rds`

### **📁 3. Check Saved Data**
```bash
ls -la data/enla_raw_data.rds
```
- Confirms RDS file was created (should be ~60MB)

### **📖 4. Inspect Data Structure**
```bash
R -e "data <- readRDS('data/enla_raw_data.rds'); str(data, max.level=2)"
```
Shows:
- `em_data`: EM performance data
- `questionnaire_data`: List of 8 questionnaires
- `data_summary`: Summary statistics
- `loaded_at`: Timestamp

### **🔄 5. Test Data Reloading**
```bash
R -e "data2 <- readRDS('data/enla_raw_data.rds'); print('✅ Success')"
```
- Verifies data can be loaded again
- Checks data integrity

### **🌐 6. Launch Application**
```bash
./launch_app.sh
```
- App loads `data/enla_raw_data.rds`
- Uses data for analysis and visualization

## 📁 File Structure
```
data/
├── xlsx/                          # 9 Raw Excel files (247MB)
│   ├── ENLA2024_6Pestudiante_EBRD1.xlsx
│   ├── ENLA2024_6Pfamilia_EBR.xlsx
│   └── ... (7 more)
└── enla_raw_data.rds              # Processed data (60MB)
```

## 🔄 Data Flow
```
Excel Files → flexible_data_integration.R → enla_raw_data.rds → app.R → Shiny App
```

## 🛠️ Troubleshooting
- **Missing Excel files**: Run validation script
- **No RDS file**: Run data integration script
- **App won't load**: Check RDS file exists and is readable
