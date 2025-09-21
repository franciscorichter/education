# Plataforma ENLA de Análisis de Datos Educativos

Una aplicación R Shiny completa para analizar datos educativos ENLA con integración dinámica de datos y análisis de redes por cuestionario.

## 📁 Estructura del Proyecto

```
github/education/
├── 📁 data/                         # Todos los archivos de datos
│   ├── 📁 xlsx/                    # 9 archivos Excel crudos (247MB)
│   └── 📄 enla_raw_data.rds        # Datos procesados para la app
├── 📁 app/                         # Aplicación Shiny completa
│   ├── 📄 app.R                    # App principal en español
│   └── 📄 launch_app.sh           # Lanzador
├── 📁 scripts/                     # Scripts de utilidad
│   ├── 📄 data_integration.R       # Carga y matching de datos
│   └── 📄 validate_data.R          # Validación de datos
├── 📄 launch_app.sh               # Lanzador principal
├── 📄 INSTRUCCIONES_MATCHING.txt   # Guía de matching
└── 📄 README.md                   # Documentación
```

## 🚀 Inicio Rápido

### **Opción 1: Lanzador de un clic (Recomendado)**
```bash
cd github/education
./launch_app.sh
```

### **Opción 2: Validación de datos**
```bash
./launch_app.sh --validate-only
```

### **Opción 3: Manual**
```bash
cd github/education
R -e "source('scripts/data_integration.R')"  # Cargar datos
R -e "shiny::runApp('app/app.R')"             # Lanzar app
```

## 🎯 Características Principales

### **✅ Pestañas Principales:**

#### **🔗 Integración:**
- **Selecciona cuestionario** y columnas para matching
- **Vista previa** de resultados de integración
- **Matching dinámico** con cualquier combinación de columnas
- **Tips contextuales** según tipo de cuestionario
- **Solución de problemas de case sensitivity** implementada

#### **📊 Análisis por Cuestionario:**
1. **📊 Estudiante** - Análisis estudiantil
2. **📐 Docente Matemática** - Análisis docente matemático
3. **📝 Docente Comunicación** - Análisis docente comunicación
4. **👨‍👩‍👧‍👦 Familia** - Análisis familiar
5. **🏫 Director F1/F2** - Análisis directivo

#### **ℹ️ Información:**
- **Resumen de archivos** disponibles
- **Estado de datos** cargados
- **Información del sistema**

### **✅ Dos Análisis por Pestaña:**
- **Columna izquierda**: Análisis de Red (correlaciones)
- **Columna derecha**: Explorador de Datos (tablas)

## 🖥️ URL de Acceso:
- **URL**: http://127.0.0.1:7856
- **Acceso local**: http://localhost:7856
- **Acceso de red**: http://0.0.0.0:7856

## 📋 Requisitos del Sistema

### **R Version:**
- R 4.0 o superior
- Descargar de: https://cran.r-project.org/

### **Paquetes R Requeridos:**
- shiny, readxl, readr, dplyr, purrr, stringr
- igraph, RColorBrewer, tibble, scales, DT

### **Instalación Automática:**
El lanzador instala automáticamente los paquetes faltantes.

## 🎨 Cómo Usar

### **1. Lanzar la aplicación:**
```bash
./launch_app.sh
```

### **2. Pestaña Integración (🔗):**
- **Seleccionar cuestionario** en el dropdown
- **Ver columnas disponibles** en ambos datasets
- **Elegir columnas de matching** según tu análisis
- **Ejecutar integración** para ver resultados
- **Revisar vista previa** de datos integrados

### **3. Pestañas de Análisis (ej: Estudiante):**
- **Análisis de Red** (columna izquierda):
  - Configurar umbral de correlación (0-0.8)
  - Elegir nivel (ítem o constructo)
  - Seleccionar método de agregación
  - Construir red para generar visualización
- **Explorador de Datos** (columna derecha):
  - Ver resumen del cuestionario
  - Seleccionar columnas para examinar
  - Explorar datos en tabla interactiva

## 📊 Flujo de Trabajo Típico:

1. **Lanzar** la app con `./launch_app.sh`
2. **Ir a Integración** para hacer matching (opcional)
3. **Seleccionar pestaña** del cuestionario deseado
4. **Configurar análisis** de red (umbral, nivel, método)
5. **Generar red** de correlaciones
6. **Explorar datos** en la tabla interactiva
7. **Comparar** entre diferentes cuestionarios

## 🔧 Solución de Problemas

### **Problemas Comunes:**

1. **"No hay datos de cuestionarios"**
   - Ejecutar: `R -e "source('scripts/validate_data.R')"`
   - Verificar que los archivos Excel estén en `data/xlsx/`

2. **"Datos EM no disponibles"**
   - Asegurarse que `EM_6P_2024_alumnos_innominados.xlsx` esté en `data/xlsx/`

3. **"Paquetes R faltantes"**
   - El lanzador los instala automáticamente

4. **"Puerto 7856 ocupado"**
   - Cambiar puerto en app.R

### **Validación de Datos:**
```bash
./launch_app.sh --validate-only
```

## 📚 Diccionario de Datos

### **Columnas de Matching:**
- **ID_ESTUDIANTE**: Identificador individual del estudiante
- **cod_mod7**: Código de 7 dígitos de la escuela
- **anexo**: Anexo de la escuela
- **ID_seccion**: Identificador de sección de clase

### **Tipos de Análisis:**
- **Nivel ítem**: Análisis pXX_YY (ítems específicos)
- **Nivel constructo**: Análisis pXX (constructos agregados)

### **Problema de Case Sensitivity (SOLUCIONADO):**
- ✅ **Antes**: EM data tenía `ID_estudiante`, cuestionarios tenían `ID_ESTUDIANTE`
- ✅ **Solución**: Estandarización automática de nombres de columnas
- ✅ **Ahora**: Ambos datasets usan `ID_ESTUDIANTE` consistentemente

---

**🎯 ¡Una plataforma ENLA completa y funcional!**

**📧 Contacto**: Para preguntas o problemas, consulta la documentación original del proyecto.
