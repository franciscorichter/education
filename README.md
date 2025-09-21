# Plataforma ENLA de Análisis de Datos Educativos

Aplicación R Shiny para analizar datos ENLA con integración robusta de EM + cuestionarios, redes de correlación, exploración de preguntas vía diccionario y EDA comparativo.

## 📁 Estructura del Proyecto

```
education/
├── data/
│   ├── xlsx/                      # Archivos Excel crudos (cuestionarios + EM)
│   └── enla_processed_data.rds    # Salida del pipeline (input de la app)
├── app/
│   └── app.R                      # App Shiny principal
├── scripts/
│   ├── data_integration.R         # Pipeline: carga, estandarización e integración
│   ├── data_validation.R          # Chequeos de consistencia
│   └── eda.R                      # EDA standalone (genera figuras PNG)
├── outputs/
│   └── eda/
│       └── figures/               # Figuras PNG generadas por scripts/eda.R
├── INSTRUCCIONES_MATCHING.txt
└── README.md
```

## 🚀 Puesta en Marcha

- Requisitos: R >= 4.0 y paquetes: `shiny`, `readxl`, `readr`, `dplyr`, `purrr`, `stringr`, `igraph`, `RColorBrewer`, `tibble`, `scales`, `DT`, `ggplot2`.

### 1) Ejecutar el pipeline de datos
Genera `data/enla_processed_data.rds` con EM completo y cuestionarios integrados.

```bash
R -e "source('scripts/data_integration.R'); run_enla_data_pipeline()"
```

### 2) Lanzar la app

```bash
R -e "shiny::runApp('app', launch.browser = TRUE)"
```

### (Opcional) EDA standalone

```bash
# Todos los gráficos (gender, language, school, area) en violin y scatter
Rscript scripts/eda.R

# Solo school como scatter
Rscript scripts/eda.R --group=school --plot=scatter
```

Las figuras quedan en `outputs/eda/figures/`.

## 🎯 Qué incluye la App

### 📊 EDA
- Subpestaña Plots: 4 comparativos reactivos según tipo de gráfico (Violin o Scatter+Contour)
  - Gender
  - Language
  - School (usa `gestion2`, mapeo: “Estatal” → public; “No estatal” → private)
  - Area (mapeo: rural/urban)
- Subpestaña Summary: tabla con filas/columnas por dataset (EM y cada cuestionario, e ítems pXX).

### 🔗 Network & Questionnaire Analysis
- Pestañas por cuestionario (arriba).
- Subpestaña Network:
  - Parámetros: umbral |r|, nivel (ítem vs pXX), método de agregación (mean/median/z-mean/PCA-1), opción para incluir nodos L/M.
  - Gráfico de red.
  - Tabla “Links to L/M”: lista de conexiones a L y M con r y |r|, ordenada por |r| y filtrada por el umbral.
- Subpestaña Questions:
  - Tabla con Item (pXX), Pregunta (enunciado), Sub-items y Opciones.
  - Se obtiene desde la hoja de diccionario del Excel del cuestionario.

### 🤖 Advanced Modeling (placeholder)
- Reservado para modelos avanzados y análisis predictivo.

## 🔧 Detalles Técnicos clave

- `scripts/data_integration.R`:
  - Carga EM (hoja BD), estandariza nombres de columnas, no descarta columnas (para EDA), hace `make.unique` si hay duplicados (p.ej. `ID_seccion`).
  - Carga cuestionarios, estandariza y genera `full_data`, `data` (numérico) e índices de ítems pXX/pXX_YY.
  - Agrega EM por estudiante/sección/sede con `aggregate_em_data()`.
  - Persistencia: `data/enla_processed_data.rds`.

- `app/app.R`:
  - Lee el RDS, arma pestañas por cuestionario (oculta `base_web2`).
  - Network: construye correlaciones, arma `igraph`, agrega L/M desde `full_data` si se marca la opción.
  - LM links table: usa la matriz de correlaciones filtrada por umbral para listar enlaces a L/M.
  - Questions: lectura flexible de diccionarios (segunda hoja o “Diccionario”).

- `scripts/eda.R`:
  - Detecta columnas de agrupación: `sexo` (gender), `lengua_materna` (language), `gestion2` (school), `area` (area) con equivalentes.
  - Mapea valores:
    - Gender → boy/girl
    - Language → spanish/other
    - School (`gestion2`) → public/private (incluye “no estatal” → private)
    - Area → rural/urban
  - Genera violin y scatter+contour para M vs L.

## 🧪 Validación y Problemas Comunes

- “Sin datos suficientes” en EDA:
  - La columna de agrupación puede faltar o tener valores no mapeados. Revise valores únicos y amplíe tokens.

- L/M no aparecen en la red:
  - Active “Include L/M nodes”. Si sigue en blanco, verifique que `medida500_L`/`medida500_M` existan en `full_data` del cuestionario.

- Diccionario no encontrado:
  - La app busca `data/xlsx/<nombre_cuestionario>.xlsx`. Si está en otra ruta, ajuste la ubicación o comparta la ruta para incorporarla.

## 📦 Datos Esperados

- EM: `data/xlsx/EM_6P_2024_alumnos_innominados.xlsx` (hoja BD) u homólogo.
- Cuestionarios: Excel por cuestionario bajo `data/xlsx/` con una hoja de diccionario (hoja 2 o “Diccionario”).

---

Si necesitas ajustar mapeos (p.ej., valores exactos en `gestion2` o `area`), compárteme ejemplos y los agrego. También puedo añadir descargas CSV para la tabla de enlaces L/M y anotaciones del diccionario en el listado.
